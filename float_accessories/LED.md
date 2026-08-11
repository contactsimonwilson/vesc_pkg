# LED behaviour

Reference for what the Float Accessories LED module does and why. It describes
the code as it stands in `lib/led.lisp`, `lib/led-vars.lisp` and the
`esp_led_strip` native library under `../lib_esp_led_strip`.

For the engine's extension API (`ext-esp_led-*`) see
[`../lib_esp_led_strip/README.md`](../lib_esp_led_strip/README.md). Effect,
palette, colour-order and timing ids are generated from the C source into
`../lib_esp_led_strip/esp_led_defs.lisp` — that file is the authority, never
retype the numbers.

---

## 1. How the two halves fit together

| | Owns |
|---|---|
| `esp_led_strip` native lib (C) | Framebuffers, the render thread, effect animation, brightness easing, the wire protocol |
| `lib/led.lisp` | The state machine: which strip shows which effect, at what colour and brightness, right now |

The lisp side never touches pixels. It decides an *appearance* per segment
(effect, palette, colour, speed, brightness) and pushes it with one
`ext-esp_led-seg-look` call. The lib animates that appearance in its own
FreeRTOS thread, so effects keep running smoothly even when the lisp evaluator
is busy with CAN, BMS or GNSS work.

Because the loop restates the same appearance on every tick, `led.lisp` keeps an
appearance cache (`seg-cache`) and only calls into the lib when something
actually changed. A steady state costs zero extension calls.

**Render settings the package uses:** 60 fps requested (`ext-esp_led-fps 60`,
which the lib rounds to a 16 ms frame), master brightness left at 255, and the
lib's default fade of 15/32 — about 47% of the remaining gap closed per 33 ms,
so a brightness change settles in roughly 0.2 s.

---

## 2. Strips, segments and chaining

Five logical strips, each an independent segment: **status**, **front**,
**rear**, **footpad**, **button**. The lib supports 8 segments total.

### Enabling a strip

Each strip's **Timing** setting doubles as its enable. `Disabled` means the strip
is never defined; any other value selects the wire timing preset. A strip is only
created when its timing is set, its pin is >= 0, and its LED count is > 0.

A strip that never gets defined is silently invisible from the outside, so every
skipped strip is logged under `DBG-LED`. If you turn a strip on and nothing
happens, that log line is the first place to look.

### Chaining several strips on one pin

Strips sharing a pin form one physical chain. Offsets are assigned automatically
in definition order:

```
status → front → rear → footpad → button
```

The first strip on a pin starts at offset 0, the next starts where it ended, and
so on. Embedded highbeam pixels count toward the strip's footprint, so a
17-pixel front strip with 4 embedded highbeams advances the next offset by 21.

Rules the lib enforces at init, all of which fail the **whole chain** — if init
fails, every strip on the board stays dark:

- every segment on a pin must have the same **pixel width** (3 bytes for
  GRB/RGB, 4 for GRBW/RGBW/WRGB). Byte *order* may differ; width may not.
- every segment on a pin must use the same **timing preset**. The chain takes
  the timing of the first strip defined on that pin.
- segment footprints on a pin must not overlap.

An init failure is reported as `ERR led init ...` and the loop then idles
without touching the lib.

### The button LED

One pixel, and the only strip with no colour-order setting of its own. Chained
behind another strip it adopts that chain's colour order — a hardcoded 3-byte
order behind a 4-byte strip is exactly the width disagreement above, and it
would take every strip down. Alone on its pin it defaults to GRB.

### Reversal

Status, front, rear and footpad each have a **Reversed** flag that flips the
pixel order of the effect. Embedded highbeam pixels are *not* reversed — their
positions are physical hardware, so they stay put while the effect flows around
them.

---

## 3. Highbeams

Configured per strip (front and rear independently):

| Mode | Hardware |
|---|---|
| 0 — None | no highbeam |
| 1 — PWM Pin | a separate driver on its own GPIO |
| 2 — Embedded LEDs | brighter LEDs built into the strip itself |

**PWM mode** starts a 1 kHz, 10-bit PWM channel — channel 0 for the front,
channel 1 for the rear. Duty is only rewritten when it changes.

**Embedded mode** uses the lib's *overlay pixels*: physical LEDs at fixed
positions inside the strip that hold their own colour and brightness while the
effect animates around them. They are driven white, and their brightness is
mapped onto the configured **Min Drive**/**Max Drive** range, because some light
bars need a minimum duty before they light at all.

Positions are packed one per byte into a single integer, lowest byte first, with
`255` meaning unused. Positions `3, 8, 14, 19` encode as `319686659`
(`0x130E0803`). In practice the QML page packs this for you from the strip
preset — the raw number only matters if you edit the parameter directly.

Positions are validated against the strip before they reach the lib: anything at
or past the segment footprint (`LED count + highbeam count`) is dropped, as are
duplicates, and a `WARN led hb pos out of range` line says what survived. This
matters because the lib *rejects* such a definition, and an unhandled rejection
used to take the LED loop down permanently. It is reachable by ordinary means —
switching a strip to the `Custom` preset keeps the previous preset's highbeam
positions while letting you lower the LED count underneath them.

### When a highbeam lights

All of these must hold:

- master **LEDs On** is on
- **Highbeam On** is on
- the board is running (state 1–5) and **not** in flywheel mode (state 5)
- the strip faces the direction of travel — front when moving forward, rear when
  reversing — and that strip's highbeam mode is not None

The strip whose highbeam is lit dims the rest of its own pixels by
**Dim On Highbeam** (default 0.2; 0 blanks them entirely). The opposite strip is
unaffected.

---

## 4. Direction

Direction picks which strip is "head" (facing travel) and which is "tail", and
which highbeam lights.

- `rpm > 100` → forward, `rpm < -100` → reverse. Between the two, direction is
  held.
- In **upside-down / darkride** (state 4) the thresholds are inverted, so the
  lights stay the right way round.
- Direction is ignored entirely during **wheelslip** (state 3).
- A change must persist for **0.5 s** before it commits. If rpm settles back
  before the window elapses the pending change is dropped, so a rock or a bit of
  rollback does not flip the lights — and does not leave the debounce disarmed
  for the next real reversal.
- Direction resets to forward whenever the board is not riding, not mall-grabbed
  and not showing a charge.

---

## 5. Ride modes (front and rear)

**LED Mode**, **LED Mode Idle** and **LED Mode Startup** all choose from the same
list. `head` is the strip facing travel.

| # | Name | head | tail |
|---|---|---|---|
| 0 | White/Red | solid white | solid red |
| 1 | Battery | battery gauge | battery gauge |
| 2 | Cyan/Magenta | solid cyan | solid magenta |
| 3 | Blue/Green | solid blue | solid green |
| 4 | Yellow/Green | solid yellow | solid green |
| 5 | Rainbow | rainbow, RGBW palette | rainbow, RGBW palette |
| 6 | Strobe | white strobe | white strobe |
| 7 | Rave | fast rainbow, neon palette | fast rainbow, neon palette |
| 8 | Rave Directional | solid white | fast rainbow, neon palette |
| 9 | Knight Rider | red larson scanner | red larson scanner |
| 10 | Felony | alternating red/blue halves | alternating red/blue halves |
| 11 | Trans Pride | slow rainbow sweep | slow rainbow sweep |

Anything unrecognised falls back to White/Red.

### Which mode is in force

Selected in this order, later steps overriding earlier ones:

1. **LED Mode** — the baseline.
2. **Idle** — no activity for `Idle Timeout` (default 1 s) *and* CAN telemetry
   fresh within 1 s → **LED Mode Idle** at idle brightness.
3. **Startup** — within `Startup Pattern Time` of boot (default 20 s) and not
   riding → **LED Mode Startup**.

"Activity" means riding, mall grab, or a charge display.

### What actually gets drawn

The chosen mode is then filtered through this priority chain — the first match
wins:

1. **Board disabled** (state 15) → both strips solid red.
2. **Handtest mode** → both strips blue breathe.
3. **Idle shutoff** — no activity for `Idle Shutoff` (default 600 s), CAN fresh,
   not flywheel → front, rear, footpad and button all off. The status bar stays
   on.
4. **Battery mode or mall grab**, with CAN fresh → battery gauge on both.
5. **No telemetry** (CAN silent > 1 s, past the startup window) **or frozen** →
   plain White/Red, so a board that has lost its float package still has usable
   lights.
6. Otherwise → the selected mode.

Two overrides run after that chain:

- **Brake light** — enabled, riding, not flywheel, motor current at or below
  **Brake Light Min Current** (default −4 A), and *Freeze While Riding* off →
  the tail strobes red.
- **Charging display** — battery gauge on both strips, pulsing.

**Freeze While Riding** (`led_update_not_running`), when on, drops the front and
rear to plain White/Red one second into a ride and disables the brake light.

---

## 6. Status bar

Driven every tick regardless of the master LEDs On switch — it is diagnostic, so
turning the lights off does not blind you. First match wins:

| Condition | Display |
|---|---|
| Handtest mode | blue bar: full both pads, half one pad, dim none |
| Board disabled (state 15) | solid red |
| CAN silent ≥ 1 s, or no CAN id yet | blue breathe ("connecting") |
| `rpm > 250` and active pushback/tiltback (`sat > 2`) | red strobe |
| `rpm > 250` otherwise | duty-cycle bar: green, yellow above 60%, red above 80% |
| A footpad engaged | split half/half — see below |
| Otherwise | battery gauge, pulsing while charging |

### Footpad indication

The bar splits at its midpoint. One half lights cyan for each engaged pad; both
halves light when both pads are down. **Status Bar Style** swaps which half maps
to which pad — `Classic` and `Alternate` exist because the bar can be mounted
either way round.

---

## 7. Footpad strip and button LED

- **Footpad strip** — rainbow. **Footpad LED Mode** currently offers only that.
- **Button LED** — **Button LED Mode** selects `Rainbow` or `Battery`.

Both go dark when the master LEDs On switch is off, and at idle shutoff.

---

## 8. Mall grab

Active when the board is **not riding** and pitched **above 70°** — i.e. picked
up by the footpad.

While held, and if **Mall Grab** is enabled, the front and rear show the battery
gauge at status brightness. Mall grab also counts as activity, so the idle
timers do not run while the board is in your hand.

Pressing the footpad (both sensors) while mall-grabbed acts as a button:

| Press | Action |
|---|---|
| Short — under 1 s | toggle **LEDs On** |
| Long — 1 s or more | toggle **Highbeam On** |

Both toggles are written back to the stored config, not just to the running
cache. The write is debounced (~0.75 s) so a burst of presses costs one flash
write. Without persisting, the next config write from VESC Tool would reload the
old value and the lights would come back on by themselves.

---

## 9. Brightness

Four settings, all fractions from 0 to 1, and all capped by **Max Brightness**:

| Setting | Applies to |
|---|---|
| Brightness | front/rear while riding |
| Brightness Idle | front/rear once idle, and in flywheel mode |
| Brightness Status | the status bar, and front/rear during mall grab |
| Brightness Highbeam | highbeam drive level |

Transitions are eased by the lib, not stepped in lisp, so brightness changes
glide rather than jump. Segments start at brightness 0 and fade up, so bringing
strips up at boot does not flash them at full brightness first.

---

## 10. Timing, failure handling and diagnostics

**Loop rate** — **LED Loop Rate** (default 20 Hz, range 1–100). This paces the
state machine only; effect animation runs in the lib at its own frame rate, so
lowering it saves CPU without slowing animations. A rate below 1 falls back to
50 Hz. Missed deadlines are counted and reported under `DBG-LED`.

**Settings changes** — writing the config from VESC Tool sets a reinit flag; the
loop tears the strips down and rebuilds them in place on its next tick, without
restarting the thread. Disabling the LED module stops the loop and blanks the
strips.

**Failure behaviour**

| Situation | What happens |
|---|---|
| Lighting enabled, nothing configured | `WARN led no strip configured`, loop runs and does nothing |
| Highbeam positions out of range or duplicated | offenders dropped, `WARN led hb pos out of range` |
| A segment definition the lib rejects | `ERR led setup ...`, loop idles — it does not retry in a crash loop |
| `ext-esp_led-init` fails (chain mismatch) | `ERR led init ...`, loop idles |
| The loop thread crashes | the restart monitor respawns it after 1 s; strips already running are not reinitialised, so there is no blink |

A settings change is what clears a failed setup and gets a genuine retry — fix
the config in VESC Tool and the strips come back without a reboot.

**Debugging** — the LED debug category (`DBG-LED`) has the value 8. Turn it on
from the lisp console with `(dbg-add 8)`; `(dbg-del 8)` turns it off. It logs segment
definitions with their pins, types, counts, offsets and timings; mode,
direction, on/off and highbeam changes as they happen; a periodic brightness and
state line; and loop overruns.
