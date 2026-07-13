# ESPLED Strip

Segmented addressable-LED strip engine for the VESC Express as a native library, with a QML test page. LispBM sets high-level segment / effect state, and a background render thread animates, applies brightness, RGBW auto-white and an adaptive current limit, and pushes pixels through the firmware rgbled driver.

Works on all Express targets (ESP32-C3, C6, S3, P4) - the package contains one binary per chip and picks the right one at runtime with `(sysinfo 'hw-target)`.

## Test UI

The package includes a VESC Tool page for testing: configure pin / LED count / strip type and press **Start**, then play with effects, palettes, color and brightness. The controls send LispBM expressions to the device as custom app data, which `espled_strip.lisp` evaluates.

## Extensions

| Extension | Args | Notes |
|---|---|---|
| `ext-espled-seg-def` | `(i pin type len [offset])` | define segment `i` (type: 0 GRB, 1 RGB, 2 GRBW, 3 RGBW). Segments on the same pin form one chained strip; `offset` is the segment's pixel position in the chain |
| `ext-espled-init` | `(n)` | start rendering the first `n` segments |
| `ext-espled-deinit` | `()` | stop rendering and release the LED driver |
| `ext-espled-seg-look` | `(i fx pal color spd bri)` | full appearance in one call |
| `ext-espled-seg-fx` / `ext-espled-fx` | `(i fx)` / `(fx)` | effect per segment / all segments |
| `ext-espled-seg-pal` / `ext-espled-pal` | `(i pal)` / `(pal)` | palette 0..7 |
| `ext-espled-seg-col` / `ext-espled-col` | `(i color)` / `(color)` | packed `0xWWRRGGBB` |
| `ext-espled-col-rgb` / `ext-espled-col-rgbw` | `(r g b [w])` | solid color on all segments |
| `ext-espled-seg-bri` | `(i bri)` | per-segment brightness 0..255 |
| `ext-espled-seg-spd` | `(i spd)` | animation speed 0..255 |
| `ext-espled-seg-size` | `(i size)` | chase head / comet tail length |
| `ext-espled-seg-level` | `(i level)` | gauge fill level 0..255 |
| `ext-espled-seg-overlay-def` | `(i idx...)` | define up to 8 fixed overlay pixel positions (e.g. embedded highbeam LEDs) before init; effect pixels flow around them. No indices clears the overlay |
| `ext-espled-seg-overlay` | `(i color bri)` | overlay color and brightness at runtime (bri 0 = off) |
| `ext-espled-seg-on` | `(i on)` | enable/disable a segment |
| `ext-espled-seg-reverse` | `(i rev)` | reverse pixel order |
| `ext-espled-bri` | `(b)` | master brightness 0..255 |
| `ext-espled-auto-white` | `(en)` | derive W from RGB on RGBW strips |
| `ext-espled-ablimit` | `(ma)` | adaptive current cap in mA (0 = off) |

Effects: 0 solid, 1 breathe, 2 chase, 3 rainbow, 4 sparkle, 5 comet, 6 gauge (fill by `level`, battery gradient when color is 0, pulses when `spd` > 0), 7 strobe, 8 larson/knight-rider, 9 felony (halves alternate red/blue).
Palettes: 0 rgbw-ish, 1 fire, 2 ocean, 3 neon, 4 ember, 5 traffic, 6 strobe, 7 police-blue.

## Example

```clj
(import "pkg::espled@://vesc_packages/lib_espled_strip/espled_strip.vescpkg" 'espled)
(load-native-lib espled)

(ext-espled-seg-def 0 20 0 30) ; seg 0: pin 20, GRB, 30 px
(ext-espled-init 1)
(ext-espled-bri 128)
(ext-espled-seg-fx 0 3)        ; rainbow
```

Segments can sit on different pins; they are transmitted sequentially through the firmware's single LED driver each frame. Timing uses the firmware's universal preset, which covers WS2812B / WS2815 / SK6812 / SK6815.

## Building

```sh
make
```

Needs the `riscv32-esp-elf` and `xtensa-esp32s3-elf` toolchains, the `c_libs/RVfplib` submodule and `vesc_tool`.

## Requirements

Firmware with native lib support including `(sysinfo 'hw-target)` and the `rgbled_*` C interface. On the ESP32-S3 the firmware must be built with `CONFIG_ESP_SYSTEM_MEMPROT_FEATURE=n`.
