# FLED Strip Control

Segmented addressable-LED effect engine for the VESC Express as a native library, with a QML test page. Modeled on the in-firmware `fled` module: LispBM sets high-level segment / effect state, and a background render thread animates, applies brightness, RGBW auto-white and an adaptive current limit, and pushes pixels through the firmware rgbled driver.

Works on all Express targets (ESP32-C3, C6, S3, P4) - the package contains one binary per chip and picks the right one at runtime with `(sysinfo 'hw-target)`.

## Test UI

The package includes a VESC Tool page for testing: configure pin / LED count / strip type and press **Start**, then play with effects, palettes, color and brightness. The controls send LispBM expressions to the device as custom app data, which `fled.lisp` evaluates.

## Extensions

| Extension | Args | Notes |
|---|---|---|
| `ext-fled-seg-def` | `(i pin type len)` | define segment `i` (type: 0 GRB, 1 RGB, 2 GRBW, 3 RGBW) |
| `ext-fled-init` | `(n)` | start rendering the first `n` segments |
| `ext-fled-deinit` | `()` | stop rendering and release the LED driver |
| `ext-fled-seg-look` | `(i fx pal color spd bri)` | full appearance in one call |
| `ext-fled-seg-fx` / `ext-fled-fx` | `(i fx)` / `(fx)` | effect per segment / all segments |
| `ext-fled-seg-pal` / `ext-fled-pal` | `(i pal)` / `(pal)` | palette 0..7 |
| `ext-fled-seg-col` / `ext-fled-col` | `(i color)` / `(color)` | packed `0xWWRRGGBB` |
| `ext-fled-col-rgb` / `ext-fled-col-rgbw` | `(r g b [w])` | solid color on all segments |
| `ext-fled-seg-bri` | `(i bri)` | per-segment brightness 0..255 |
| `ext-fled-seg-spd` | `(i spd)` | animation speed 0..255 |
| `ext-fled-seg-size` | `(i size)` | chase head / comet tail length |
| `ext-fled-seg-on` | `(i on)` | enable/disable a segment |
| `ext-fled-seg-reverse` | `(i rev)` | reverse pixel order |
| `ext-fled-bri` | `(b)` | master brightness 0..255 |
| `ext-fled-auto-white` | `(en)` | derive W from RGB on RGBW strips |
| `ext-fled-ablimit` | `(ma)` | adaptive current cap in mA (0 = off) |

Effects: 0 solid, 1 breathe, 2 chase, 3 rainbow, 4 sparkle, 5 comet.
Palettes: 0 rgbw-ish, 1 fire, 2 ocean, 3 neon, 4 ember, 5 traffic, 6 strobe, 7 police-blue.

## Example

```clj
(import "pkg::fled@://vesc_packages/lib_fled/fled.vescpkg" 'fled)
(load-native-lib fled)

(ext-fled-seg-def 0 20 0 30) ; seg 0: pin 20, GRB, 30 px
(ext-fled-init 1)
(ext-fled-bri 128)
(ext-fled-seg-fx 0 3)        ; rainbow
```

Segments can sit on different pins; they are transmitted sequentially through the firmware's single LED driver each frame. Timing uses the firmware's universal preset, which covers WS2812B / WS2815 / SK6812 / SK6815.

## Building

```sh
make
```

Needs the `riscv32-esp-elf` and `xtensa-esp32s3-elf` toolchains, the `c_libs/RVfplib` submodule and `vesc_tool`.

## Requirements

Firmware with native lib support including `(sysinfo 'hw-target)` and the `rgbled_*` C interface. On the ESP32-S3 the firmware must be built with `CONFIG_ESP_SYSTEM_MEMPROT_FEATURE=n`.
