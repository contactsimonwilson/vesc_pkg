; Appearance cache for the segments: the last effect / palette / colour /
; speed / brightness pushed to each one, so a steady state costs no
; extension calls at all. The lib animates in its own thread and the loop
; below only re-states the same appearance every tick, so almost every tick
; is a steady state and the cache hits.
;
; Flat byte buffers rather than a tuple per segment: the compare runs on
; every segment on every tick, and building a tuple to compare against would
; allocate at the loop rate and pull the GC in with it.
;
; Above @const-start deliberately, like the buffers in bms-vars.lisp: these
; are written in place with bufset, and buffers that live in the constant
; heap cannot be written.
;
; Stride 8 per segment: fx, pal, spd, bri, fx-val (3 spare).
(def seg-cache (bufcreate 64))
; Colours, u32 per segment, in their own buffer for the 4-byte slots.
(def seg-cache-col (bufcreate 32))
; Overlay (embedded highbeam) brightness per segment. u16 so the post-reset
; 0xFFFF cannot collide with a real 0..255 brightness - as a byte it would,
; and a highbeam that came up at full brightness would then be skipped.
(def seg-cache-ov (bufcreate 16))

@const-start

; LED control on top of the esp_led_strip native lib. The lib owns the
; framebuffers, the render thread and the LED driver; this loop only runs
; the state machine (direction, idle, mall grab, brake light, charging)
; and sets per-segment effect/color/brightness through the ext-esp_led-*
; extensions. Strips on the same pin are chained with segment offsets.
;
; Each strip's timing config doubles as its enable: 0 = disabled, 1+ =
; wire timing preset (esp_led preset value + 1).
;
; Highbeams are described concretely in the config (the named board
; presets exist only in the QML page): mode 1 drives a separate PWM pin,
; mode 2 drives LEDs embedded in the strip as esp_led overlay pixels, at
; positions packed one per byte in the highbeam-pos int (255 = unused),
; with the brightness mapped onto the configured min-max drive range. The
; strip facing the direction of travel lights its highbeam and dims by
; the configured ratio.

(defun load-led-settings () {
    (setq led-enabled (get-config 'led-enabled))
    (setq led-on (get-config 'led-on))
    (setq led-highbeam-on (get-config 'led-highbeam-on))
    (setq led-mode (get-config 'led-mode))
    (setq led-mode-idle (get-config 'led-mode-idle))
    (setq led-mode-status (get-config 'led-mode-status))
    (setq led-mode-startup (get-config 'led-mode-startup))
    (setq led-mode-button (get-config 'led-mode-button))
    (setq led-mode-footpad (get-config 'led-mode-footpad))
    (setq led-mall-grab-enabled (get-config 'led-mall-grab-enabled))
    (setq led-brake-light-enabled (get-config 'led-brake-light-enabled))
    (setq led-brake-light-min-amps (get-config 'led-brake-light-min-amps))
    (setq idle-timeout (get-config 'idle-timeout))
    (setq idle-timeout-shutoff (get-config 'idle-timeout-shutoff))
    (setq led-brightness (get-config 'led-brightness))
    (setq led-brightness-highbeam (get-config 'led-brightness-highbeam))
    (setq led-brightness-idle (get-config 'led-brightness-idle))
    (setq led-brightness-status (get-config 'led-brightness-status))
    (setq led-status-pin (get-config 'led-status-pin))
    (setq led-status-num (get-config 'led-status-num))
    (setq led-status-type (get-config 'led-status-type))
    (setq led-status-reversed (get-config 'led-status-reversed))
    (setq led-status-timing (get-config 'led-status-timing))
    (setq led-front-pin (get-config 'led-front-pin))
    (setq led-front-num (get-config 'led-front-num))
    (setq led-front-type (get-config 'led-front-type))
    (setq led-front-reversed (get-config 'led-front-reversed))
    (setq led-front-timing (get-config 'led-front-timing))
    (setq led-front-highbeam-mode (get-config 'led-front-highbeam-mode))
    (setq led-front-highbeam-pos (get-config 'led-front-highbeam-pos))
    (setq led-front-highbeam-min (get-config 'led-front-highbeam-min))
    (setq led-front-highbeam-max (get-config 'led-front-highbeam-max))
    (setq led-rear-pin (get-config 'led-rear-pin))
    (setq led-rear-num (get-config 'led-rear-num))
    (setq led-rear-type (get-config 'led-rear-type))
    (setq led-rear-reversed (get-config 'led-rear-reversed))
    (setq led-rear-timing (get-config 'led-rear-timing))
    (setq led-rear-highbeam-mode (get-config 'led-rear-highbeam-mode))
    (setq led-rear-highbeam-pos (get-config 'led-rear-highbeam-pos))
    (setq led-rear-highbeam-min (get-config 'led-rear-highbeam-min))
    (setq led-rear-highbeam-max (get-config 'led-rear-highbeam-max))
    (setq led-button-pin (get-config 'led-button-pin))
    (setq led-button-timing (get-config 'led-button-timing))
    (setq led-footpad-pin (get-config 'led-footpad-pin))
    (setq led-footpad-num (get-config 'led-footpad-num))
    (setq led-footpad-type (get-config 'led-footpad-type))
    (setq led-footpad-reversed (get-config 'led-footpad-reversed))
    (setq led-footpad-timing (get-config 'led-footpad-timing))
    (setq led-startup-timeout (get-config 'led-startup-timeout))
    (setq led-dim-on-highbeam-ratio (get-config 'led-dim-on-highbeam-ratio))
    (setq led-loop-delay (get-config 'led-loop-delay))
    (setq led-show-battery-charging (get-config 'led-show-battery-charging))
    (setq led-front-highbeam-pin (get-config 'led-front-highbeam-pin))
    (setq led-rear-highbeam-pin (get-config 'led-rear-highbeam-pin))
    (setq led-max-brightness (get-config 'led-max-brightness))
    (setq led-update-not-running (get-config 'led-update-not-running))
})

; Define the esp_led segments from the config. Strips sharing a pin become
; one chain: each next strip on the pin gets the accumulated pixel offset.
; Chain order matches the original wiring convention: status, front, rear,
; then footpad and button.
(defun led-setup-segments () {
    (ext-esp_led-deinit)
    ; deinit/init resets the lib's segment state, so nothing the appearance
    ; cache remembers is true of the new segments.
    (seg-cache-reset)
    (setq seg-front -1)
    (setq seg-rear -1)
    (setq seg-status -1)
    (setq seg-footpad -1)
    (setq seg-button -1)

    (var idx 0)
    (var pin-offsets nil) ; assoc pin -> next chain offset
    (var pin-timings nil) ; assoc pin -> chain timing preset

    (var next-offset (fn (pin len) {
        (var entry (assoc pin-offsets pin))
        (var off (if (eq entry nil) 0 entry))
        (setq pin-offsets (acons pin (+ off len) pin-offsets))
        off
    }))

    ; Segments chained on one pin share one data line, so the whole chain
    ; uses the timing of the first strip defined on that pin. Config
    ; timing values are esp_led preset + 1 (0 = strip disabled).
    (var chain-timing (fn (pin timing) {
        (var entry (assoc pin-timings pin))
        (if (eq entry nil) {
            (setq pin-timings (acons pin (- timing 1) pin-timings))
            (- timing 1)
        } entry)
    }))

    ; Logged unconditionally at DBG-LED: "my strip doesn't light up" is
    ; almost always a segment that was never defined because timing was 0,
    ; the pin was -1 or the LED count was 0 - and that is invisible from
    ; the outside.
    (var log-seg (fn (i name pin type num off timing) {
        (dbg DBG-LED (str-merge "led seg " (str-from-n i "%d") " " name
            " pin " (str-from-n pin "%d")
            " type " (str-from-n type "%d")
            " n " (str-from-n num "%d")
            " off " (str-from-n off "%d")
            " tim " (str-from-n timing "%d")))
    }))
    (var log-skip (fn (name pin num timing) {
        (if (dbg-active DBG-LED)
            (dbg DBG-LED (str-merge "led seg - " name
                " pin " (str-from-n pin "%d")
                " n " (str-from-n num "%d")
                " tim " (str-from-n timing "%d"))))
    }))

    (if (and (> led-status-timing 0) (>= led-status-pin 0) (> led-status-num 0)) {
        (var off (next-offset led-status-pin led-status-num))
        (var tim (chain-timing led-status-pin led-status-timing))
        (if (dbg-active DBG-LED) (log-seg idx "status" led-status-pin led-status-type led-status-num off tim))
        (ext-esp_led-seg-def idx led-status-pin led-status-type led-status-num off tim)
        (setq seg-status idx)
        (setq idx (+ idx 1))
    } {
        (log-skip "status" led-status-pin led-status-num led-status-timing)
    })
    ; Embedded highbeam LED positions (segment-relative), unpacked from
    ; the config int: one position per byte from the lowest, 255 = unused.
    (var hb-positions (fn (packed) {
        (var lst nil)
        (looprange k 0 4 {
            (var p (bitwise-and (shr packed (* k 8)) 0xFF))
            (if (!= p 0xFF) (setq lst (append lst (list p))))
        })
        lst
    }))
    (var overlay-def (fn (seg ps) {
        (cond
            ((= (length ps) 1) (ext-esp_led-seg-overlay-def seg (ix ps 0)))
            ((= (length ps) 2) (ext-esp_led-seg-overlay-def seg (ix ps 0) (ix ps 1)))
            ((= (length ps) 3) (ext-esp_led-seg-overlay-def seg (ix ps 0) (ix ps 1) (ix ps 2)))
            ((= (length ps) 4) (ext-esp_led-seg-overlay-def seg (ix ps 0) (ix ps 1) (ix ps 2) (ix ps 3)))
        )
    }))

    (if (and (> led-front-timing 0) (>= led-front-pin 0) (> led-front-num 0)) {
        (var ps (if (= led-front-highbeam-mode 2) (hb-positions led-front-highbeam-pos) nil))
        (var off (next-offset led-front-pin (+ led-front-num (length ps))))
        (var tim (chain-timing led-front-pin led-front-timing))
        (if (dbg-active DBG-LED) {
            (log-seg idx "front" led-front-pin led-front-type led-front-num off tim)
            (dbg DBG-LED (str-merge "led hb front mode " (str-from-n led-front-highbeam-mode "%d")
                " pos " (to-str ps)
                " pin " (str-from-n led-front-highbeam-pin "%d")))
        })
        (ext-esp_led-seg-def idx led-front-pin led-front-type led-front-num off tim)
        (overlay-def idx ps)
        (setq seg-front idx)
        (setq idx (+ idx 1))
    } {
        (log-skip "front" led-front-pin led-front-num led-front-timing)
    })
    (if (and (> led-rear-timing 0) (>= led-rear-pin 0) (> led-rear-num 0)) {
        (var ps (if (= led-rear-highbeam-mode 2) (hb-positions led-rear-highbeam-pos) nil))
        (var off (next-offset led-rear-pin (+ led-rear-num (length ps))))
        (var tim (chain-timing led-rear-pin led-rear-timing))
        (if (dbg-active DBG-LED) {
            (log-seg idx "rear" led-rear-pin led-rear-type led-rear-num off tim)
            (dbg DBG-LED (str-merge "led hb rear mode " (str-from-n led-rear-highbeam-mode "%d")
                " pos " (to-str ps)
                " pin " (str-from-n led-rear-highbeam-pin "%d")))
        })
        (ext-esp_led-seg-def idx led-rear-pin led-rear-type led-rear-num off tim)
        (overlay-def idx ps)
        (setq seg-rear idx)
        (setq idx (+ idx 1))
    } {
        (log-skip "rear" led-rear-pin led-rear-num led-rear-timing)
    })
    (if (and (> led-footpad-timing 0) (>= led-footpad-pin 0) (> led-footpad-num 0)) {
        (var off (next-offset led-footpad-pin led-footpad-num))
        (var tim (chain-timing led-footpad-pin led-footpad-timing))
        (if (dbg-active DBG-LED) (log-seg idx "footpad" led-footpad-pin led-footpad-type led-footpad-num off tim))
        (ext-esp_led-seg-def idx led-footpad-pin led-footpad-type led-footpad-num off tim)
        (setq seg-footpad idx)
        (setq idx (+ idx 1))
    } {
        (log-skip "footpad" led-footpad-pin led-footpad-num led-footpad-timing)
    })
    (if (and (> led-button-timing 0) (>= led-button-pin 0)) {
        (var off (next-offset led-button-pin 1))
        (var tim (chain-timing led-button-pin led-button-timing))
        (if (dbg-active DBG-LED) (log-seg idx "button" led-button-pin 0 1 off tim))
        (ext-esp_led-seg-def idx led-button-pin 0 1 off tim)
        (setq seg-button idx)
        (setq idx (+ idx 1))
    } {
        (log-skip "button" led-button-pin 1 led-button-timing)
    })

    (if (> idx 0) {
        (var r (trap (ext-esp_led-init idx)))
        (if (eq (ix r 0) 'exit-error)
            (dbg-err (str-merge "led init " (to-str (ix r 1))))
            (dbg DBG-LED (str-merge "led init " (str-from-n idx "%d") " segs")))
        (ext-esp_led-fps 60)
        (if (>= seg-status 0) (ext-esp_led-seg-reverse seg-status led-status-reversed))
        (if (>= seg-front 0) (ext-esp_led-seg-reverse seg-front led-front-reversed))
        (if (>= seg-rear 0) (ext-esp_led-seg-reverse seg-rear led-rear-reversed))
        (if (>= seg-footpad 0) (ext-esp_led-seg-reverse seg-footpad led-footpad-reversed))
    } {
        ; Lighting is enabled but nothing was configured - the loop will run
        ; and do nothing at all, which looks identical to a crash.
        (dbg-warn "led no strip configured")
    })

    ; PWM highbeams (highbeam mode 1)
    (if (and (= led-front-highbeam-mode 1) (>= led-front-highbeam-pin 0)) {
        (pwm-start 1000 0.0 0 led-front-highbeam-pin 10)
    })
    (if (and (= led-rear-highbeam-mode 1) (>= led-rear-highbeam-pin 0)) {
        (pwm-start 1000 0.0 1 led-rear-highbeam-pin 10)
    })

    (> idx 0)
})

; Strip bring-up state. Globals rather than led-loop locals because setup()
; brings the strips up before the loop is spawned, and a loop respawned by
; its restart monitor must not deinit/reinit a strip that is already
; rendering - that shows up as a blink after every crash.
(def led-setup-done nil)
(def led-have-segs nil)

; Read the config into the lisp cache and define + start the segments.
; Called from setup() so the strips light as early in the boot as they can,
; and by led-loop for the cases setup() did not cover (lighting enabled at
; runtime, a reinit after a settings change, a respawn after a crash).
; Idempotent: a second caller gets the first one's result rather than a
; deinit/reinit cycle.
(defun led-start () {
    (if (not led-setup-done) {
        (load-led-settings)
        (setq led-have-segs (led-setup-segments))
        ; Set last, so a throw above leaves the flag clear and the next
        ; caller (the LED loop) retries instead of running with no segments.
        (setq led-setup-done t)
    })
    led-have-segs
})

(defun led-teardown () {
    (setq led-setup-done nil)
    (ext-esp_led-deinit)
    (if (and (= led-front-highbeam-mode 1) (>= led-front-highbeam-pin 0)) (pwm-stop 0))
    (if (and (= led-rear-highbeam-mode 1) (>= led-rear-highbeam-pin 0)) (pwm-stop 1))
})

(defun bri255 (b) (to-i (* 255.0 (min (max b 0.0) 1.0))))

; Last PWM highbeam duty per channel, -1 = unknown.
(def hb-duty-front -1.0)
(def hb-duty-rear -1.0)

; Invalidate every entry. fx 0xFF is not a real effect id, so the first
; compare after this always misses and pushes. Must be called whenever the
; lib's own segment state is reset, i.e. around deinit/init.
(defun seg-cache-reset () {
    (bufclear seg-cache 0xFF)
    (bufclear seg-cache-ov 0xFF)
    (setq hb-duty-front -1.0)
    (setq hb-duty-rear -1.0)
})

(defun seg-apply (seg fx pal color spd bri) {
    (if (>= seg 0) {
        (var o (* seg 8))
        (if (or (!= (bufget-u8 seg-cache o) fx)
                (!= (bufget-u8 seg-cache (+ o 1)) pal)
                (!= (bufget-u8 seg-cache (+ o 2)) spd)
                (!= (bufget-u8 seg-cache (+ o 3)) bri)
                (!= (bufget-u32 seg-cache-col (* seg 4)) color)) {
            ; One call, not five: seg-look sets the whole appearance under a
            ; single lock acquisition in the lib.
            (ext-esp_led-seg-look seg fx pal color spd bri)
            (bufset-u8 seg-cache o fx)
            (bufset-u8 seg-cache (+ o 1) pal)
            (bufset-u8 seg-cache (+ o 2) spd)
            (bufset-u8 seg-cache (+ o 3) bri)
            (bufset-u32 seg-cache-col (* seg 4) color)
        })
    })
})

; A fill bar. `color` 0 selects the lib's battery gradient (red when nearly
; empty, green when full); any other colour fills flat in that colour, and
; FX-GAUGE ignores the palette once a colour is set. The palette is reset to
; 0 either way so one left over from another mode cannot recolor the bar.
(defun seg-bar (seg color level spd bri) {
    (if (>= seg 0) {
        (var o (* seg 8))
        (if (or (!= (bufget-u8 seg-cache o) FX-GAUGE)
                (!= (bufget-u8 seg-cache (+ o 1)) 0)
                (!= (bufget-u8 seg-cache (+ o 2)) spd)
                (!= (bufget-u8 seg-cache (+ o 3)) bri)
                (!= (bufget-u8 seg-cache (+ o 4)) level)
                (!= (bufget-u32 seg-cache-col (* seg 4)) color)) {
            (ext-esp_led-seg-look seg FX-GAUGE 0 color spd bri)
            (ext-esp_led-seg-fx-val seg level)
            (bufset-u8 seg-cache o FX-GAUGE)
            (bufset-u8 seg-cache (+ o 1) 0)
            (bufset-u8 seg-cache (+ o 2) spd)
            (bufset-u8 seg-cache (+ o 3) bri)
            (bufset-u8 seg-cache (+ o 4) level)
            (bufset-u32 seg-cache-col (* seg 4) color)
        })
    })
})

(defun seg-gauge (seg level spd bri) (seg-bar seg 0 level spd bri))

; Overlay pixels are always driven white here, so only the brightness varies
; and only that is cached.
(defun seg-overlay-bri (seg bri) {
    (if (>= seg 0) {
        (if (!= (bufget-u16 seg-cache-ov (* seg 2)) bri) {
            (ext-esp_led-seg-overlay seg 0xFFFFFFFFu32 bri)
            (bufset-u16 seg-cache-ov (* seg 2) bri)
        })
    })
})

(defun display-battery-charging ()
    (or bms-charger-just-plugged (and (= led-show-battery-charging 1) bms-is-charging (not (running-state))))
)

; Head/tail pattern per LED mode. head-seg faces the direction of travel.
(defun apply-drive-mode (mode head-seg tail-seg head-bri tail-bri) {
    (cond
        ((= mode 0) { ; White / Red
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 head-bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF0000u32 32 tail-bri)
        })
        ((= mode 1) { ; Battery
            (seg-gauge head-seg (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) head-bri)
            (seg-gauge tail-seg (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) tail-bri)
        })
        ((= mode 2) { ; Cyan / Magenta
            (seg-apply head-seg FX-SOLID 0 0x0000FFFFu32 32 head-bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF00FFu32 32 tail-bri)
        })
        ((= mode 3) { ; Blue / Green
            (seg-apply head-seg FX-SOLID 0 0x000000FFu32 32 head-bri)
            (seg-apply tail-seg FX-SOLID 0 0x0000FF00u32 32 tail-bri)
        })
        ((= mode 4) { ; Yellow / Green
            (seg-apply head-seg FX-SOLID 0 0x00FFFF00u32 32 head-bri)
            (seg-apply tail-seg FX-SOLID 0 0x0000FF00u32 32 tail-bri)
        })
        ((= mode 5) { ; Rainbow
            (seg-apply head-seg FX-RAINBOW PAL-RGBW 0 32 head-bri)
            (seg-apply tail-seg FX-RAINBOW PAL-RGBW 0 32 tail-bri)
        })
        ((= mode 6) { ; Strobe
            (seg-apply head-seg FX-STROBE 0 0xFFFFFFFFu32 128 head-bri)
            (seg-apply tail-seg FX-STROBE 0 0xFFFFFFFFu32 128 tail-bri)
        })
        ((= mode 7) { ; Rave
            (seg-apply head-seg FX-RAINBOW PAL-NEON 0 220 head-bri)
            (seg-apply tail-seg FX-RAINBOW PAL-NEON 0 220 tail-bri)
        })
        ((= mode 8) { ; Rave directional
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 head-bri)
            (seg-apply tail-seg FX-RAINBOW PAL-NEON 0 220 tail-bri)
        })
        ((= mode 9) { ; Knight Rider
            (seg-apply head-seg FX-LARSON 0 0x00FF0000u32 48 head-bri)
            (seg-apply tail-seg FX-LARSON 0 0x00FF0000u32 48 tail-bri)
        })
        ((= mode 10) { ; Felony
            (seg-apply head-seg FX-FELONY 0 0 128 head-bri)
            (seg-apply tail-seg FX-FELONY 0 0 128 tail-bri)
        })
        ((= mode 11) { ; Trans pride (slow rainbow sweep)
            (seg-apply head-seg FX-RAINBOW PAL-RGBW 0 8 head-bri)
            (seg-apply tail-seg FX-RAINBOW PAL-RGBW 0 8 tail-bri)
        })
        (t {
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 head-bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF0000u32 32 tail-bri)
        })
    )
})

(defun update-status-leds (can-last-activity-time-sec bri) {
    (cond
        (handtest-mode {
            (seg-bar seg-status 0x000000FFu32
                (cond ((= switch-state 3) 255) ((or (= switch-state 1) (= switch-state 2)) 128) (t 16))
                0 bri)
        })
        ((= state 15) { ; disabled
            (seg-apply seg-status FX-SOLID 0 0x00FF0000u32 32 bri)
        })
        ((or (>= can-last-activity-time-sec 1) (< can-id 0)) { ; connecting
            (seg-apply seg-status FX-BREATHE 0 0x000000FFu32 64 bri)
        })
        ((> rpm 250.0) {
            (if (> sat-t 2) {
                (seg-apply seg-status FX-STROBE 0 0x00FF0000u32 200 bri)
            }{
                ; duty cycle bar
                (var duty (abs duty-cycle-now))
                (seg-bar seg-status
                    (cond ((> duty 0.8) 0x00FF0000u32) ((> duty 0.6) 0x00FFFF00u32) (t 0x0000FF00u32))
                    (to-i (* 255.0 duty)) 0 bri)
            })
        })
        ((or (= switch-state 1) (= switch-state 2) (= switch-state 3)) {
            ; footpad indication
            (seg-bar seg-status 0x0000FFFFu32 (if (= switch-state 3) 255 128) 0 bri)
        })
        (t {
            (seg-gauge seg-status (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
        })
    )
})

(defun update-aux-leds (bri) {
    (if (>= seg-footpad 0) {
        ; mode 0: rainbow
        (seg-apply seg-footpad FX-RAINBOW PAL-RGBW 0 32 bri)
    })
    (if (>= seg-button 0) {
        (if (= led-mode-button 1)
            (seg-gauge seg-button (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
            (seg-apply seg-button FX-RAINBOW PAL-RGBW 0 32 bri)
        )
    })
})

(defun led-loop () {
    (var have-segs (led-start))
    (dbg DBG-LED (str-merge "led start mode " (str-from-n led-mode "%d")
        " idle " (str-from-n led-mode-idle "%d")
        " hz " (str-from-n led-loop-delay "%d")))
    ; A zero rate would divide by zero here and put the loop into a
    ; crash/restart cycle with the LEDs stuck on the last frame.
    (if (< led-loop-delay 1) {
        (dbg-warn "led bad rate, using 50Hz")
        (setq led-loop-delay 50)
    })
    (var led-loop-delay-sec (/ 1.0 led-loop-delay))
    (var next-run-time (secs-since 0))
    (var prev-direction 1)
    (var direction-change-start-time 0)
    (var direction-change-window 0.5)
    (var prev-run-state 0)
    (var led-run-start-time 0)
    (var mall-grab-press-start 0)
    (var mall-grab-press-active nil)

    (loopwhile t {
        (if (!= dbg-mask 0) (setq dbg-ticks-led (+ dbg-ticks-led 1)))
        (var t-start (secs-since 0))
        ; Seed the phase marks so a branch that does not run (LEDs off, no
        ; segments) reports 0 for its phase instead of a stale timestamp.
        (if (dbg-active DBG-LED) {
            (setq dbg-led-t1 t-start)
            (setq dbg-led-t2 t-start)
            (setq dbg-led-t3 t-start)
        })

        (if led-exit-flag {
            (break)
        })

        ; Reinitialize in place when settings change
        (if led-reinit-flag {
            (dbg DBG-LED "led reinit")
            (led-teardown)
            (setq have-segs (led-start))
            (if (< led-loop-delay 1) (setq led-loop-delay 50))
            (setq led-loop-delay-sec (/ 1.0 led-loop-delay))
            (setq led-reinit-flag nil)
        })

        ; Direction detection with a commit window; ignore wheelslip (3)
        (var idle-rpm-darkride 100)
        (if (= state 4) ; RUNNING_UPSIDEDOWN
            (setq idle-rpm-darkride (* idle-rpm-darkride -1))
        )
        (if (!= state 3) {
            (var current-direction direction)
            (if (> rpm idle-rpm-darkride) (setq current-direction 1))
            (if (< rpm (* idle-rpm-darkride -1)) (setq current-direction -1))
            (if (!= current-direction prev-direction) {
                (if (= direction-change-start-time 0) {
                    (setq direction-change-start-time (systime))
                }{
                    (if (>= (secs-since direction-change-start-time) direction-change-window) {
                        (setq direction current-direction)
                        (setq prev-direction current-direction)
                        (setq direction-change-start-time 0)
                        (if (!= direction dbg-prev-direction) {
                            (setq dbg-prev-direction direction)
                            (dbg DBG-LED (str-merge "led dir " (str-from-n direction "%d")
                                " rpm " (str-from-n (to-float rpm) "%.0f")))
                        })
                    })
                })
            })
        })

        ; Mall grab: board held nose-up while not riding. Short press of
        ; the footpad toggles the LEDs, long press toggles the highbeam.
        (if (and (not (running-state)) (> pitch-angle 70)) {
            (setq led-mall-grab (if (= led-mall-grab-enabled 1) 1 0))
            (if (= switch-state 3) {
                (if (not mall-grab-press-active) {
                    (setq mall-grab-press-start (systime))
                    (setq mall-grab-press-active t)
                })
            }{
                (if mall-grab-press-active {
                    (var short-press (< (secs-since mall-grab-press-start) 1))
                    (if short-press
                        (setq led-on (if (= led-on 1) 0 1))
                        (setq led-highbeam-on (if (= led-highbeam-on 1) 0 1))
                    )
                    (dbg DBG-LED (str-merge "led mallgrab press on " (str-from-n (to-i led-on) "%d")
                        " hb " (str-from-n (to-i led-highbeam-on) "%d")))
                    (setq mall-grab-press-active nil)
                })
            })
        }{
            (setq led-mall-grab 0)
            (setq mall-grab-press-active nil)
        })

        (if (or (running-state) (= led-mall-grab 1) (display-battery-charging)) {
            (setq led-last-activity-time (systime))
        }{
            (setq direction 1)
        })

        (if (running-state) {
            (if (= prev-run-state 0) (setq led-run-start-time (systime)))
            (setq prev-run-state 1)
        }{
            (setq prev-run-state 0)
        })

        (if have-segs {
            (var last-activity-sec (secs-since led-last-activity-time))
            (var can-last-activity-time-sec (secs-since can-last-activity-time))

            ; Mode and brightness selection
            (var current-led-mode led-mode)
            (setq led-current-brightness (min led-brightness led-max-brightness))
            (if (= led-mall-grab 1) {
                (setq led-current-brightness (min led-brightness-status led-max-brightness))
            })
            (if (and (>= last-activity-sec idle-timeout) (<= can-last-activity-time-sec 1)) {
                (setq current-led-mode led-mode-idle)
                (setq led-current-brightness (min led-brightness-idle led-max-brightness))
            })
            (if (= state 5) {
                (setq led-current-brightness (min led-brightness-idle led-max-brightness))
            })
            (if (and (<= (secs-since 0) led-startup-timeout) (not (running-state))) {
                (setq current-led-mode led-mode-startup)
            })

            ; Brightness transitions are handled by the esp_led lib
            ; (ext-esp_led-fade), so targets are set directly here.
            ; Highbeams: the strip facing the direction of travel lights
            ; its highbeam (mode 1 = PWM pin, mode 2 = embedded overlay
            ; pixels) and the rest of that strip dims by the configured
            ; ratio (0 = fully off, like the original). Embedded light
            ; bars need a minimum drive, so their brightness is mapped
            ; onto the configured min-max range.
            (var highbeam-active (and (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)))
            (var hb-front (and highbeam-active (>= direction 0) (> led-front-highbeam-mode 0)))
            (var hb-rear (and highbeam-active (< direction 0) (> led-rear-highbeam-mode 0)))
            (var hb-frac (min led-brightness-highbeam led-max-brightness))
            (var front-bri (bri255 (* led-current-brightness (if hb-front led-dim-on-highbeam-ratio 1.0))))
            (var rear-bri (bri255 (* led-current-brightness (if hb-rear led-dim-on-highbeam-ratio 1.0))))

            ; Highbeam drive, only pushed on change - these sit in the loop
            ; body and would otherwise re-send an unchanged duty every tick.
            (if (and (= led-front-highbeam-mode 1) (>= led-front-highbeam-pin 0)) {
                (var d (if hb-front hb-frac 0.0))
                (if (!= d hb-duty-front) {
                    (pwm-set-duty d 0)
                    (setq hb-duty-front d)
                })
            })
            (if (and (= led-rear-highbeam-mode 1) (>= led-rear-highbeam-pin 0)) {
                (var d (if hb-rear hb-frac 0.0))
                (if (!= d hb-duty-rear) {
                    (pwm-set-duty d 1)
                    (setq hb-duty-rear d)
                })
            })
            (if (= led-front-highbeam-mode 2) {
                (seg-overlay-bri seg-front
                    (if hb-front (bri255 (+ led-front-highbeam-min (* (- led-front-highbeam-max led-front-highbeam-min) hb-frac))) 0))
            })
            (if (= led-rear-highbeam-mode 2) {
                (seg-overlay-bri seg-rear
                    (if hb-rear (bri255 (+ led-rear-highbeam-min (* (- led-rear-highbeam-max led-rear-highbeam-min) hb-frac))) 0))
            })

            (var status-bri (bri255 (min led-brightness-status led-max-brightness)))

            (if (dbg-active DBG-LED) {
                ; Edge-triggered: the effective mode and the on/off state are
                ; what people actually ask about ("why did my lights change?"),
                ; and they only move a handful of times per ride.
                (if (!= current-led-mode dbg-prev-led-mode) {
                    (setq dbg-prev-led-mode current-led-mode)
                    (dbg DBG-LED (str-merge "led mode " (str-from-n current-led-mode "%d")
                        " idle " (str-from-n last-activity-sec "%.1f")
                        " bri " (str-from-n led-current-brightness "%.2f")))
                })
                (if (!= (to-i led-on) dbg-prev-led-on) {
                    (setq dbg-prev-led-on (to-i led-on))
                    (dbg DBG-LED (if (= led-on 1) "led on" "led off"))
                })
                (var hb (+ (if hb-front 1 0) (if hb-rear 2 0)))
                (if (!= hb dbg-prev-hb) {
                    (setq dbg-prev-hb hb)
                    (dbg DBG-LED (str-merge "led hb " (str-from-n hb "%d")
                        " " (str-from-n hb-frac "%.2f")))
                })
                (if (dbg-due 'led-state 5.0)
                    (dbg DBG-LED (str-merge "led bri " (str-from-n front-bri "%d")
                        " " (str-from-n rear-bri "%d")
                        " " (str-from-n status-bri "%d")
                        " dir " (str-from-n direction "%d")
                        " mg " (str-from-n led-mall-grab "%d")
                        " age " (str-from-n can-last-activity-time-sec "%.2f"))))
            })

            (setq dbg-led-t1 (secs-since 0))
            (update-status-leds can-last-activity-time-sec status-bri)
            (setq dbg-led-t2 (secs-since 0))

            (if (= led-on 1) {
                (var head-seg (if (> direction 0) seg-front seg-rear))
                (var tail-seg (if (> direction 0) seg-rear seg-front))
                (var head-bri (if (> direction 0) front-bri rear-bri))
                (var tail-bri (if (> direction 0) rear-bri front-bri))
                (var aux-bri (bri255 led-current-brightness))
                (var frozen (and (running-state) (= led-update-not-running 1) (> (secs-since led-run-start-time) 1)))

                (cond
                    ((= state 15) {
                        (seg-apply seg-front FX-SOLID 0 0x00FF0000u32 32 front-bri)
                        (seg-apply seg-rear FX-SOLID 0 0x00FF0000u32 32 rear-bri)
                    })
                    (handtest-mode {
                        (seg-apply seg-front FX-BREATHE 0 0x000000FFu32 64 front-bri)
                        (seg-apply seg-rear FX-BREATHE 0 0x000000FFu32 64 rear-bri)
                    })
                    ((and (> last-activity-sec idle-timeout-shutoff) (< can-last-activity-time-sec 1) (!= state 5)) {
                        (seg-apply seg-front FX-OFF 0 0 32 0)
                        (seg-apply seg-rear FX-OFF 0 0 32 0)
                    })
                    ((and (or (= current-led-mode 1) (= led-mall-grab 1)) (< can-last-activity-time-sec 1)) {
                        (seg-gauge seg-front (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) front-bri)
                        (seg-gauge seg-rear (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) rear-bri)
                    })
                    ((or (and (> can-last-activity-time-sec 1) (> (secs-since 0) led-startup-timeout)) frozen) {
                        ; No telemetry or frozen: plain white/red
                        (apply-drive-mode 0 head-seg tail-seg head-bri tail-bri)
                    })
                    (t {
                        (apply-drive-mode current-led-mode head-seg tail-seg head-bri tail-bri)
                    })
                )

                ; Brake light: strobe the tail red while braking
                (if (and (= led-brake-light-enabled 1) (running-state) (!= state 5) (<= tot-current led-brake-light-min-amps) (= led-update-not-running 0)) {
                    (seg-apply tail-seg FX-STROBE 0 0x00FF0000u32 200 tail-bri)
                })

                (if (display-battery-charging) {
                    (seg-gauge seg-front (to-i (* 255.0 battery-percent-remaining)) 32 front-bri)
                    (seg-gauge seg-rear (to-i (* 255.0 battery-percent-remaining)) 32 rear-bri)
                })

                (update-aux-leds aux-bri)
                (setq dbg-led-t3 (secs-since 0))
            }{
                ; LEDs off: blank the drive/aux strips, keep the status bar
                (seg-apply seg-front FX-OFF 0 0 32 0)
                (seg-apply seg-rear FX-OFF 0 0 32 0)
                (seg-apply seg-footpad FX-OFF 0 0 32 0)
                (seg-apply seg-button FX-OFF 0 0 32 0)
            })
        })

        (var work (- (secs-since 0) t-start))
        (var time-to-wait (- next-run-time (secs-since 0)))
        (if (> time-to-wait 0)
            (yield (* time-to-wait 1000000))
            {
                ; The loop is not keeping up with the configured rate - the
                ; visible symptom is stuttering effects.
                ; work = time this iteration actually spent executing. If
                ; work is small but the overrun is large, the loop is being
                ; starved by another thread holding the evaluator rather
                ; than being slow itself. decide/status/drive split the body.
                ;
                ; Only reported past a quarter of the period, and with the
                ; number of misses since the last report. The previous
                ; version warned on any miss and printed one line per 5 s,
                ; which made ordinary scheduler jitter look identical to a
                ; real stall and hid how often either was happening.
                (setq dbg-led-overruns (+ dbg-led-overruns 1))
                (if (and (> (- 0 time-to-wait) (* led-loop-delay-sec 0.25))
                         (dbg-tick DBG-LED 'led-loop 5.0)) {
                    (dbg-warn (str-merge "led overrun " (str-from-n (- 0 time-to-wait) "%.4f")
                        " n " (str-from-n dbg-led-overruns "%d")
                        " want " (str-from-n led-loop-delay-sec "%.4f")
                        " work " (str-from-n work "%.4f")
                        " decide " (str-from-n (- dbg-led-t1 t-start) "%.4f")
                        " status " (str-from-n (- dbg-led-t2 dbg-led-t1) "%.4f")
                        " drive " (str-from-n (- dbg-led-t3 dbg-led-t2) "%.4f")))
                    (setq dbg-led-overruns 0)
                })
                (setq next-run-time (secs-since 0))
            }
        )
        (setq next-run-time (+ next-run-time led-loop-delay-sec))
    })

    (led-teardown)
    (setq led-exit-flag nil)
})

@const-end
