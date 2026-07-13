;@const-symbol-strings
@const-start

; LED control on top of the espled_strip native lib. The lib owns the
; framebuffers, the render thread and the LED driver; this loop only runs
; the state machine (direction, idle, mall grab, brake light, charging)
; and sets per-segment effect/color/brightness through the ext-espled-*
; extensions. Strips on the same pin are chained with segment offsets.
;
; Compared to the rgbled-based implementation this drops the special
; interleaved-highbeam strip types (JetFleet/GTFO); "Standard + PWM
; Highbeam" with a separate highbeam pin is supported.

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
    (setq led-front-pin (get-config 'led-front-pin))
    (setq led-front-num (get-config 'led-front-num))
    (setq led-front-type (get-config 'led-front-type))
    (setq led-front-reversed (get-config 'led-front-reversed))
    (setq led-front-strip-type (get-config 'led-front-strip-type))
    (setq led-rear-pin (get-config 'led-rear-pin))
    (setq led-rear-num (get-config 'led-rear-num))
    (setq led-rear-type (get-config 'led-rear-type))
    (setq led-rear-reversed (get-config 'led-rear-reversed))
    (setq led-rear-strip-type (get-config 'led-rear-strip-type))
    (setq led-button-pin (get-config 'led-button-pin))
    (setq led-button-strip-type (get-config 'led-button-strip-type))
    (setq led-footpad-pin (get-config 'led-footpad-pin))
    (setq led-footpad-num (get-config 'led-footpad-num))
    (setq led-footpad-type (get-config 'led-footpad-type))
    (setq led-footpad-reversed (get-config 'led-footpad-reversed))
    (setq led-footpad-strip-type (get-config 'led-footpad-strip-type))
    (setq led-startup-timeout (get-config 'led-startup-timeout))
    (setq led-dim-on-highbeam-ratio (get-config 'led-dim-on-highbeam-ratio))
    (setq led-status-strip-type (get-config 'led-status-strip-type))
    (setq led-loop-delay (get-config 'led-loop-delay))
    (setq led-show-battery-charging (get-config 'led-show-battery-charging))
    (setq led-front-highbeam-pin (get-config 'led-front-highbeam-pin))
    (setq led-rear-highbeam-pin (get-config 'led-rear-highbeam-pin))
    (setq led-max-brightness (get-config 'led-max-brightness))
    (setq led-update-not-running (get-config 'led-update-not-running))
})

; Define the espled segments from the config. Strips sharing a pin become
; one chain: each next strip on the pin gets the accumulated pixel offset.
; Chain order matches the original wiring convention: status, front, rear,
; then footpad and button.
(defun led-setup-segments () {
    (ext-espled-deinit)
    (setq seg-front -1)
    (setq seg-rear -1)
    (setq seg-status -1)
    (setq seg-footpad -1)
    (setq seg-button -1)

    (var idx 0)
    (var pin-offsets nil) ; assoc pin -> next chain offset

    (var next-offset (fn (pin len) {
        (var entry (assoc pin-offsets pin))
        (var off (if (eq entry nil) 0 entry))
        (setq pin-offsets (acons pin (+ off len) pin-offsets))
        off
    }))

    (if (and (> led-status-strip-type 0) (>= led-status-pin 0) (> led-status-num 0)) {
        (ext-espled-seg-def idx led-status-pin led-status-type led-status-num (next-offset led-status-pin led-status-num))
        (setq seg-status idx)
        (setq idx (+ idx 1))
    })
    (if (and (> led-front-strip-type 0) (>= led-front-pin 0) (> led-front-num 0)) {
        (ext-espled-seg-def idx led-front-pin led-front-type led-front-num (next-offset led-front-pin led-front-num))
        (setq seg-front idx)
        (setq idx (+ idx 1))
    })
    (if (and (> led-rear-strip-type 0) (>= led-rear-pin 0) (> led-rear-num 0)) {
        (ext-espled-seg-def idx led-rear-pin led-rear-type led-rear-num (next-offset led-rear-pin led-rear-num))
        (setq seg-rear idx)
        (setq idx (+ idx 1))
    })
    (if (and (> led-footpad-strip-type 0) (>= led-footpad-pin 0) (> led-footpad-num 0)) {
        (ext-espled-seg-def idx led-footpad-pin led-footpad-type led-footpad-num (next-offset led-footpad-pin led-footpad-num))
        (setq seg-footpad idx)
        (setq idx (+ idx 1))
    })
    (if (and (> led-button-strip-type 0) (>= led-button-pin 0)) {
        (ext-espled-seg-def idx led-button-pin 0 1 (next-offset led-button-pin 1))
        (setq seg-button idx)
        (setq idx (+ idx 1))
    })

    (if (> idx 0) {
        (ext-espled-init idx)
        (if (>= seg-status 0) (ext-espled-seg-reverse seg-status led-status-reversed))
        (if (>= seg-front 0) (ext-espled-seg-reverse seg-front led-front-reversed))
        (if (>= seg-rear 0) (ext-espled-seg-reverse seg-rear led-rear-reversed))
        (if (>= seg-footpad 0) (ext-espled-seg-reverse seg-footpad led-footpad-reversed))
    })

    ; PWM highbeams (strip type 2 = Standard + PWM Highbeam)
    (if (and (= led-front-strip-type 2) (>= led-front-highbeam-pin 0)) {
        (pwm-start 1000 0.0 0 led-front-highbeam-pin 10)
    })
    (if (and (= led-rear-strip-type 2) (>= led-rear-highbeam-pin 0)) {
        (pwm-start 1000 0.0 1 led-rear-highbeam-pin 10)
    })

    (> idx 0)
})

(defun led-teardown () {
    (ext-espled-deinit)
    (if (and (= led-front-strip-type 2) (>= led-front-highbeam-pin 0)) (pwm-stop 0))
    (if (and (= led-rear-strip-type 2) (>= led-rear-highbeam-pin 0)) (pwm-stop 1))
})

(defun bri255 (b) (to-i (* 255.0 (min (max b 0.0) 1.0))))

(defun seg-apply (seg fx pal color spd bri) {
    (if (>= seg 0) {
        (ext-espled-seg-fx seg fx)
        (ext-espled-seg-pal seg pal)
        (ext-espled-seg-col seg color)
        (ext-espled-seg-spd seg spd)
        (ext-espled-seg-bri seg bri)
    })
})

(defun seg-gauge (seg level spd bri) {
    (if (>= seg 0) {
        (ext-espled-seg-fx seg FX-GAUGE)
        (ext-espled-seg-col seg 0)
        (ext-espled-seg-level seg level)
        (ext-espled-seg-spd seg spd)
        (ext-espled-seg-bri seg bri)
    })
})

(defun display-battery-charging ()
    (or bms-charger-just-plugged (and (= led-show-battery-charging 1) bms-is-charging (not (running-state))))
)

; Head/tail pattern per LED mode. head-seg faces the direction of travel.
(defun apply-drive-mode (mode head-seg tail-seg bri) {
    (cond
        ((= mode 0) { ; White / Red
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF0000u32 32 bri)
        })
        ((= mode 1) { ; Battery
            (seg-gauge head-seg (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
            (seg-gauge tail-seg (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
        })
        ((= mode 2) { ; Cyan / Magenta
            (seg-apply head-seg FX-SOLID 0 0x0000FFFFu32 32 bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF00FFu32 32 bri)
        })
        ((= mode 3) { ; Blue / Green
            (seg-apply head-seg FX-SOLID 0 0x000000FFu32 32 bri)
            (seg-apply tail-seg FX-SOLID 0 0x0000FF00u32 32 bri)
        })
        ((= mode 4) { ; Yellow / Green
            (seg-apply head-seg FX-SOLID 0 0x00FFFF00u32 32 bri)
            (seg-apply tail-seg FX-SOLID 0 0x0000FF00u32 32 bri)
        })
        ((= mode 5) { ; Rainbow
            (seg-apply head-seg FX-RAINBOW 0 0 32 bri)
            (seg-apply tail-seg FX-RAINBOW 0 0 32 bri)
        })
        ((= mode 6) { ; Strobe
            (seg-apply head-seg FX-STROBE 0 0xFFFFFFFFu32 128 bri)
            (seg-apply tail-seg FX-STROBE 0 0xFFFFFFFFu32 128 bri)
        })
        ((= mode 7) { ; Rave
            (seg-apply head-seg FX-RAINBOW 3 0 220 bri)
            (seg-apply tail-seg FX-RAINBOW 3 0 220 bri)
        })
        ((= mode 8) { ; Rave directional
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 bri)
            (seg-apply tail-seg FX-RAINBOW 3 0 220 bri)
        })
        ((= mode 9) { ; Knight Rider
            (seg-apply head-seg FX-LARSON 0 0x00FF0000u32 48 bri)
            (seg-apply tail-seg FX-LARSON 0 0x00FF0000u32 48 bri)
        })
        ((= mode 10) { ; Felony
            (seg-apply head-seg FX-FELONY 0 0 128 bri)
            (seg-apply tail-seg FX-FELONY 0 0 128 bri)
        })
        ((= mode 11) { ; Trans pride (slow rainbow sweep)
            (seg-apply head-seg FX-RAINBOW 0 0 8 bri)
            (seg-apply tail-seg FX-RAINBOW 0 0 8 bri)
        })
        (t {
            (seg-apply head-seg FX-SOLID 0 0xFFFFFFFFu32 32 bri)
            (seg-apply tail-seg FX-SOLID 0 0x00FF0000u32 32 bri)
        })
    )
})

(defun update-status-leds (can-last-activity-time-sec bri) {
    (cond
        (handtest-mode {
            (if (>= seg-status 0) {
                (ext-espled-seg-fx seg-status FX-GAUGE)
                (ext-espled-seg-col seg-status 0x000000FFu32)
                (ext-espled-seg-level seg-status (cond ((= switch-state 3) 255) ((or (= switch-state 1) (= switch-state 2)) 128) (t 16)))
                (ext-espled-seg-spd seg-status 0)
                (ext-espled-seg-bri seg-status bri)
            })
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
                (if (>= seg-status 0) {
                    (ext-espled-seg-fx seg-status FX-GAUGE)
                    (ext-espled-seg-col seg-status (cond ((> duty 0.8) 0x00FF0000u32) ((> duty 0.6) 0x00FFFF00u32) (t 0x0000FF00u32)))
                    (ext-espled-seg-level seg-status (to-i (* 255.0 duty)))
                    (ext-espled-seg-spd seg-status 0)
                    (ext-espled-seg-bri seg-status bri)
                })
            })
        })
        ((or (= switch-state 1) (= switch-state 2) (= switch-state 3)) {
            ; footpad indication
            (if (>= seg-status 0) {
                (ext-espled-seg-fx seg-status FX-GAUGE)
                (ext-espled-seg-col seg-status 0x0000FFFFu32)
                (ext-espled-seg-level seg-status (if (= switch-state 3) 255 128))
                (ext-espled-seg-spd seg-status 0)
                (ext-espled-seg-bri seg-status bri)
            })
        })
        (t {
            (seg-gauge seg-status (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
        })
    )
})

(defun update-aux-leds (bri) {
    (if (>= seg-footpad 0) {
        ; mode 0: rainbow
        (seg-apply seg-footpad FX-RAINBOW 0 0 32 bri)
    })
    (if (>= seg-button 0) {
        (if (= led-mode-button 1)
            (seg-gauge seg-button (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
            (seg-apply seg-button FX-RAINBOW 0 0 32 bri)
        )
    })
})

(defun led-loop () {
    (load-led-settings)
    (var have-segs (led-setup-segments))
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
        (if led-exit-flag {
            (break)
        })

        ; Reinitialize in place when settings change
        (if led-reinit-flag {
            (led-teardown)
            (load-led-settings)
            (setq have-segs (led-setup-segments))
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
                    (if (< (secs-since mall-grab-press-start) 1)
                        (setq led-on (if (= led-on 1) 0 1))
                        (setq led-highbeam-on (if (= led-highbeam-on 1) 0 1))
                    )
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

            ; Smooth brightness toward the target
            (var brightness-step (* 5.0 led-loop-delay-sec))
            (if (> led-current-brightness led-smoothed-brightness)
                (setq led-smoothed-brightness (min led-current-brightness (+ led-smoothed-brightness brightness-step)))
                (setq led-smoothed-brightness (max led-current-brightness (- led-smoothed-brightness brightness-step)))
            )

            ; PWM highbeams + dimming while they are on
            (var highbeam-active (and (= led-on 1) (= led-highbeam-on 1) (running-state) (!= state 5)))
            (var main-bri led-smoothed-brightness)
            (if (and (= led-front-strip-type 2) (>= led-front-highbeam-pin 0)) {
                (if (and highbeam-active (>= direction 0)) {
                    (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 0)
                    (if (> led-dim-on-highbeam-ratio 0.0) (setq main-bri (* led-smoothed-brightness led-dim-on-highbeam-ratio)))
                }{
                    (pwm-set-duty 0.0 0)
                })
            })
            (if (and (= led-rear-strip-type 2) (>= led-rear-highbeam-pin 0)) {
                (if (and highbeam-active (< direction 0)) {
                    (pwm-set-duty (min led-brightness-highbeam led-max-brightness) 1)
                    (if (> led-dim-on-highbeam-ratio 0.0) (setq main-bri (* led-smoothed-brightness led-dim-on-highbeam-ratio)))
                }{
                    (pwm-set-duty 0.0 1)
                })
            })

            (var bri (bri255 main-bri))
            (var status-bri (bri255 (min led-brightness-status led-max-brightness)))

            (update-status-leds can-last-activity-time-sec status-bri)

            (if (= led-on 1) {
                (var head-seg (if (> direction 0) seg-front seg-rear))
                (var tail-seg (if (> direction 0) seg-rear seg-front))
                (var frozen (and (running-state) (= led-update-not-running 1) (> (secs-since led-run-start-time) 1)))

                (cond
                    ((= state 15) {
                        (seg-apply seg-front FX-SOLID 0 0x00FF0000u32 32 bri)
                        (seg-apply seg-rear FX-SOLID 0 0x00FF0000u32 32 bri)
                    })
                    (handtest-mode {
                        (seg-apply seg-front FX-BREATHE 0 0x000000FFu32 64 bri)
                        (seg-apply seg-rear FX-BREATHE 0 0x000000FFu32 64 bri)
                    })
                    ((and (> last-activity-sec idle-timeout-shutoff) (< can-last-activity-time-sec 1) (!= state 5)) {
                        (seg-apply seg-front FX-SOLID 0 0 32 0)
                        (seg-apply seg-rear FX-SOLID 0 0 32 0)
                    })
                    ((and (or (= current-led-mode 1) (= led-mall-grab 1)) (< can-last-activity-time-sec 1)) {
                        (seg-gauge seg-front (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
                        (seg-gauge seg-rear (to-i (* 255.0 battery-percent-remaining)) (if bms-is-charging 32 0) bri)
                    })
                    ((or (and (> can-last-activity-time-sec 1) (> (secs-since 0) led-startup-timeout)) frozen) {
                        ; No telemetry or frozen: plain white/red
                        (apply-drive-mode 0 head-seg tail-seg bri)
                    })
                    (t {
                        (apply-drive-mode current-led-mode head-seg tail-seg bri)
                    })
                )

                ; Brake light: strobe the tail red while braking
                (if (and (= led-brake-light-enabled 1) (running-state) (!= state 5) (<= tot-current led-brake-light-min-amps) (= led-update-not-running 0)) {
                    (seg-apply tail-seg FX-STROBE 0 0x00FF0000u32 200 bri)
                })

                (if (display-battery-charging) {
                    (seg-gauge seg-front (to-i (* 255.0 battery-percent-remaining)) 32 bri)
                    (seg-gauge seg-rear (to-i (* 255.0 battery-percent-remaining)) 32 bri)
                })

                (update-aux-leds bri)
            }{
                ; LEDs off: blank the drive/aux strips, keep the status bar
                (seg-apply seg-front FX-SOLID 0 0 32 0)
                (seg-apply seg-rear FX-SOLID 0 0 32 0)
                (seg-apply seg-footpad FX-SOLID 0 0 32 0)
                (seg-apply seg-button FX-SOLID 0 0 32 0)
            })
        })

        (var time-to-wait (- next-run-time (secs-since 0)))
        (if (> time-to-wait 0)
            (yield (* time-to-wait 1000000))
            (setq next-run-time (secs-since 0))
        )
        (setq next-run-time (+ next-run-time led-loop-delay-sec))
    })

    (led-teardown)
    (setq led-exit-flag nil)
})

@const-end
