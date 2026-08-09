; float-accessories.lisp
; Smart LED Control, Tilt Remote and stock OW BMS bridge for VESC Express
; Version 2.0
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools & marcos (avaspark), auden_builds (pubmote)
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Pickles
;
; This version renders LEDs through the esp_led_strip native lib and keeps
; its configuration in a VESC custom config (Float Accessories in VESC
; Tool) provided by the fa_cfg native lib - see conf/settings.xml.

; Native libs, one binary per chip.
(import "fa_cfg/fa_cfg_esp32c3.bin" 'facfg-esp32c3)
(import "fa_cfg/fa_cfg_esp32c6.bin" 'facfg-esp32c6)
(import "fa_cfg/fa_cfg_esp32s3.bin" 'facfg-esp32s3)
(import "fa_cfg/fa_cfg_esp32p4.bin" 'facfg-esp32p4)
(import "../lib_esp_led_strip/esp_led_strip/esp_led_strip_esp32c3.bin" 'esp_led-esp32c3)
(import "../lib_esp_led_strip/esp_led_strip/esp_led_strip_esp32c6.bin" 'esp_led-esp32c6)
(import "../lib_esp_led_strip/esp_led_strip/esp_led_strip_esp32s3.bin" 'esp_led-esp32s3)
(import "../lib_esp_led_strip/esp_led_strip/esp_led_strip_esp32p4.bin" 'esp_led-esp32p4)

@const-start
; debug.lisp first: every other module logs through it.
(import "lib/debug.lisp" 'debug)
(read-eval-program debug)
(import "lib/utils.lisp" 'utils)
(read-eval-program utils)
(import "lib/settings-vars.lisp" 'settings-vars)
(read-eval-program settings-vars)
(import "lib/settings.lisp" 'settings)
(read-eval-program settings)
(import "lib/can.lisp" 'can)
(read-eval-program can)
(import "lib/logger.lisp" 'logger)
(read-eval-program logger)
(import "lib/gnss.lisp" 'gnss)
(read-eval-program gnss)
(import "lib/humidity.lisp" 'humidity)
(read-eval-program humidity)
(import "../lib_esp_led_strip/esp_led_defs.lisp" 'esp_led-defs)
(read-eval-program esp_led-defs)
(import "lib/led-vars.lisp" 'led-vars)
(read-eval-program led-vars)
(import "lib/led.lisp" 'led)
(read-eval-program led)
(import "lib/bms-vars.lisp" 'bms-vars)
(read-eval-program bms-vars)
(import "lib/bms.lisp" 'bms)
(read-eval-program bms)
(import "pubmote/pubmote-consts.lisp" 'pubmote-consts)
(import "pubmote/pubmote-vars.lisp" 'pubmote-vars)
(import "pubmote/pubmote-utils.lisp" 'pubmote-utils)
(import "pubmote/pubmote.lisp" 'pubmote)
(read-eval-program pubmote-consts)
(read-eval-program pubmote-vars)
(read-eval-program pubmote-utils)
(read-eval-program pubmote)
(import "lib/commands.lisp" 'commands)
(read-eval-program commands)

(defun load-native-libs () {
    (var target (sysinfo 'hw-target))
    (var libs (cond
        ((= (str-cmp target "esp32c3") 0) (list facfg-esp32c3 esp_led-esp32c3))
        ((= (str-cmp target "esp32c6") 0) (list facfg-esp32c6 esp_led-esp32c6))
        ((= (str-cmp target "esp32s3") 0) (list facfg-esp32s3 esp_led-esp32s3))
        ((= (str-cmp target "esp32p4") 0) (list facfg-esp32p4 esp_led-esp32p4))
        (t nil)
    ))
    (if (eq libs nil) {
        (exit-error (str-merge "No native libs for target " target))
    })
    ; Trapped individually: without this a failed load shows up much later
    ; as "undefined extension ext-facfg-get" from whichever thread touches
    ; the config first, which points at the wrong thing entirely.
    (var r-cfg (trap (load-native-lib (ix libs 0))))
    (if (eq (ix r-cfg 0) 'exit-error)
        (dbg-err (str-merge "fa_cfg load: " (to-str (ix r-cfg 1)))))
    (var r-led (trap (load-native-lib (ix libs 1))))
    (if (eq (ix r-led 0) 'exit-error)
        (dbg-err (str-merge "esp_led load: " (to-str (ix r-led 1)))))
    (dbg DBG-CORE "libs loaded")
})

; Boot phase marks, seconds since power-on. Printed as one line at the end
; of main so "it takes ages to boot" can be attributed instead of guessed
; at: `code` is everything before main (firmware bringup plus loading this
; package, or restoring its image), and the rest are the phases of main.
(def boot-t-main 0.0)
(def boot-t-libs 0.0)
(def boot-t-led 0.0)

(defun main () {
    (setq boot-t-main (secs-since 0))
    (setup)
    (init)
    (print (str-merge "Boot " (str-from-n (secs-since 0) "%.3f") "s"
        " (code " (str-from-n boot-t-main "%.3f")
        " libs " (str-from-n (- boot-t-libs boot-t-main) "%.3f")
        " led " (str-from-n (- boot-t-led boot-t-libs) "%.3f")
        " rest " (str-from-n (- (secs-since 0) boot-t-led) "%.3f") ")"))
})
(defun spawn-with-restart (name stack-size func) {
    (var monitor-fn (fn ()
        (loopwhile t {
            (if stack-size
                (spawn-trap stack-size func)
                (spawn-trap func))
            (dbg DBG-CORE (str-merge "spawn " name))
            (recv   ((exit-error (? tid) (? e)) {
                        ; Always printed: a worker dying is the single most
                        ; useful thing to see, and it is followed by a >=1 s
                        ; blackout of whatever that loop drives.
                        (dbg-err (str-merge name " " (to-str e)))
                    })
                    ((exit-ok (? tid) (? v)) {
                        (dbg DBG-CORE (str-merge "exit " name))
                    }))
            (sleep 1.0)
        })
    ))
    (if stack-size
        (spawn stack-size monitor-fn)
        (spawn monitor-fn))
})

; The event handler must be registered with the worker thread's id each time it
; is (re)spawned: events are delivered to the registered thread's mailbox, and
; a recv only consumes matching messages. Registering the monitor thread (whose
; recv only matches exit messages) would silently swallow every event.
(defun spawn-event-handler-with-restart () {
    (spawn (fn ()
        (loopwhile t {
            (event-register-handler (spawn-trap event-handler))
            (dbg DBG-CORE "spawn event-handler")
            (recv   ((exit-error (? tid) (? e))
                        (dbg-err (str-merge "event-handler " (to-str e)))
                    )
                    ((exit-ok (? tid) (? v)) 'ok))
            (sleep 1.0)
        })
    ))
})

(defun setup () {
    ; fw-num is a global (settings-vars.lisp): bms.lisp gates two crypto
    ; paths on it, and as a local `var` here it was unbound by the time the
    ; BMS loop read it.
    (setq fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
    (print (str-merge "Float Accessories " (to-str (get-version))
        " on " (sysinfo 'hw-target)
        " fw " (str-from-n fw-num "%.2f")))

    (setq can-last-activity-time (systime))
    (setq bms-last-activity-time (systime))
    (setq led-last-activity-time (systime))

    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })

    (if (< fw-num 7.00) (exit-error "hw-express needs to be running 7.00"))

    (load-native-libs)
    (setq boot-t-libs (secs-since 0))

    ; The fa_cfg lib loads the stored config itself (defaults when nothing
    ; valid is stored - the confparser signature replaces the old
    ; magic/crc scheme). Mirror the control state into the lisp vars.
    (setq led-on (get-config 'led-on))
    (setq led-highbeam-on (get-config 'led-highbeam-on))
    (setq led-brightness (get-config 'led-brightness))
    (setq led-brightness-highbeam (get-config 'led-brightness-highbeam))
    (setq led-brightness-idle (get-config 'led-brightness-idle))
    (setq led-brightness-status (get-config 'led-brightness-status))

    ; Lighting first, and synchronously. Everything after this point either
    ; spawns a thread that competes for the evaluator or blocks it outright
    ; (CAN discovery, BMS/GNSS UART, humidity I2C), and lisp threads share a
    ; single evaluator - so anything started before the strips are defined
    ; delays the first frame. Defining the segments here rather than inside
    ; the LED thread also gets the lib's render thread - a real FreeRTOS
    ; thread, unaffected by whatever the evaluator is doing - running before
    ; the loop has had its first slice.
    ;
    ; Trapped: this used to run inside spawn-with-restart, where a bad
    ; config could only kill the LED thread. led-start leaves its done flag
    ; clear if it throws, so the loop retries it.
    (if (= (get-config 'led-enabled) 1) {
        (var r (trap (led-start)))
        (if (eq (ix r 0) 'exit-error)
            (dbg-err (str-merge "led start " (to-str (ix r 1)))))
        (setq led-context-id (spawn-with-restart "led-loop" nil led-loop))
    })
    (setq boot-t-led (secs-since 0))

    (spawn-event-handler-with-restart)
    (event-enable 'event-data-rx)
    (event-enable 'event-esp-now-rx)
})

(defun init () {
    ; The heartbeat thread first, so a crash in any loop spawned below is
    ; visible as a dead tick counter straight away.
    (spawn-with-restart "dbg-loop" nil dbg-loop)

    ; The LED loop is not spawned here - setup() brings the strips up and
    ; starts it before any of this, so the lights are already on while CAN
    ; discovery and the rest of the peripherals come up behind them.
    (setq can-context-id (spawn-with-restart "can-loop" nil can-loop))
    ; Always inject the pubmote callbacks, even with pubmote disabled -
    ; enabling it later from the settings page spawns pubmote-loop through
    ; apply-config, which must never run with unset callbacks.
    (pubmote-setup
        VEHICLE_TYPE_ONEWHEEL
        (fn (jsy jsx bt-c bt-z is-rev) {
            (setq pubmote-last-jsy jsy)
            (setq pubmote-last-jsx jsx)
            (setq pubmote-last-bt-c bt-c)
            (setq pubmote-last-bt-z bt-z)
            (setq pubmote-last-is-rev is-rev)
            (if (>= (get-config 'can-id) 0) {
                (can-cmd (get-config 'can-id) (str-replace (to-str (list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
            })
        })
        (fn () {
            (list fault-code pitch-angle roll-angle state switch-state vin rpm speed tot-current duty-cycle-now distance-abs fet-temp-filtered motor-temp-filtered odometer battery-percent-remaining)
        })
        (fn (text) {
            (send-msg text)
        })
        (fn (name) {
            (get-config name)
        })
        (fn (name val) {
            (set-config name val)
        })
        (fn () {
            ; Persist the pairing (remote mac + secret) in the config
            (ext-facfg-store)
        })
        (fn (state) {
            ; The QML pairing flow watches this
            (send-data (str-merge "pairing-status " (to-str state)))
        })
    )
    (if (= (get-config 'pubmote-enabled) 1){
        (setq pubmote-context-id (spawn-with-restart "pubmote-loop" nil pubmote-loop))
    })
    (if (= (get-config 'bms-enabled) 1){
        (setq bms-context-id (spawn-with-restart "bms-loop" nil bms-loop))
    })

    (if (= (get-config 'humidity-enabled) 1) (setq humidity-context-id (spawn-with-restart "humidity-loop" nil humidity-loop)))

    (if (= (get-config 'gnss-enabled) 1) (setq gnss-context-id (spawn-with-restart "gnss-loop" nil gnss-loop)))


    (if (= (get-config 'log-enabled) 1) (setq log-context-id (spawn-with-restart "log-loop" 50 log-loop)))

    ; Apply config edits made in VESC Tool (Float Accessories) at runtime
    (spawn-with-restart "config-watch" nil config-watch-loop)
})

; Save the environment as a binary image for fast boot on subsequent power-cycles.
; On the very next boot the reader is skipped and main() is called directly.
(if (is-606-or-newer) {
    (image-save)
})
; Start immediately on this (first) boot too.
(main)
@const-end
