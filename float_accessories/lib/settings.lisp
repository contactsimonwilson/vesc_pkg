;@const-symbol-strings
@const-start

; Configuration access. The config itself is a VESC custom config provided
; by the fa_cfg native lib (see conf/settings.xml). It is edited in VESC
; Tool's standard parameter UI and persisted by the firmware - the package
; no longer implements its own eeprom layout, magic numbers or CRCs.
;
; Parameter names: the lisp code uses its traditional dashed symbols
; (e.g. 'led-front-pin); the native lib treats '-' and '_' as equal.

(defun get-config (name)
    (ext-facfg-get (sym2str name))
)

(defun set-config (name value)
    (ext-facfg-set (sym2str name) value)
)

(defun save-config () {
    (ext-facfg-store)
    (send-status "Settings saved")
})

(defun restore-config () {
    (ext-facfg-restore)
    (send-status "Settings restored")
})

(defun print-config ()
    (print "Configuration is in VESC Tool: Float Accessories Cfg")
)

; Applies runtime feature changes after the config was edited (from VESC
; Tool or from lisp). Starts/stops the feature loops to match the config.
(defun apply-config () {
    (setq led-on (get-config 'led-on))
    (setq led-highbeam-on (get-config 'led-highbeam-on))
    (setq led-brightness (get-config 'led-brightness))
    (setq led-brightness-highbeam (get-config 'led-brightness-highbeam))
    (setq led-brightness-idle (get-config 'led-brightness-idle))
    (setq led-brightness-status (get-config 'led-brightness-status))

    (if (or (!= (to-i soc-type) (get-config 'soc-type)) (!= (to-i cell-type) (get-config 'cell-type))) {
        (apply-battery-config (get-config 'soc-type) (get-config 'cell-type))
    })

    ; LED loop: reinit in place when running, spawn/stop on enable change
    (if (and (>= led-context-id 0) (!= (get-config 'led-enabled) 1)) {
        (var start-time (systime))
        (setq led-exit-flag t)
        (loopwhile (and led-exit-flag (< (- (systime) start-time) 2000000))
            (yield 10000))
        (if led-exit-flag (send-msg "WARNING: LED loop did not exit in time."))
        (setq led-context-id -1)
    })
    (if (and (>= led-context-id 0) (= (get-config 'led-enabled) 1)) {
        (setq led-reinit-flag t)
    })
    (if (and (= led-context-id -1) (= (get-config 'led-enabled) 1)) {
        (setq led-context-id (spawn led-loop))
    })

    ; BMS loop: restart to pick up new pins/settings
    (if (>= bms-context-id 0) {
        (var start-time (systime))
        (setq bms-exit-flag t)
        (loopwhile (and bms-exit-flag (< (- (systime) start-time) 2000000))
            (yield 10000))
        (if bms-exit-flag (send-msg "WARNING: BMS loop did not exit in time."))
    })
    (setq bms-context-id (if (= (get-config 'bms-enabled) 1) (spawn bms-loop) -1))

    ; Humidity loop
    (if (and (>= humidity-context-id 0) (!= (get-config 'humidity-enabled) 1)) {
        (var start-time (systime))
        (setq humidity-exit-flag t)
        (loopwhile (and humidity-exit-flag (< (- (systime) start-time) 2000000))
            (yield 10000))
        (if humidity-exit-flag (send-msg "WARNING: Humidity loop did not exit in time."))
        (setq humidity-context-id -1)
    })
    (if (and (= humidity-context-id -1) (= (get-config 'humidity-enabled) 1)) {
        (setq humidity-context-id (spawn humidity-loop))
    })

    ; Pubmote loop
    (if (and (>= pubmote-context-id 0) (!= (get-config 'pubmote-enabled) 1)) {
        (var start-time (systime))
        (setq pubmote-exit-flag t)
        (loopwhile (and pubmote-exit-flag (< (- (systime) start-time) 2000000))
            (yield 10000))
        (if pubmote-exit-flag (send-msg "WARNING: Pubmote loop did not exit in time."))
        (setq pubmote-context-id -1)
    })
    (if (and (= pubmote-context-id -1) (= (get-config 'pubmote-enabled) 1)) {
        (setq pubmote-context-id (spawn pubmote-loop))
    })

    ; Logging
    (if (= (get-config 'log-enabled) 1) {
        (if (= log-context-id -1) {
            (setq log-context-id (spawn log-loop))
        }{
            (start-log (get-config 'log-append-gnss) (get-config 'log-rate))
        })
    }{
        (stop-log)
    })
})

; Poll for config writes from VESC Tool and apply them.
(defun config-watch-loop ()
    (loopwhile t {
        (if (ext-facfg-changed) {
            (send-status "Settings updated")
            (apply-config)
        })
        (sleep 0.5)
    })
)

; Quick controls from the QML page (brightness / on-off), persisted.
(defun recv-control (in-led-on in-led-highbeam-on in-led-brightness in-led-brightness-highbeam in-led-brightness-idle in-led-brightness-status in-bms-charge-state) {
    (setq led-on (to-i in-led-on))
    (setq led-highbeam-on (to-i in-led-highbeam-on))
    (setq led-brightness (to-float in-led-brightness))
    (setq led-brightness-highbeam (to-float in-led-brightness-highbeam))
    (setq led-brightness-idle (to-float in-led-brightness-idle))
    (setq led-brightness-status (to-float in-led-brightness-status))

    (set-config 'led-on (to-i in-led-on))
    (set-config 'led-highbeam-on (to-i in-led-highbeam-on))
    (set-config 'led-brightness (to-float in-led-brightness))
    (set-config 'led-brightness-highbeam (to-float in-led-brightness-highbeam))
    (set-config 'led-brightness-idle (to-float in-led-brightness-idle))
    (set-config 'led-brightness-status (to-float in-led-brightness-status))
    (ext-facfg-store)

    (if (and (= (get-config 'bms-enabled) 1) (> bms-type 1) (!= bms-charge-state in-bms-charge-state) ) {
        (setq bms-charge-state (if (= bms-charge-state 1) 1 0))
        (setq bms-user-cmd 0x64)
    })
})

(defun bms-trigger-factory-init () {
    (if (and (= (get-config 'bms-enabled) 1) (> bms-type 1) (= bms-rs485-chip 1) ) {
        (setq bms-user-cmd 0x0e)
    })
})

(defun send-control () {
    (var config-string "control ")

    (setq config-string (
        str-merge
        config-string
        (str-from-n (to-i led-on) "%d ")
        (str-from-n (to-i led-highbeam-on) "%d ")
        (str-from-n led-brightness "%.2f ")
        (str-from-n led-brightness-highbeam "%.2f ")
        (str-from-n led-brightness-idle "%.2f ")
        (str-from-n led-brightness-status "%.2f ")
        (str-from-n (to-i bms-charge-state) "%d ")
    ))

    (send-data config-string)
})

(defun send-keys (key-list counter-list) {
    (print "Received key: ")
    (print key-list)
    (setq key-list (split-list key-list 4))
    (set-config 'bms-key-a (pack-bytes-to-uint32 (ix key-list 0)))
    (set-config 'bms-key-b (pack-bytes-to-uint32 (ix key-list 1)))
    (set-config 'bms-key-c (pack-bytes-to-uint32 (ix key-list 2)))
    (set-config 'bms-key-d (pack-bytes-to-uint32 (ix key-list 3)))
    (print "Received counter: ")
    (print counter-list)
    (setq counter-list (split-list counter-list 4))
    (set-config 'bms-counter-a (pack-bytes-to-uint32 (ix counter-list 0)))
    (set-config 'bms-counter-b (pack-bytes-to-uint32 (ix counter-list 1)))
    (set-config 'bms-counter-c (pack-bytes-to-uint32 (ix counter-list 2)))
    (set-config 'bms-counter-d (pack-bytes-to-uint32 (ix counter-list 3)))
    (save-config)
})

(defun accept-tos() {
    (set-config 'accept-tos 1)
    (ext-facfg-store)
})

(defun status () {
    (var status-string "float-stats ")
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since can-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (is-pubmote-connected) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (< (secs-since bms-last-activity-time) 1) 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-status "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-battery-type "%d ")))
    (setq status-string (str-merge status-string (str-from-n bms-battery-cycles "%d ")))
    (setq status-string (str-merge status-string (str-from-n (if (> (conf-get 'wifi-mode) 0) (wifi-get-chan) -1) "%d ")))
    (setq status-string (str-merge status-string (str-from-n hum "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n hum-temp "%.2f ")))
    (setq status-string (str-merge status-string (str-from-n (get-bms-val 'bms-hum) "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n (get-bms-val 'bms-temp-hum) "%.0f ")))
    (setq status-string (str-merge status-string (str-from-n (if log-running 1 0) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (to-i (get-config 'accept-tos)) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (to-i led-on) "%d ")))
    (setq status-string (str-merge status-string (str-from-n (to-i led-highbeam-on) "%d ")))
    (setq status-string (str-merge status-string (str-from-n led-brightness "%.2f ")))
    (send-data status-string)

    (if (= (is-pubmote-connected) 1) {
        (send-data (str-merge "pubmote-info " (to-str (ix pubmote-version 0)) "." (to-str (ix pubmote-version 1)) "." (to-str (ix pubmote-version 2))))
    })
})

(defun input-state () {
    (var input-string "input-state ")
    (setq input-string (str-merge input-string (str-from-n (is-pubmote-connected) "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-jsy "%.3f ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-jsx "%.3f ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-bt-c "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-bt-z "%d ")))
    (setq input-string (str-merge input-string (str-from-n pubmote-last-is-rev "%d")))
    (send-data input-string)
})

@const-end
