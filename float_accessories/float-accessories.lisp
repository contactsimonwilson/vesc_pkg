; float-accessories.lisp
; Smart LED Control and Tilt Remote for VESC Express
; Version 0.1
; 4/7/2024
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools (avaspark), auden_builds (pubmote)
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Koddex, Pickles

@const-symbol-strings

@const-start
; Debug flag for testing
(def debug 1)

; Settings version
(def config-version 463i32)
; Define an alist to store the variable values
(def config-alist '())
; Persistent settings
; Format: (label . (offset type default-value))
(def eeprom-addrs '(
    (ver-code                  . (0  i config-version))
    ; Features enabled
    (led-enable                . (1  b 1))   ; Basic version works on previous VESC firmware
    (bms-enable                . (2  b 0))   ; Stock OneWheel BMS to Smart VESC BMS.
    (pubmote-enable            . (3  b 1))   ; VESC 6.5 beta only. Also requires code-server
    ; VESC configuration
    (can-id                    . (4  i 98))  ; if can-id < 0 then it will scan for one and pick the first. Takes awhile on startup
    (cells-series              . (5  i 18))  ;Dynamic if code-server-enable
    (led-highbeam-on           . (6  b 1))
    (led-type                  . (7  i 3))
    (led-mode                  . (8  i 0))
    (led-mode-idle             . (9  i 5))
    (led-mode-status           . (10 i 0))
    (led-mode-startup          . (11 i 0))
    (led-mall-grab-enabled     . (12 b 0))
    (led-brake-light-enabled   . (13 b 0))
    (idle-timeout              . (14 i 5))
    (idle-timeout-shutoff      . (15 i 120))
    (led-brightness            . (16 f 0.5))
    (led-brightness-idle       . (17 f 0.2))
    (led-brightness-status     . (18 f 0.5))
    (led-status-pin            . (17 i 17))
    (led-status-num            . (20 i 10))
    (led-status-type           . (21 i 2))
    (led-status-reversed       . (22 b 0))
    (led-front-pin             . (23 i 18))
    (led-front-num             . (24 i 19))
    (led-front-type            . (25 i 0))
    (led-front-reversed        . (26 b 0))
    (led-front-has-laserbeam   . (27 b 1))
    (led-rear-pin              . (28 i 17))
    (led-rear-num              . (29 i 12))
    (led-rear-type             . (30 i 2))
    (led-rear-reversed         . (31 b 0))
    (led-rear-has-laserbeam    . (32 b 0))
    (bms-rs485-a-pin           . (33 i -1))
    (bms-wakeup-pin            . (34 i -1))
    (esp-now-remote-mac-a      . (35 i 132))
    (esp-now-remote-mac-b      . (36 i 252))
    (esp-now-remote-mac-c      . (37 i 230))
    (esp-now-remote-mac-d      . (38 i 80))
    (esp-now-remote-mac-e      . (39 i 168))
    (esp-now-remote-mac-f      . (40 i 56))
))
;GLOBAL_VARS_DONT_TOUCH
(def led-on 1)
(def data-rx-enable 0) ; VESC 6.5 beta only. Needed for communication to float package for advanced LED support and pubmote telemetry.
(def fault-code -1)
(def pitch-angle -1)
(def roll-angle -1)
(def state -1)
(def switch-state -1)
(def rpm -1)
(def input-voltage-filtered -1)
(def speed -1)
(def tot-current -1)
(def duty-cycle-now -1)
(def distance-abs -1)
(def fet-temp-filtered -1)
(def motor-temp-filtered -1)
(def odometer -1)
(def battery-level -1)

(def BLACK 0x00)
(def WHITE 0xFF)
(def idle-rpm 10.0)
(def led-status-duty-rpm 250.0)
(def led-delay 0.1);delay between LED stuff. TODO: Maybe use timer instead and sleep dynamically instead of being tied to clock?
(def pubmote-delay 1)
;Initialize the LED strips
(defun init-leds () {
    (if (>= led-status-pin 0){
        (def led-status-color (mklist led-status-num 0))
        (def led-status-buffer (rgbled-buffer led-status-num led-status-type))
    })
    (if (>= led-front-pin 0){
        (def led-front-color (mklist led-front-num 0))
        (def led-front-buffer (rgbled-buffer led-front-num led-front-type))
    })
    (if (>= led-rear-pin 0){
        (def led-rear-color (mklist led-rear-num 0))
        (def led-rear-buffer (rgbled-buffer led-rear-num led-rear-type))
    })
})

;Update the status LED bar
(defun update-status-leds () {
    (if (> rpm led-status-duty-rpm){
        (duty-cycle-pattern)
    }{;else
        (if (and (!= switch-state 1) (!= switch-state 2) (!= switch-state 3)){
            (battery-pattern led-status-color)
        }{;else
            (footpad-pattern switch-state)
        })
    })
})

;Update the status LED bar based on duty cycle
(defun duty-cycle-pattern () {
    (var scaled-duty-cycle (* (abs duty-cycle-now) 1.1112))
    (var clamped-duty-cycle 0.0)

    (if (< scaled-duty-cycle 1.0) {
        (setq clamped-duty-cycle scaled-duty-cycle)
    } {;else
        (setq clamped-duty-cycle 1.0)
    })

    (var duty-leds (floor (* clamped-duty-cycle led-status-num)))
    (var duty-color 0x00FFFF00u32)

    (if (> (abs duty-cycle-now) 0.85) {
        (setq duty-color 0x00FF0000u32)
    } {;else if
        (if (> (abs duty-cycle-now) 0.7) {
            (setq duty-color 0x00FF8800u32)
        })
    })

    (looprange led-index 0 led-status-num {
        (setix led-status-color led-index (if (< led-index duty-leds) duty-color 0x00000000u32))
    })
})

;Update the status LED bar based footpad
(defun footpad-pattern (switch-state){
    (var color-status-half1 (if (or (= switch-state 1) (= switch-state 3)) 0xFF BLACK))
    (var color-status-half2 (if (or (= switch-state 2) (= switch-state 3)) 0xFF BLACK))
    
    (looprange led-index 0 led-status-num {
        (setix led-status-color led-index (if (< led-index (/ led-status-num 2)) color-status-half1 color-status-half2))
    })
})


; Battery LED strip based on the current voltage
(defun battery-pattern (color-list) {
    (var led-num (length color-list))
    (var vin-low 3.0)
    (var vin-high 4.2)
    (var voltage-range (- (* cells-series vin-high) (* cells-series vin-low)))
    (var voltage-per-led (/ voltage-range led-num))
    (var num-lit-leds (floor (* led-num (/ (- input-voltage-filtered vin-low) voltage-range))))
    (var percent-remaining (/ (- input-voltage-filtered vin-low) voltage-range))

    (looprange led-index 0 led-num {
        (var red 0)
        (var green 0)

        (if (or (< led-index num-lit-leds) (and (= led-index 0) (<= num-lit-leds 1))) {
            (if (or (< percent-remaining 0.2) (and (= led-index 0) (<= num-lit-leds 1))) {
                (setq red 255)
                (setq green 0)
            } {;else
                (setq red (floor (* 255 (- 1 (/ percent-remaining 0.8)))))
                (setq green (floor (* 255 (/ percent-remaining 0.8))))
            })
        } {;else
            (setq red 0)
            (setq green 0)
        })
        (var color BLACK)
        (setq color (bits-enc-int color 16 red 8))
        (setq color (bits-enc-int color 8 green 8))
        (setix color-list led-index color)
    })
})

; Update the rainbow LED effect on the front and rear LED strips
(def rainbow-index 0)
(defun rainbow-pattern () {
    (var num-colors 6)
    (looprange led-index 0 led-front-num {
        (var color-index (mod (+ rainbow-index led-index led-status-num) num-colors))
        (var color (bufget-u32 rainbow-colors (* color-index 4)))
        (if (and (= led-index 0) (= led-front-has-laserbeam 1)) {
            (setq color BLACK)
        })
        (setix led-front-color led-index color)
    })
    (looprange led-index 0 led-rear-num {
        (var color-index (mod (+ rainbow-index led-index led-status-num led-front-num) num-colors))
        (var color (bufget-u32 rainbow-colors (* color-index 4)))
        (if (and (= led-index 0) (= led-rear-has-laserbeam 1)) {
            (setq color BLACK)
        })
        (setix led-rear-color led-index color)
    })
    (setq rainbow-index (mod (+ rainbow-index 1) num-colors))
})

; Clear all LED strips
(defun clear-leds () {
    (if (>= led-status-pin 0) {
        (looprange led-index 0 led-status-num {
            (setix led-status-color led-index BLACK)
        })      
    })
    (if (>= led-front-pin 0) {
        (looprange led-index 0 led-front-num {
            (setix led-front-color led-index BLACK)
        })
    })
    (if (>= led-rear-pin 0) {
        (looprange led-index 0 led-rear-num {
            (setix led-rear-color led-index BLACK)
        })
    })
})



(defun swap-rg (color-list) {
    (looprange led-index 0 (length color-list) {
        (var color (color-split (ix color-list led-index) 1))
        (var new-color (color-make (ix color 1) (ix color 0) (ix color 2) (ix color 3)))
        (setix color-list led-index new-color)
    })
})

; Split the LED buffer based on pin configuration and update the physical LEDs
(defun led-update () {
    (if (>= led-status-pin 0) {
        (if (= led-status-reversed 1) {
            (setq led-status-color (reverse led-status-color))
        })
    })
    (if (>= led-front-pin 0) {
        (if (= led-front-reversed 1) {
            (setq led-front-color (reverse led-front-color))
        })
    })
    (if (>= led-rear-pin 0) {
        (if (= led-rear-reversed 1) {
            (setq led-rear-color (reverse led-rear-color))
        })
    })
    (if (and (>= led-front-pin 0) (= led-status-pin led-front-pin) (= led-front-pin led-rear-pin)) {
        ; All LED strips are chained on the same pin
        (var led-combined-color (append led-status-color led-front-color led-rear-color))
        (var total-leds (length led-combined-color))
        (var led-combined-buffer (rgbled-buffer total-leds led-status-type))
        (rgbled-color led-combined-buffer 0 led-combined-color)
        (rgbled-init led-status-pin led-status-type)
        (sleep 0.01)
        (rgbled-update led-combined-buffer)
        (sleep 0.01)
    } {
        ;LED front/back are on same pin
        (if (and (>= led-front-pin 0) (= led-front-pin led-rear-pin)) {
            (var led-combined-color (append led-front-color led-rear-color))
            (var total-leds (length led-combined-color))
            (var led-combined-buffer (rgbled-buffer total-leds led-front-type))
            (rgbled-color led-combined-buffer 0 led-combined-color)
            (rgbled-init led-front-pin)
            (sleep 0.01)
            (rgbled-update led-combined-buffer)
            (sleep 0.01)        
        }{
            (if (and (>= led-status-pin 0) (= led-status-pin led-rear-pin)) {
                (if (and (= led-status-type 2) (!= led-status-type led-rear-type))
                    (swap-rg led-status-color); Fix for avaspark rgb when there's different types. e.g. stock GT RGBW
                )
                (var led-combined-color (append led-status-color led-rear-color))
                (var total-leds (length led-combined-color))
                (var led-combined-buffer (rgbled-buffer total-leds led-rear-type))
                (rgbled-color led-combined-buffer 0 led-combined-color) 
                (rgbled-init led-status-pin)
                (sleep 0.01)
                (rgbled-update led-combined-buffer)
                (sleep 0.01)
            }{
                ; LED strips are on separate pins
                (if (>= led-status-pin 0) {
                    (rgbled-color led-status-buffer 0 led-status-color)
                    (rgbled-init led-status-pin led-status-type)
                    (sleep 0.01)
                    (rgbled-update led-status-buffer)
                    (sleep 0.01)
                })
                (if (>= led-rear-pin 0) {
                    (rgbled-color led-rear-buffer 0 led-rear-color)
                    (rgbled-init led-rear-pin led-rear-type)
                    (sleep 0.01)
                    (rgbled-update led-rear-buffer)
                    (sleep 0.01)
                })
            })
            (if (>= led-front-pin 0) {
                (rgbled-color led-front-buffer 0 led-front-color)
                (rgbled-init led-front-pin led-front-type)
                (sleep 0.01)
                (rgbled-update led-front-buffer)
                (sleep 0.01)
            })
        })
    })
})
@const-end
; Retrieve VESC values from CAN bus
(defun get-vesc-values () {
    (setq rpm (canget-rpm can-id))
    (setq input-voltage-filtered (canget-vin can-id)) ;Get this from float package. Also set cells-series here
    (setq duty-cycle-now (canget-duty can-id))
    (var adc1 (canget-adc can-id 1))
    (var adc2 (canget-adc can-id 2))
    (setq switch-state 0)
    (if (> adc1 2.5) 
        (setq switch-state 1)
    )
    (if (> adc2 2.5)
        (setq switch-state 2)
    )
    (if (and (> adc1 2.5) (> adc2 2.5))
        (setq switch-state 3)
    )
})

(defunret init-can (can-id) { ;might have race condition with xlite D:
    (if (< can-id 0) {
        (var can-devices (can-scan))
        (loopwhile (<= (length can-devices) 0) {
            (setq can-devices (can-scan))
        })
        (setq can-id (first (can-scan)))
    })
    (return can-id)
})

(def FLOAT_MAGIC 101) ; Magic number used by float
(def FLOAT_ACCESSORIES_MAGIC 102)

; Float commands. Here we are using an association list, but there
; are many other ways to do the same thing.
(def float-cmds '(
        (FLOAT_COMMAND_LIGHT_INFO . 25)
        (FLOAT_COMMAND_GET_ALLDATA . 10)
))

;TODO
(def float-accessories-cmds '(
        (FLOAT_ACCESSORIES_COMMAND_SEND_SETTINGS . 0)
        (FLOAT_ACCESSORIES_COMMAND_RESTORE_SETTINGS . 1)
        (FLOAT_ACCESSORIES_COMMAND_SAVE_SETTINGS . 2)
))

(defun float-cmd (can-id cmd) {
        (send-data (append (list FLOAT_MAGIC) cmd) 2 can-id)
})


;BMS Stuff
(defun bms-wake-up (last-activity-sec bms-timeout) {
    (if (>= bms-wakeup-pin 0){
        (if (> last-activity-sec bms-timeout){
            (gpio-write bms-wakeup-pin 1)
            (sleep 0.1)
            (gpio-write bms-wakeup-pin 0)
        })
    })
})

(defun init-bms () {
    (if (>= bms-wakeup-pin 0){
        (gpio-configure bms-wakeup-pin 'pin-mode-out)
    })
    (if (>= bms-rs485-a-pin 0){
        (gpio-configure bms-rs485-a-pin 'pin-mode-in-pu)
        (uart-start 1 bms-rs485-a-pin -1 115200);If GNSS is connected UART 1 must be used
        (set-bms-val 'bms-cell-num 15)
        (set-bms-val 'bms-temp-adc-num 4) ; Temperature sensor count
        (set-bms-val 'bms-temp-cell-max 45)
    })
})

; Helper functions to get and set variable values
(defunret get-config (name) {
    (var pair (assoc config-alist name))
    (if pair
        (return pair)
        (return nil)
    )
})

(defun set-config (name value) {
    (let ((pair (assoc config-alist name)))
        (if pair
            (setassoc config-alist name value)
            (setq config-alist (cons (cons name value) config-alist))
        )
    )
})

(defun save-config () {
        (loopforeach setting eeprom-addrs {
            (write-val-eeprom (first setting) (get-config (first setting)))
        })
        (send-data "Settings Saved!")
})

(defun load-config () {
    (setq config-alist nil)
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var val (read-val-eeprom name))
        (setq config-alist (cons (cons name val) config-alist))
    })
})

(defun restore-config () {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var default-value (if (eq name 'ver-code) config-version (ix setting 3)))
        (write-val-eeprom name default-value)
    })
})

(defun print-config ()
    (loopforeach it eeprom-addrs
        (print (list (first it) (read-val-eeprom (first it))))
))

(defun read-val-eeprom (name)
    (let (
            (addr (first (assoc eeprom-addrs name)))
            (type (second (assoc eeprom-addrs name)))
        )
        (cond
            ((eq type 'i) (eeprom-read-i addr))
            ((eq type 'f) (eeprom-read-f addr))
            ((eq type 'b) (eeprom-read-i addr))
)))

(defun write-val-eeprom (name val)
    (let (
            (addr (first (assoc eeprom-addrs name)))
            (type (second (assoc eeprom-addrs name)))
        )
        (cond
            ((eq type 'i) (eeprom-store-i addr val))
            ((eq type 'f) (eeprom-store-f addr val))
            ((eq type 'b) (eeprom-store-i addr val))
)))

(defun send-msg (text)
    (send-data (str-merge "msg " text))
)

(defun mklist (len val) (map (fn (x) val) (range len)))


; Main
(defun main () {
    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })
    ; Restore settings if version number does not match
    ; as that probably means something else is in eeprom
    (if (not-eq (read-val-eeprom 'ver-code) config-version) (restore-config))
    (load-config)
    
    ;TODO: Read in all settings
    (def led-enable (get-config 'led-enable))
    (def bms-enable (get-config 'bms-enable))
    (def pubmote-enable (get-config 'pubmote-enable))
    (def can-id (init-can (get-config 'can-id)))
    (def esp-now-remote-mac (list (get-config 'esp-now-remote-mac-a) (get-config 'esp-now-remote-mac-b) (get-config 'esp-now-remote-mac-c) (get-config 'esp-now-remote-mac-d) (get-config 'esp-now-remote-mac-e) (get-config 'esp-now-remote-mac-f)))
    (def cells-series (get-config 'cells-series)) 
    ; LED Configuration
    (def led-highbeam-on (get-config 'led-highbeam-on)) 
    (def led-type (get-config 'led-type)) 
    (def led-mode (get-config 'led-mode)) 
    (def led-mode-idle (get-config 'led-mode-idle)) 
    (def led-mode-status (get-config 'led-mode-status)) 
    (def led-mode-startup (get-config 'led-mode-startup)) ;todo
    (def led-mall-grab-enabled (get-config 'led-mall-grab-enabled)) 
    (def led-brake-light-enabled (get-config 'led-brake-light-enabled)) ; TODO: "brake light" detection when current <-4.0A (regen)
    (def idle-timeout (get-config 'idle-timeout)) 
    (def idle-timeout-shutoff (get-config 'idle-timeout-shutoff)) 
    ; LED brightness
    (def led-brightness (get-config 'led-brightness)) 
    (def led-brightness-idle (get-config 'led-brightness-idle)) 
    (def led-brightness-status (get-config 'led-brightness-status)) 
    ; LED status configuration
    (def led-status-pin (get-config 'led-status-pin)) 
    (def led-status-num (get-config 'led-status-num)) 
    (def led-status-type (get-config 'led-status-type)) 
    (def led-status-reversed (get-config 'led-status-reversed)) 
    ; LED front configuration
    (def led-front-pin (get-config 'led-front-pin)) 
    (def led-front-num (get-config 'led-front-num)) 
    (def led-front-type (get-config 'led-front-type)) 
    (def led-front-reversed (get-config 'led-front-reversed)) 
    (def led-front-has-laserbeam (get-config 'led-front-has-laserbeam)) 
    ; LED rear configuration
    (def led-rear-pin (get-config 'led-rear-pin)) 
    (def led-rear-num (get-config 'led-rear-num)) 
    (def led-rear-type (get-config 'led-rear-type)) 
    (def led-rear-reversed (get-config 'led-rear-reversed)) 
    (def led-rear-has-laserbeam (get-config 'led-rear-has-laserbeam)) 
    ; BMS configuration
    (def bms-rs485-a-pin (get-config 'bms-rs485-a-pin)) ; white wire on stock pint bms
    (def bms-wakeup-pin (get-config 'bms-wakeup-pin)) ; gonna be used to wake up bms if no data in charge only setup.
    
    ; Spawn the event handler thread and pass the ID it returns to C
    (event-register-handler (spawn event-handler))
    (event-enable 'event-data-rx)
    
    ;check if features are supported
    (def fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
    ;todo better error handeling
    (if (>= fw-num 6.05) {
        (import "pkg@://vesc_packages/lib_code_server/code_server.vescpkg" 'code-server)
        (read-eval-program code-server)
        (setq data-rx-enable (get-vesc-config))
        (if (and (= pubmote-enable 1) (= data-rx-enable 1)) {
            (init-pubmote)
            (pubmote-loop)
        })
    })
    (if (= bms-enable 1){
        (init-bms)
        (if (>= bms-rs485-a-pin 0){
            (bms-loop)
        })
    })
    (if (= led-enable 1){
        (init-leds)
        (led-loop)
    })
})
(defun init-pubmote () {
    (esp-now-start)
    (wifi-set-chan 1)
    (esp-now-add-peer esp-now-remote-mac) ;
    ;(print (list "starting" (get-mac-addr) (wifi-get-chan)))
    ;(print esp-now-remote-mac)
    (event-enable 'event-esp-now-rx)
})

; Update the front and rear LED strips based on the current direction
(defun update-direction-leds (direction led-forward-color led-backward-color) {
    (var rear-color (if (< direction 0) led-forward-color led-backward-color))
    (var front-color (if (< direction 0) led-backward-color led-forward-color))
    (var front-color-laserbeam (if (< direction 0) BLACK WHITE))
    (var rear-color-laserbeam (if (< direction 0) WHITE BLACK))
    (if (= led-front-has-laserbeam 1) {
        (setix led-front-color 0 (if (= led-highbeam-on 1) front-color-laserbeam BLACK))
    })
    (looprange led-index led-front-has-laserbeam (- led-front-num led-front-has-laserbeam) {
        (setix led-front-color led-index front-color)
    })
    (if (= led-rear-has-laserbeam 1) {
        (setix led-rear-color 0 (if (= led-highbeam-on 1) rear-color-laserbeam BLACK))
    })
    (looprange led-index led-rear-has-laserbeam (- led-rear-num led-rear-has-laserbeam) {
        (setix led-rear-color led-index rear-color)
    })
})
;Pubmote loop
(defun pubmote-loop () {
    (loopwhile-thd 50 t {
        (if (= led-enable 0){
            (float-cmd can-id (list (assoc float-cmds 'FLOAT_COMMAND_GET_ALLDATA) 3))
        })
        (var data (bufcreate 28))
        (bufset-u8 data 0 69) ; Mode
        (bufset-u8 data 1 fault-code)
        (bufset-i16 data 2 (floor (* pitch-angle 10))) ; Store pitch-angle with 1 decimal place precision
        (bufset-i16 data 4 (floor (* roll-angle 10))) ; Store roll-angle with 1 decimal place precision
        (bufset-u8 data 6 state)
        (bufset-u8 data 7 switch-state)
        (bufset-i16 data 8 (floor (* input-voltage-filtered 10))) ; Store input-voltage-filtered with 1 decimal place precision
        (bufset-i16 data 10 (floor rpm))
        (bufset-i16 data 12 (floor (* speed 10))) ; Store speed with 1 decimal place precision
        (bufset-i16 data 14 (floor (* tot-current 10))) ; Store tot-current with 1 decimal place precision
        (bufset-u8 data 16 (floor (* (+ duty-cycle-now 0.5) 100))) ; Store duty-cycle-now as percentage (0-100)
        (bufset-f32 data 17 distance-abs 'little-endian)
        (bufset-u8 data 21 (floor (* fet-temp-filtered 2))) ; Store fet-temp-filtered with half-precision
        (bufset-u8 data 22 (floor (* motor-temp-filtered 2))) ; Store motor-temp-filtered with half-precision
        (bufset-u32 data 23 odometer)
        (bufset-u8 data 27 (floor (* battery-level 2))) ; Store battery-level with half-precision
        (esp-now-send esp-now-remote-mac data)
        ;TODO add delay
        (sleep pubmote-delay)
        ;battery, duty, speed, voltage, footpads, motor and controler temp, trip, remaining miles.
        ;(print "Responded")
        (free data)
    })
})

(defunret get-vesc-config () {
    (var ret (rcode-run can-id 0.5 '(conf-get 'si-battery-cells)))
    (if (eq ret 'timeout) {
        (return 0)
    })
    (setq cells-series ret)
    (return 1)
})

(defun send-config () {
  (var config-string "settings ")
  (loopforeach setting eeprom-addrs {
    (let ((name (first setting))
          (type (third setting))) {
      (var value (read-val-eeprom name))
      (setq config-string (str-merge config-string
        (cond
          ((eq type 'b) (str-from-n value "%d "))
          ((eq type 'i) (str-from-n value "%d "))
          ((eq type 'f) (str-from-n value "%.2f ")))))
    })
  })
  (send-data config-string)
})

(defun recv-config (led-enable bms-enable pubmote-enable can-id cells-series led-highbeam-on led-type led-mode led-mode-idle led-mode-status led-mode-startup led-mall-grab-enabled led-brake-light-enabled idle-timeout idle-timeout-shutoff led-brightness led-brightness-idle led-brightness-status led-status-pin led-status-num led-status-type led-status-reversed led-front-pin led-front-num led-front-type led-front-reversed led-front-has-laserbeam led-rear-pin led-rear-num led-rear-type led-rear-reversed led-rear-has-laserbeam bms-rs485-a-pin bms-wakeup-pin esp-now-remote-mac-a esp-now-remote-mac-b esp-now-remote-mac-c esp-now-remote-mac-d esp-now-remote-mac-e esp-now-remote-mac-f) {
    (set-config 'led-enable (to-i led-enable))
    (set-config 'bms-enable (to-i bms-enable))
    (set-config 'pubmote-enable (to-i pubmote-enable))
    (set-config 'can-id (to-i can-id))
    (set-config 'cells-series (to-i cells-series))
    (set-config 'led-highbeam-on (to-i led-highbeam-on))
    (set-config 'led-type (to-i led-type))
    (set-config 'led-mode (to-i led-mode))
    (set-config 'led-mode-idle (to-i led-mode-idle))
    (set-config 'led-mode-status (to-i led-mode-status))
    (set-config 'led-mode-startup (to-i led-mode-startup))
    (set-config 'led-mall-grab-enabled (to-i led-mall-grab-enabled))
    (set-config 'led-brake-light-enabled (to-i led-brake-light-enabled))
    (set-config 'idle-timeout (to-i idle-timeout))
    (set-config 'idle-timeout-shutoff (to-i idle-timeout-shutoff))
    (set-config 'led-brightness (to-float led-brightness))
    (set-config 'led-brightness-idle (to-float led-brightness-idle))
    (set-config 'led-brightness-status (to-float led-brightness-status))
    (set-config 'led-status-pin (to-i led-status-pin))
    (set-config 'led-status-num (to-i led-status-num))
    (set-config 'led-status-type (to-i led-status-type))
    (set-config 'led-status-reversed (to-i led-status-reversed))
    (set-config 'led-front-pin (to-i led-front-pin))
    (set-config 'led-front-num (to-i led-front-num))
    (set-config 'led-front-type (to-i led-front-type))
    (set-config 'led-front-reversed (to-i led-front-reversed))
    (set-config 'led-front-has-laserbeam (to-i led-front-has-laserbeam))
    (set-config 'led-rear-pin (to-i led-rear-pin))
    (set-config 'led-rear-num (to-i led-rear-num))
    (set-config 'led-rear-type (to-i led-rear-type))
    (set-config 'led-rear-reversed (to-i led-rear-reversed))
    (set-config 'led-rear-has-laserbeam (to-i led-rear-has-laserbeam))
    (set-config 'bms-rs485-a-pin (to-i bms-rs485-a-pin))
    (set-config 'bms-wakeup-pin (to-i bms-wakeup-pin))
    (set-config 'esp-now-remote-mac-a (to-i esp-now-remote-mac-a))
    (set-config 'esp-now-remote-mac-b (to-i esp-now-remote-mac-b))
    (set-config 'esp-now-remote-mac-c (to-i esp-now-remote-mac-c))
    (set-config 'esp-now-remote-mac-d (to-i esp-now-remote-mac-d))
    (set-config 'esp-now-remote-mac-e (to-i esp-now-remote-mac-e))
    (set-config 'esp-now-remote-mac-f (to-i esp-now-remote-mac-f))
})

(defun bms-loop () {
    (var bms-last-activity-time (systime))
    (var bms-timeout 5)
    (var bms-buf (bufcreate 64))
    (loopwhile-thd 100 t {
        (var res (uart-read bms-buf (buflen bms-buf) nil nil 0.5))
        (looprange i 0 (buflen bms-buf) { 
            (if (< i (- (- (buflen bms-buf) 3) 38)) {
                (if (and (= (bufget-u8 bms-buf i) 0xFF) (= (bufget-u8 bms-buf (+ i 1)) 0x55) (= (bufget-u8 bms-buf (+ i 2)) 0xAA)) {
                    (setq bms-last-activity-time (systime)) ; Update the last activity time for BMS
                    (looprange j (+ i 3) (- (buflen bms-buf) 3) {
                        (if (and (= (bufget-u8 bms-buf (+ j 1)) 0xFF) (= (bufget-u8 bms-buf (+ j 2)) 0x55) (= (bufget-u8 bms-buf (+ j 3)) 0xAA)) {
                            (var packet-type (bufget-u8 bms-buf (+ i 3)))
                            (var len (- (- j 1) i))
                            (var packet (bufcreate len))
                            (bufcpy packet 0 bms-buf i len)
                            (var crc (bufget-u16 bms-buf (- j 1)))
                            (var calc-crc 0)
                            (looprange k 0 (buflen packet) {
                                (setq calc-crc (+ calc-crc (bufget-u8 packet k)))
                            })
                            (if (eq crc calc-crc){
                                (if (= packet-type 2) {
                                    (var cell-index 0)
                                    (looprange k 4 (- (buflen packet) 3) {
                                        (if (eq (mod k 2) 0) {
                                            (var current-cell (/ (bufget-i16 packet k) 1000.0))
                                            (set-bms-val 'bms-v-cell cell-index current-cell)
                                            (setq cell-index (+ cell-index 1))
                                        })
                                    })
                                
                                })
                                (if (= packet-type 3) {
                                    ;TODO SoC support (and advanced support for non-FM batteries)
                                    ;(print "Reported SOC")
                                    ;(var soc (bufget-i8 packet 4))
                                    ;(print soc)
                                })
                                (if (= packet-type 5) {
                                    (var CURRENT_SCALER 0.055)
                                    (var current (* (bufget-i16 packet 4) CURRENT_SCALER))
                                    (set-bms-val 'bms-i-in-ic current)
                                })
                                (if (= packet-type 0) {
                                    ;Todo: Set BMS errors here
                                    (var status (bufget-i8 packet 4))
                                    (var isCharging (if (= (bitwise-and status 0x20) 0) 0 1))
                                    (var isBatteryEmpty (if (= (bitwise-and status 0x4) 0) 0 1))
                                    (var isBatteryTempOutOfRange (if (= (bitwise-and status 0x3) 0) 0 1))
                                    (var isBatteryOvercharged (if (= (bitwise-and status 0x8) 0) 0 1))
                                })
                                (if (= packet-type 4) {
                                    (var temp-index 0)
                                    (looprange k 4 (buflen packet) {
                                        (var temp (bufget-i8 packet k))
                                        ;(print temp)
                                        (if (= temp-index (- (buflen packet) 5)) (set-bms-val 'bms-temp-ic temp) (set-bms-val 'bms-temps-adc temp-index temp))
                                        (setq temp-index (+ temp-index 1))
                                    })
                                })
                            })
                            (break)
                        })
                    })
                })
            })
        })
        (send-bms-can)
        (bms-wake-up (secs-since bms-last-activity-time) bms-timeout)
    })
})


(defun proc-data (data) {
        ;(print (map (fn (x) (bufget-u8 data x)) (range (buflen data))))
        ;Support for saving config/code exec from qml
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_ACCESSORIES_MAGIC)) {
            (bufcpy data 0 data 1 (-(buflen data) 1))
            (eval (read data))
        })
        ; Only process data if data is long enough and magic number is correct
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {
                (match (cossa float-cmds (bufget-u8 data 1))
                    (FLOAT_COMMAND_LIGHT_INFO {
                            (setq led-type (bufget-u8 data 2))
                            (setq led-brightness (/ (bufget-u8 data 3) 100.0))
                            (setq led-brightness-idle (/ (bufget-u8 data 4) 100.0))
                            (setq led-brightness-status (/ (bufget-u8 data 5) 100.0))
                            (setq led-mode (bufget-u8 data 6))
                            (setq led-mode-idle (bufget-u8 data 7))
                            (setq led-mode-status (bufget-u8 data 8))
                            
                            ;(print (list "Light Info Received"
                            ;        (str-from-n led-type "led_type: %d")
                            ;        (str-from-n led-brightness "led_brightness: %d")
                            ;))
                    })
                    (FLOAT_COMMAND_GET_ALLDATA {
                        (var mode (bufget-u8 data 2))
                        (if (= mode 69) {
                            (setq fault-code (bufget-u8 data 3))
                        } { ;else
                            (setq pitch-angle (/ (to-float (bufget-i16 data 5)) 10))
                            ;(print pitch-angle)
                            (setq roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                            (setq state (bufget-u8 data 10))
                            (setq switch-state (bufget-u8 data 11))
                            (setq input-voltage-filtered (/ (to-float (bufget-i16 data 22)) 10))
                            (setq rpm (to-float(bufget-i16 data 24)))
                            (setq speed (/ (to-float (bufget-i16 data 26)) 10))
                            (setq tot-current (/ (to-float (bufget-i16 data 28)) 10))
                            (setq duty-cycle-now (- (/ (bufget-u8 data 30) 100.0) 0.5))
                            (if (>= mode 2) {
                                (setq distance-abs (bufget-f32 data 34))
                                (setq fet-temp-filtered (/ (bufget-u8 data 38) 2.0))
                                (setq motor-temp-filtered (/ (bufget-u8 data 39) 2.0))
                              
                            })
                            (if (>= mode 3) {
                                (setq odometer (bufget-u32 data 41)) ;meters
                                (setq battery-level (/ (bufget-u8 data 53) 2.0))
                            })
                        })
                    })
                    (_ nil) ; Ignore other commands
                )
        })
})
(defun event-handler ()
    (loopwhile t
        (recv
            ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (proc-esp-now-rx src des data rssi))
            ((event-data-rx . (? data)) (proc-data data))
            (_ nil)
        )
    )
)

; LED Loop
(defun led-loop () {
    (var led-last-activity-time (systime))
    (var direction 0)
    (var previous-direction 0)
    (var debug-val 1)
    (var debug-timer (systime))
    (var idle-timeout-shutoff-event 0)
    (var led-mall-grab 0)
    (loopwhile-thd 100 t {
        (if (= data-rx-enable 1) {
            ;TODO: https://github.com/vedderb/bldc/blob/master/lispBM/lispBM/doc/lbmref.md#message-passing
            (float-cmd can-id (list (assoc float-cmds 'FLOAT_COMMAND_LIGHT_INFO)))
            (float-cmd can-id (list (assoc float-cmds 'FLOAT_COMMAND_GET_ALLDATA) 3))
        } {
            (get-vesc-values)
        })
        (if (= led-type 3) (setq led-on 1) (setq led-on 0))
        (if (= led-on 1) {
            (setq direction 0)
            (var idle-rpm-darkride idle-rpm)
            (if (= state 4); RUNNING_UPSIDEDOWN 
                (setq idle-rpm-darkride (*idle-rpm-darkride -1))
            )
            (if (> rpm idle-rpm-darkride) {
                (setq direction 1)
            })
            (if (< rpm (* idle-rpm-darkride -1)) {
                (setq direction -1)
            })
            (if (= led-mall-grab-enabled 1){
                (if (> pitch-angle 70) (setq led-mall-grab 1) (setq led-mall-grab 0))
            })
            (if (= debug 1) {
                (setq direction 0)
                (setq idle-timeout 10)
                (setq idle-timeout-shutoff 20)
                (if (< (secs-since debug-timer) idle-timeout ) {
                    (setq direction debug-val)
                    (setq debug-val (* debug-val -1))
                })
            })
            (update-leds (secs-since led-last-activity-time) previous-direction direction idle-timeout-shutoff-event led-mall-grab)
            (setq previous-direction direction)
            (if (or (!= direction 0) (= led-mall-grab 1)) {
                (setq idle-timeout-shutoff-event 0)
                (setq led-last-activity-time (systime))
            })
            (if (< idle-timeout-shutoff-event 2) (led-update))
        } {
            (if (> led-on -2) {
                ;clear all LEDs twice if disabled
                (clear-leds)
                (led-update)
                (setq led-on (- led-on 1))
            })
        })
        (sleep led-delay)
    })
})


;Update all leds
(defunret update-leds (last-activity-sec previous-direction direction idle-timeout-shutoff-event led-mall-grab) {
    (if (< last-activity-sec idle-timeout-shutoff) {
        (if (>= led-status-pin 0){
            (if (= led-mode-status 0)
                (update-status-leds)
            )
        })
        
        (var current-led-mode led-mode)
        (var led-current-brightness led-brightness)
        (if (> last-activity-sec idle-timeout) {
            (setq current-led-mode led-mode-idle)
            (setq led-current-brightness led-brightness-idle)
        })
        (if (and (>= led-front-pin 0) (>= led-rear-pin 0)) {
            (var led-forward-color BLACK)
            (var led-backward-color BLACK)
            ;battery meter
            (if (or (= current-led-mode 1) (= led-mall-grab 1)){
                (battery-pattern led-front-color)
                (battery-pattern led-rear-color)
                (return)
            })
            ;red/white
            (if (= current-led-mode 0){
                (setq led-forward-color 0xFFFFFFFFu32)
                (setq led-backward-color 0x00FF0000u32)
                (if (or (and (> direction 0) (<= previous-direction 0)) (and (<= direction 0) (> previous-direction 0))) {
                    (update-direction-leds direction led-forward-color led-backward-color)
                })
                (return)
            })               
            ;cyan/magenta
            (if (= current-led-mode 2){
                (setq led-forward-color 0xFF00FFFFu32)
                (setq led-backward-color 0x00FF00FFu32)
                (update-direction-leds direction led-forward-color led-backward-color)
                (return)
            })
            ;blue/green
            (if (= current-led-mode 3){
                (setq led-forward-color 0xFF0000FFu32)
                (setq led-backward-color 0x0000FF00u32)
                (update-direction-leds direction led-forward-color led-backward-color)
                (return)
            })
            ;yellow/green
            (if (= current-led-mode 4){
                (setq led-forward-color 0xFFFFFF00u32)
                (setq led-backward-color 0x0000FF00u32)
                (update-direction-leds direction led-forward-color led-backward-color)
                (return)
            })
            ;rgb led
            (if (= current-led-mode 5){
                (rainbow-pattern)
                (return)
            })
            ;Handle ERROR?
            (if (> current-led-mode 5){
                (rainbow-pattern)
                (return)
            })
            ;TODO
            ;strobe
            (if (= current-led-mode 6){
                ;
                (return)
            })
            ;rave
            (if (= current-led-mode 7) {
                ;
                (return)
            })
            ;mullet
            (if (= current-led-mode 8) {
                ;
                (return)
            })
            ;knight rider
            (if (= current-led-mode 9) {
                ;
                (return)
            })
            ;felony
            (if (= current-led-mode 10) {
                ;
                (return)
            })
        })
    }
    {;else
        (if (< idle-timeout-shutoff-event 2) {
            (clear-leds)
            (setq idle-timeout-shutoff-event (+ idle-timeout-shutoff-event 1))
        })
    })
})

;Pubmote
(defun proc-esp-now-rx (src des data rssi) {
    ;(print (list "Received" src des data rssi))
    (var jsx (bufget-f32 data 0 'little-endian))
    (var jsy (bufget-f32 data 4 'little-endian))
    (var bt-c (bufget-u8 data 8))
    (var bt-z (bufget-u8 data 9))
    (var is-rev (bufget-u8 data 10))
    (rcode-run-noret can-id `(set-remote-state ,jsy ,jsx 0 0 0))
})

;CONSTANTS
(def rainbow-colors (array-create 24)) ; 6 colors * 4 bytes per color
(bufset-u32 rainbow-colors 0 0xFF0000u32)
(bufset-u32 rainbow-colors 4 0xFFFF00u32)
(bufset-u32 rainbow-colors 8 0x00FF00u32)
(bufset-u32 rainbow-colors 12 0x00FFFFu32)
(bufset-u32 rainbow-colors 16 0x0000FFu32)
(bufset-u32 rainbow-colors 20 0xFF00FFu32)

; Start the main
(main)