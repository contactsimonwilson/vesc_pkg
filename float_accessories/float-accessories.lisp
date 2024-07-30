; float-accessories.lisp
; Smart LED Control, Tilt Remote and stock OW BMS bridge for VESC Express
; Version 0.2
; 4/7/2024
; Copyright 2024 Syler Clayton <syler.clayton@gmail.com>
; Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools (avaspark), auden_builds (pubmote)
; gr33tz: outlandnish, exphat, datboig42069
; Beta Testers: Koddex, Pickles

@const-symbol-strings

;GLOBAL_VARS_DONT_TOUCH
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
(def led-current-brightness 1)
(def led-status-color '())
(def led-front-color '())
(def led-rear-color '())
;(def led-front-color-prev '())
(def battery-percent-remaining 0)
(def discover-can-id -1)
(def esp-now-remote-mac '())
(def pubmote-pairing-timer 31)
(def pubmote-last-activity-time 0)
(def bms-last-activity-time 0)
(def led-last-activity-time 0)
(def can-last-activity-time 0)

(def rainbow-colors '(0xFF0000u32 0xFFFF00u32 0x00FF00u32 0x00FFFFu32 0x0000FFu32 0xFF00FFu32))
(def rave-colors '(0x00FFFF00 0x0000FF00 0x0000FFFF 0x000000FF 0x00FF00FF 0x00FF0000))

; Settings version
(def config-version 422i32)
; Persistent settings

; Format: (label . (offset type default-value current-value))
(def eeprom-addrs '(
    (ver-code                  . (0  i config-version -1))
	(crc                       . (1 i 60967 -1))
    (can-id                    . (2  i -1 -1))  ; if can-id < 0 then it will scan for one and pick the first.
    (led-on                	   . (3  b 1 -1))
    (led-highbeam-on           . (4  b 1 -1))
    (led-mode                  . (5  i 0 -1))
    (led-mode-idle             . (6  i 5 -1))
    (led-mode-status           . (7 i 0 -1))
    (led-mode-startup          . (8 i 5 -1))
    (led-mall-grab-enabled     . (9 b 1 -1))
    (led-brake-light-enabled   . (10 b 1 -1))
	(led-brake-light-min-amps  . (11 f -4.0 -1))
    (idle-timeout              . (12 i 30 -1))
    (idle-timeout-shutoff      . (13 i 120 -1))
    (led-brightness            . (14 f 0.5 -1))
	(led-brightness-highbeam   . (15 f 0.5 -1))
    (led-brightness-idle       . (16 f 0.2 -1))
    (led-brightness-status     . (17 f 0.5 -1))
    (led-status-pin            . (18 i -1 -1))
    (led-status-num            . (19 i 10 -1))
    (led-status-type           . (20 i 2 -1))
    (led-status-reversed       . (21 b 0 -1))
    (led-front-pin             . (22 i 18 -1))
    (led-front-num             . (23 i 13 -1))
    (led-front-type            . (24 i 2 -1))
    (led-front-reversed        . (25 b 0 -1))
    (led-front-strip-type      . (26 b 1 -1))
    (led-rear-pin              . (27 i 17 -1))
    (led-rear-num              . (28 i 13 -1))
    (led-rear-type             . (29 i 2 -1))
    (led-rear-reversed         . (30 b 0 -1))
    (led-rear-strip-type       . (31 b 1 -1))
    (bms-rs485-a-pin           . (32 i -1 -1))
    (bms-wakeup-pin            . (33 i -1 -1))
    (bms-override-soc          . (34 i 1 -1))
    (esp-now-remote-mac-a      . (35 i -1 -1))
    (esp-now-remote-mac-b      . (36 i -1 -1))
    (esp-now-remote-mac-c      . (37 i -1 -1))
    (esp-now-remote-mac-d      . (38 i -1 -1))
    (esp-now-remote-mac-e      . (39 i -1 -1))
    (esp-now-remote-mac-f      . (40 i -1 -1))
	(esp-now-secret-code       . (41 i -1 -1))
))
@const-start
(def strobe-index 0)
(defun strobe-pattern () {
    (setq strobe-index (mod (+ strobe-index 1) 2))
    (var color (if (= strobe-index 0) 0xFFFFFFFF 0x00000000))
	
    (looprange i 0 (length led-front-color) {
        (setix led-front-color i color)
    })
    (looprange i 0 (length led-rear-color) {
        (setix led-rear-color i color)
    })
})

(def brake-index 0)
(defun brake-pattern () {
    (setq brake-index (mod (+ brake-index 1) 2))
    (looprange i 0 (length led-rear-color) {
        (setix led-rear-color i (if (= brake-index 0) 0x00FF0000 0x00000000))
    })
})

(def rave-index 0)
(defun rave-pattern (type){
        (var current-color (ix rave-colors rave-index))
        (looprange i 0 (length led-front-color)
            (setix led-front-color i (if (= type 0) current-color 0xFF000000)))
        (looprange i 0 (length led-rear-color)
            (setix led-rear-color i current-color))
        (setq rave-index (mod (+ rave-index 1) 6))
})
(def knight-rider-position 0)
(def knight-rider-direction 1)
(defun max (a b) (if (> a b) a b))
(defun min (a b) (if (< a b) a b))
  
(defun knight-rider-pattern (){
        (var total-leds (+ (length led-front-color) (length led-rear-color)))
        (looprange i 0 total-leds {
                (var distance (abs (- i knight-rider-position)))
                (var intensity (max 0 (- 255 (* distance 51))))
                (var color (color-make intensity 0 0))
                (if (< i (length led-front-color))
                    (setix led-front-color i color)
                    (setix led-rear-color (- i (length led-front-color)) color))
                    })
        (setq knight-rider-position (+ knight-rider-position knight-rider-direction))
        (if (or (>= knight-rider-position total-leds) (< knight-rider-position 0))
            (setq knight-rider-direction (* knight-rider-direction -1)))
})


; Battery LED strip based on the current voltage
(defun battery-pattern (color-list) {
    (var led-num (length color-list))
    (var num-lit-leds (floor (* led-num battery-percent-remaining)))

    (looprange led-index 0 led-num {
        (var red 0)
        (var green 0)
        (if (or (< led-index num-lit-leds) (and (= led-index 0) (<= num-lit-leds 1))) {
            (if (or (< battery-percent-remaining 0.2) (and (= led-index 0) (<= num-lit-leds 1))) {
                (setq red 255)
                (setq green 0)
            } {;else
                (setq red (floor (* 255 (- 1 (/ battery-percent-remaining 0.8)))))
                (setq green (floor (* 255 (/ battery-percent-remaining 0.8))))
            })
        } {;else
            (setq red 0)
            (setq green 0)
        })
        (var color 0x00)
        (setq color (bits-enc-int color 16 red 8))
        (setq color (bits-enc-int color 8 green 8))
        (setix color-list led-index color)
    })
})
; Update the rainbow LED effect on the front and rear LED strips
(def rainbow-index 0)
(defun rainbow-pattern () {
    (var num-colors (length rainbow-colors))
    (looprange led-index 0 (length led-front-color) {
        (var color-index (mod (+ rainbow-index led-index (length led-front-color)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix led-front-color led-index color)
    })
    (looprange led-index 0 (length led-rear-color) {
        (var color-index (mod (+ rainbow-index led-index (length led-rear-color)) num-colors))
        (var color (ix rainbow-colors color-index))
        (setix led-rear-color led-index color)
    })
    (setq rainbow-index (mod (+ rainbow-index 1) num-colors))
})

(def felony-index 0)
(defun felony-pattern () {
    (var felony-state (mod felony-index 3))
	(var led-front-num (length led-front-color))
	(var led-rear-num (length led-rear-color))
    (var forward-half (floor (/ led-front-num 2)))
    (var rear-half (floor (/ led-rear-num 2)))
    
    (cond
      ((= felony-state 0) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x00000000)) ; BLACK
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x00FF0000)) ; RED
       (looprange i 0 rear-half
         (setix led-rear-color i 0x00000000)) ; BLACK
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x00FF0000)) ; RED
      })
      
      ((= felony-state 1) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x00FF0000)) ; RED
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x000000FF)) ; BLUE
       (looprange i 0 rear-half
         (setix led-rear-color i 0x00FF0000)) ; RED
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x000000FF)) ; BLUE
      })
      
      ((= felony-state 2) {
       (looprange i 0 forward-half
         (setix led-front-color i 0x000000FF)) ; BLUE
       (looprange i forward-half led-front-num
         (setix led-front-color i 0x00000000)) ; BLACK
       (looprange i 0 rear-half
         (setix led-rear-color i 0x000000FF)) ; BLUE
       (looprange i rear-half led-rear-num
         (setix led-rear-color i 0x00000000)) ; BLACK
      }))
    
    (setq felony-index (mod (+ felony-index 1) 3))
})
        
(defun update-status-leds () {
    (if (> rpm 250.0){
        (duty-cycle-pattern)
    }{;else
        (if (and (!= switch-state 1) (!= switch-state 2) (!= switch-state 3)){
            (battery-pattern led-status-color)
        }{;else
            (footpad-pattern switch-state)
        })
    })
})

(defun duty-cycle-pattern () {
	(var led-status-num (length led-status-color))
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

(defun footpad-pattern (switch-state){
	(var led-status-num (length led-status-color))
    (var color-status-half1 (if (or (= switch-state 1) (= switch-state 3)) 0xFF 0x00))
    (var color-status-half2 (if (or (= switch-state 2) (= switch-state 3)) 0xFF 0x00))
    (looprange led-index 0 led-status-num {
        (setix led-status-color led-index (if (< led-index (/ led-status-num 2)) color-status-half1 color-status-half2))
    })
})

(defun clear-leds () {
    (looprange led-index 0 (length led-status-color) {
		(setix led-status-color led-index 0x00)
    })      

    (looprange led-index 0 (length led-front-color) {
        (setix led-front-color led-index 0x00)
    })
	(looprange led-index 0 (length led-rear-color) {
        (setix led-rear-color led-index 0x00)
    })
})

(defun swap-rg (color-list) {
    (looprange led-index 0 (length color-list) {
        (var color (color-split (ix color-list led-index) 1))
        (var new-color (color-make (ix color 1) (ix color 0) (ix color 2) (ix color 3)))
        (setix color-list led-index new-color)
    })
})

;(defun number-in-list-p (number lst)
;  (if (eq lst nil)
;      nil
;    (if (eq number (car lst))
;        t
;      (number-in-list-p number (cdr lst)))))

(defun led-update (direction) {
    (if (= (get-config 'led-status-reversed) 1) {
        (setq led-status-color (reverse led-status-color))
    })
    (if (= (get-config 'led-front-reversed) 1) {
        (setq led-front-color (reverse led-front-color))
    })
    (if (= (get-config 'led-rear-reversed) 1) {
        (setq led-rear-color (reverse led-rear-color))
    })
	
	(var led-highbeam-on (get-config 'led-highbeam-on))
	(var led-brightness-highbeam (get-config 'led-brightness-highbeam))
	(var led-status-pin (get-config 'led-status-pin))
	(var led-front-pin (get-config 'led-front-pin))
	(var led-rear-pin (get-config 'led-rear-pin))
	(var led-front-strip-type (get-config 'led-front-strip-type) )
	(var led-rear-strip-type (get-config 'led-rear-strip-type) )
    (var front-color-highbeam (if (and (> direction 0) (= led-highbeam-on 0)) (to-i(* 0xFF led-brightness-highbeam)) 0x00))
    (var rear-color-highbeam (if (and (< direction 0) (= led-highbeam-on 0)) (to-i(* 0xFF led-brightness-highbeam)) 0x00))
    (if (or (= led-front-strip-type 2) (= led-front-strip-type 3)) {
		(setq led-front-color (append (list front-color-highbeam) led-front-color))
    })
    (if (or (= led-rear-strip-type 2) (= led-rear-strip-type 3)) {
        (setq led-rear-color (append (list rear-color-highbeam) led-rear-color))
    })
	
    (if (or (= led-front-strip-type 4) (= led-front-strip-type 5)) {
		(var led-tmp (take led-front-color (length led-front-color)))
		(setq led-front-color (mklist (+(length led-front-color)4) 0))
		(var led-tmp-index 0)
		(looprange k 0 (length led-front-color){
			(if (or (and (= led-front-strip-type 4) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-front-strip-type 5) (or (= k 1) (= k 5) (= k 10) (= k 3)))) {
				(setix led-front-color k front-color-highbeam)
			}{
				(setix led-front-color k (ix led-tmp led-tmp-index))
				(setq led-tmp-index (+ led-tmp-index 1))
			})
		})
		
    })
	;(print led-front-color)
    (if (or (= led-rear-strip-type 4) (= led-rear-strip-type 5)) {
		(var led-tmp (take led-rear-color (length led-rear-color)))
		(setq led-rear-color (mklist (+(length led-rear-color)4) 0))
		(var led-tmp-index 0)
		(looprange k 0 (length led-rear-color){
			(if (or (and (= led-rear-strip-type 4) (or (= k 2) (= k 7) (= k 13) (= k 18))) (and (= led-rear-strip-type 5) (or (= k 1) (= k 5) (= k 10) (= k 3)))) {
				(setix led-rear-color k rear-color-highbeam)
			}{
				(setix led-rear-color k (ix led-tmp led-tmp-index))
				(setq led-tmp-index (+ led-tmp-index 1))
			})
		})
    })
	;(print led-rear-color)
	(var led-status-type (get-config 'led-status-type))
	(var led-front-type (get-config 'led-front-type))
	(var led-rear-type (get-config 'led-rear-type))
	(var led-front-num (length led-front-color))
	(var led-rear-num (length led-rear-color))
    (if (and (>= led-front-pin 0) (= led-status-pin led-front-pin) (= led-front-pin led-rear-pin)) {
        ; All LED strips are chained on the same pin
        (var led-combined-color (append led-status-color led-front-color led-rear-color))
        (var total-leds (length led-combined-color))
        (var led-combined-buffer (rgbled-buffer total-leds led-status-type))
        (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
		(if (not-eq (first (trap (rgbled-init led-front-pin led-front-type))) 'exit-ok) {
			(set-config 'led-front-pin -1)
			;send msg about invalid pin
			(send-msg "Invalid Pin")
			;maybe also save config and checksum?
			(write-val-eeprom 'led-front-pin -1)
			(write-val-eeprom 'crc (config-crc))
		}{
			(sleep2 0.01)
			(rgbled-update led-front-buffer)
			(sleep2 0.01)
		})
		(free led-combined-buffer)
    } {
        ;LED front/back are on same pin
        (if (and (>= led-front-pin 0) (= led-front-pin led-rear-pin)) {
            (var led-combined-color (append led-front-color led-rear-color))
            (var total-leds (length led-combined-color))
            (var led-combined-buffer (rgbled-buffer total-leds led-front-type))
            (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness)
			(if (not-eq (first (trap (rgbled-init led-front-pin led-front-type))) 'exit-ok) {
				(set-config 'led-front-pin -1)
				;send msg about invalid pin
				(send-msg "Invalid Pin")
				;maybe also save config and checksum?
				(write-val-eeprom 'led-front-pin -1)
				(write-val-eeprom 'crc (config-crc))
			}{
				(sleep2 0.01)
				(rgbled-update led-front-buffer)
				(sleep2 0.01)
			})
			(free led-combined-buffer)
        }{
            (if (and (>= led-status-pin 0) (= led-status-pin led-rear-pin)) {
                (if (!= led-status-type led-rear-type)
                    (swap-rg led-status-color); Fix for avaspark rgb when there's different types. e.g. stock GT RGBW
                )
                (var led-combined-color (append led-status-color led-rear-color))
                (var total-leds (length led-combined-color))
                (var led-combined-buffer (rgbled-buffer total-leds led-rear-type))
                (rgbled-color led-combined-buffer 0 led-combined-color led-current-brightness) 
				(if (not-eq (first (trap (rgbled-init led-status-pin led-status-type))) 'exit-ok) {
					(set-config 'led-status-pin -1)
					;send msg about invalid pin
					(send-msg "Invalid Pin")
					;maybe also save config and checksum?
					(write-val-eeprom 'led-status-pin -1)
					(write-val-eeprom 'crc (config-crc))
				}{
					(sleep2 0.01)
					(rgbled-update led-status-buffer)
					(sleep2 0.01)
				})
				(free led-combined-buffer)
            }{
                ; LED strips are on separate pins
                (if (>= led-status-pin 0) {
					(var led-status-buffer (rgbled-buffer (length led-status-color) led-status-type))
                    (rgbled-color led-status-buffer 0 led-status-color (get-config 'led-brightness-status))
					(if (not-eq (first (trap (rgbled-init led-status-pin led-status-type))) 'exit-ok) {
						(set-config 'led-status-pin -1)
						;send msg about invalid pin
						(send-msg "Invalid Pin")
						;maybe also save config and checksum?
						(write-val-eeprom 'led-status-pin -1)
						(write-val-eeprom 'crc (config-crc))
					}{
						(sleep2 0.01)
						(rgbled-update led-status-buffer)
						(sleep2 0.01)
					})
					(free led-status-buffer)
                })
                (if (>= led-rear-pin 0) {
					(var led-rear-buffer (rgbled-buffer led-rear-num led-status-type))
                    (rgbled-color led-rear-buffer 0 led-rear-color led-current-brightness)
					(if (not-eq (first (trap (rgbled-init led-rear-pin led-rear-type))) 'exit-ok) {
						(set-config 'led-rear-pin -1)
						;send msg about invalid pin
						(send-msg "Invalid Pin")
						;maybe also save config and checksum?
						(write-val-eeprom 'led-rear-pin -1)
						(write-val-eeprom 'crc (config-crc))
					}{
						(sleep2 0.01)
						(rgbled-update led-rear-buffer)
						(sleep2 0.01)
					})
					(free led-rear-buffer)
                })
            })
            (if (>= led-front-pin 0) {
				(var led-front-buffer (rgbled-buffer led-front-num led-status-type))
                (rgbled-color led-front-buffer 0 led-front-color led-current-brightness)
				(if (not-eq (first (trap (rgbled-init led-front-pin led-front-type))) 'exit-ok) {
					(set-config 'led-front-pin -1)
					;send msg about invalid pin
					(send-msg "Invalid Pin")
					;maybe also save config and checksum?
					(write-val-eeprom 'led-front-pin -1)
					(write-val-eeprom 'crc (config-crc))
				}{
				(sleep2 0.01)
                ;(looprange k 0 10 {
                    ;(var ratio (/ k 9.0))  ; This ensures ratio goes from 0 to 1
                    ;(print led-front-color-prev)
                    ;(print led-front-color)
                    ;(print ratio)
                    ;(var mixed-buffers (mklist (length led-front-color) 0))
					;(print led-front-color)
					;(print mixed-buffers)
                    ;(looprange index 0 (length led-front-color) {
						;(print (ix led-front-color-prev index))
						;(print (color-mix (ix led-front-color-prev index) (ix led-front-color index) ratio))
                        ;(setix mixed-buffers index (color-mix (ix led-front-color-prev index) (ix led-front-color index) ratio))
                    ;})
					;(print mixed-buffers)
                    ;(rgbled-color led-front-buffer 0 mixed-buffers led-current-brightness)
                    ;(rgbled-update led-front-buffer)
					;(sleep2 0.001)
                ;})
				;(setq led-front-color-prev (take led-front-color (length led-front-color)))
				(rgbled-update led-front-buffer)
				(sleep2 0.01)
				})
				(free led-front-buffer)
            })
        })
    })
})
(defunret init-can () {
    (var can-devices '())
	(var original-can-id (get-config 'can-id ))
	(set-config 'can-id -1)
	(var init-time (systime))
    (loopwhile-thd 150 (<= (secs-since init-time) 10) {
		(if (and (>= original-can-id 0) (<= (secs-since init-time) 5)){
			(setq can-devices (list original-can-id))
		}{
			(setq can-devices (can-scan))
		})
        (loopforeach can-id can-devices {
			(setq discover-can-id can-id)
			(float-cmd can-id (list (assoc float-cmds 'COMMAND_GET_INFO)))
			(sleep2 0.5)
			(if (>= (get-config 'can-id ) 0)
			{
				(if (not-eq (get-config 'can-id ) original-can-id) {
					(write-val-eeprom 'can-id (get-config 'can-id ))
					(write-val-eeprom 'crc (config-crc))
				})
				(return 1)
			})
        })
    })
	(return 0)
})

(def FLOAT_MAGIC 101) ; Magic number used by float
(def FLOAT_ACCESSORIES_MAGIC 102)

; Float commands. Here we are using an association list, but there
; are many other ways to do the same thing.
(def float-cmds '(
		(COMMAND_GET_INFO . 0)
		(COMMAND_GET_ALLDATA . 10)
        (COMMAND_LCM_POLL . 24)
		(COMMAND_LCM_GET_BATTERY . 29)
		(COMMAND_LIGHTS_CONTROL . 202)
))

(defun float-cmd (can-id cmd) {
        (send-data (append (list FLOAT_MAGIC) cmd) 2 can-id)
})

(defun status () {
	(var status-string "status ")
	(setq status-string (str-merge status-string (str-from-n (if (< (secs-since can-last-activity-time) 1) 1 0) "%d ")))
	(setq status-string (str-merge status-string (str-from-n (if (< (secs-since pubmote-last-activity-time) 1) 1 0) "%d ")))
	(setq status-string (str-merge status-string (str-from-n (if (< (secs-since bms-last-activity-time) 1) 1 0) "%d ")))
	(send-data status-string)
})

(defun init-bms () {
	(var bms-rs485-a-pin (get-config 'bms-rs485-a-pin))
	(var bms-wakeup-pin (get-config 'bms-wakeup-pin))
	(uart-stop)
    (if (>= bms-wakeup-pin 0){
		(if (not-eq (first (trap (gpio-configure bms-wakeup-pin 'pin-mode-out))) 'exit-ok) {
			(set-config 'bms-wakeup-pin -1)
			;send msg about invalid pin
			(send-msg "Invalid Pin")
			;maybe also save config and checksum?
			(write-val-eeprom 'bms-wakeup-pin -1)
			(write-val-eeprom 'crc (config-crc))
		})
    })
    (if (>= bms-rs485-a-pin 0){
		(if (not-eq (first (trap (gpio-configure bms-rs485-a-pin 'pin-mode-in-pu))) 'exit-ok) {
			(set-config 'bms-rs485-a-pin -1)
			;send msg about invalid pin
			(send-msg "Invalid Pin")
			(write-val-eeprom 'bms-rs485-a-pin -1)
			(write-val-eeprom 'crc (config-crc))
		}{
			(uart-start 1 bms-rs485-a-pin -1 115200);If GNSS is connected UART 1 must be used
			(set-bms-val 'bms-cell-num 15)
			(set-bms-val 'bms-temp-adc-num 4)
			(set-bms-val 'bms-temp-cell-max 45)
			;(sleep 1);Gotta make sure uart is ready
		})
    })
})

; Helper functions to get and set variable values
(defunret get-config (name) {
    (var pair (assoc eeprom-addrs name))
    (if pair
        (return (car (cdr (cdr (cdr pair)))))
        (return nil)
    )
})


(defun save-config () {
        (loopforeach setting eeprom-addrs {
			(var name (first setting))
			;(print (get-config name))
			(if (eq name 'crc) {
				(write-val-eeprom name (config-crc)) 
			}{
				(write-val-eeprom name (get-config name))
			})
			;(write-val-eeprom name (get-config name))
        })
        (send-data "Settings Saved!")
})

(defunret config-crc () {
	(var i 0)
	(var crclen (- (length eeprom-addrs) 1))
	(var crcbuf (bufcreate crclen))
	(loopforeach setting eeprom-addrs {
		(var name (first setting))
		(if (not-eq name 'crc) {
			(bufset-i32 crcbuf i (get-config name))
			;(print name)
			;(print (get-config name))
			(+ i 1)
		})
	})
	(var crc (crc16 crcbuf))
	;(print crc)
	(free crcbuf)
	(return crc)
})

(defun load-config () {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var val (read-val-eeprom name))
        ;(print val)
        (set-config name val)
    })
})

(defun restore-config () {
    (loopforeach setting eeprom-addrs {
        (var name (first setting))
        (var default-value (if (eq name 'ver-code) config-version (ix setting 3)))
        (write-val-eeprom name default-value)
    })
	(load-config)
	(send-data "Settings Restored!")
})

;(defun print-config ()
;    (loopforeach it eeprom-addrs
;        (print (list (first it) (read-val-eeprom (first it))))
;))

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

(defun update-direction-leds (direction led-forward-color led-backward-color) {
    (var rear-color (if (< direction 0) led-forward-color led-backward-color))
    (var front-color (if (< direction 0) led-backward-color led-forward-color))
    (looprange led-index 0 (length led-front-color) {
        (setix led-front-color led-index front-color)
    })
    (looprange led-index 0 (length led-rear-color) {
        (setix led-rear-color led-index rear-color)
    })
})
(defun main () {
    (if (!= (str-cmp (to-str (sysinfo 'hw-type)) "hw-express") 0) {
        (exit-error "Not running on hw-express")
    })
	(def fw-num (+ (first (sysinfo 'fw-ver)) (* (second (sysinfo 'fw-ver)) 0.01)))
	(if (< fw-num 6.05) (exit-error "hw-express needs to be running 6.05"))
	
	; Restore settings if version number does not match
    ; as that probably means something else is in eeprom
    (if (not-eq (read-val-eeprom 'ver-code) config-version) (restore-config) (load-config))
	;(print (config-crc))
	;(print (to-i (read-val-eeprom 'crc)))
	(if (!= (config-crc) (to-i (read-val-eeprom 'crc)) ){ (send-msg "Error: crc corrupt") (restore-config) })
	;(print "hi")
    ; Spawn the event handler thread and pass the ID it returns to C
	(spawn led-loop); start the led loop as soon as possible once checks are done. once CAN bus comes online it will start responding, and since this is multi-process now leds won't freeze when can is scanning. :)
    (event-register-handler (spawn event-handler))
    (event-enable 'event-data-rx)
	(spawn init-can)
	(if (= (conf-get 'wifi-mode) 0) {
		(send-msg "WiFi is disabled. Please enable and reboot.")
	}{
		(event-enable 'event-esp-now-rx)
		(setq esp-now-remote-mac (list (get-config 'esp-now-remote-mac-a) (get-config 'esp-now-remote-mac-b) (get-config 'esp-now-remote-mac-c) (get-config 'esp-now-remote-mac-d) (get-config 'esp-now-remote-mac-e) (get-config 'esp-now-remote-mac-f)))
		(init-pubmote esp-now-remote-mac)
		(spawn pubmote-loop)
	})
    (init-bms)
    (spawn bms-loop)
})
(defun recv-config (led-on led-highbeam-on led-mode led-mode-idle led-mode-status led-mode-startup led-mall-grab-enabled led-brake-light-enabled led-brake-light-min-amps idle-timeout idle-timeout-shutoff led-brightness led-brightness-highbeam led-brightness-idle led-brightness-status led-status-pin led-status-num led-status-type led-status-reversed led-front-pin led-front-num led-front-type led-front-reversed led-front-strip-type led-rear-pin led-rear-num led-rear-type led-rear-reversed led-rear-strip-type bms-rs485-a-pin bms-wakeup-pin bms-override-soc) {
    (set-config 'led-on (to-i led-on))
    (set-config 'led-highbeam-on (to-i led-highbeam-on))
    (set-config 'led-mode (to-i led-mode))
    (set-config 'led-mode-idle (to-i led-mode-idle))
    (set-config 'led-mode-status (to-i led-mode-status))
    (set-config 'led-mode-startup (to-i led-mode-startup))
    (set-config 'led-mall-grab-enabled (to-i led-mall-grab-enabled))
    (set-config 'led-brake-light-enabled (to-i led-brake-light-enabled))
	(set-config 'led-brake-light-min-amps (to-float led-brake-light-min-amps))
    (set-config 'idle-timeout (to-i idle-timeout))
    (set-config 'idle-timeout-shutoff (to-i idle-timeout-shutoff))
    (set-config 'led-brightness (to-float led-brightness))
	(set-config 'led-brightness-highbeam (to-float led-brightness-highbeam))
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
    (set-config 'led-front-strip-type (to-i led-front-strip-type))
    (set-config 'led-rear-pin (to-i led-rear-pin))
    (set-config 'led-rear-num (to-i led-rear-num))
    (set-config 'led-rear-type (to-i led-rear-type))
    (set-config 'led-rear-reversed (to-i led-rear-reversed))
    (set-config 'led-rear-strip-type (to-i led-rear-strip-type))
	(var bms-rs485-a-pin-prev (get-config 'bms-rs485-a-pin))
    (set-config 'bms-rs485-a-pin (to-i bms-rs485-a-pin))
	(var bms-wakeup-pin-prev (get-config 'bms-wakeup-pin))
    (set-config 'bms-wakeup-pin (to-i bms-wakeup-pin))
    (set-config 'bms-override-soc (to-i bms-override-soc))
	(if (or (!=(to-i bms-rs485-a-pin) bms-rs485-a-pin-prev) (!= (to-i bms-wakeup-pin) bms-wakeup-pin-prev)) {
		(init-bms)
	})
})

(def lut [0 0 0 0 1 2 3 4 5 7 8 11 14 16 18 19 25 30 33 37 43 48 53 60 67 71 76 82 92 97 100])
;(def min-mv 2700)
;(def max-mv 4200)
;(def mv-range (- max-mv min-mv));1500
(defun soc (mv)
  (let ((v (max 0 (min (- mv 2700) (- 1500 1)))))
    (let ((i (* v (/ 30.0 1500))))
      (let ((l (floor i)))
        (let ((f (- i l)))
          (let ((a (bufget-u8 lut l))
                (b (bufget-u8 lut (+ l 1))))
            (max 0 (min (round (+ a (* f (- b a)))) 100))))))))


(defun float-command-rx (data) {
        ;(print (map (fn (x) (bufget-u8 data x)) (range (buflen data))))
        ;Support for saving config/code exec from qml
		(atomic {
			(if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_ACCESSORIES_MAGIC)) {
				(bufcpy data 0 data 1 (-(buflen data) 1))
				(eval (read data))
			})
		})
        ; Only process data if data is long enough and magic number is correct
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) FLOAT_MAGIC)) {

				(setq can-last-activity-time (systime))
                (match (cossa float-cmds (bufget-u8 data 1))
                    (COMMAND_GET_INFO {
						(set-config 'can-id discover-can-id)
                    })
					(COMMAND_LIGHTS_CONTROL {
						(var led-state (bufget-u8 data 2))
						(set-config 'led-on (bits-dec-int led-state 0 1))  ; Get the LSB
						(set-config 'led-highbeam-on (bits-dec-int led-state 1 1))  ; Get the second bit
						; You can now use these variables as needed
					})
                    (COMMAND_LCM_POLL {
							(setq state (bufget-u8 data 2))
							(setq fault-code (bufget-u8 data 3))
							(if (= state 3){
								(setq pitch-angle 0.0)
								(setq duty-cycle-now (bufget-u8 data 4))
							}{
								(setq pitch-angle (to-float (bufget-i8 data 4)))
								(setq duty-cycle-now 0.0)
							})
							(setq rpm (/ (to-float (bufget-i16 data 5)) 10))
							(setq tot-current (/ (to-float (bufget-i16 data 7)) 10))
							(setq input-voltage-filtered (/ (to-float (bufget-i16 data 9)) 10))
                            (set-config 'led-brightness (/ (bufget-u8 data 11) 100.0))
                            (set-config 'led-brightness-idle (/ (bufget-u8 data 12) 100.0))
                            (set-config 'led-brightness-status (/ (bufget-u8 data 13) 100.0))
							;can do extra stuff here
                    })
                    (COMMAND_LCM_GET_BATTERY {
							(setq battery-percent-remaining (/ (bufget-u8 data 2) 100.0))
                    })
                    (COMMAND_GET_ALLDATA {
                        (var mode (bufget-u8 data 2))
                        (if (= mode 69) {
                            (setq fault-code (bufget-u8 data 3))
                        } { ;else
                            (setq pitch-angle (/ (to-float (bufget-i16 data 5)) 10))
                            (setq roll-angle (/ (to-float (bufget-i16 data 7)) 10))
                            (setq state (bufget-u8 data 9))
                            (setq switch-state (bufget-u8 data 10))
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
		(free data)
})

(defun led-loop () {
    (var direction 0)
    (var previous-direction 0)
    (var idle-timeout-shutoff-event 0)
    (var led-mall-grab 0)
    (var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    ;(var actual-loop-time 0)
	;(setq led-front-color-prev (mklist (get-config 'led-front-num) 0))
    (loopwhile-thd 100 t {
	    (setq loop-start-time (secs-since 0))
		(var can-id (get-config 'can-id))
		;"float-accessories1.0\0"
		(float-cmd can-id (append (list (assoc float-cmds 'COMMAND_LCM_POLL)) '(102 108 111 97 116 45 97 99 99 101 115 115 111 114 105 101 115 49 46 48 0)))
		(float-cmd can-id (list (assoc float-cmds 'COMMAND_LIGHTS_CONTROL)))
		(float-cmd can-id (list (assoc float-cmds 'COMMAND_LCM_GET_BATTERY)))
        (if (= (get-config 'led-on) 1) {
			(setq led-status-color (mklist (get-config 'led-status-num) 0))
			(setq led-front-color (mklist (get-config 'led-front-num) 0))
			(setq led-rear-color (mklist (get-config 'led-rear-num) 0))
            (setq direction 0)
            (var idle-rpm-darkride 10)
            (if (= state 4); RUNNING_UPSIDEDOWN 
                (setq idle-rpm-darkride (*idle-rpm-darkride -1))
            )
            (if (> rpm idle-rpm-darkride) {
                (setq direction 1)
            })
            (if (< rpm (* idle-rpm-darkride -1)) {
                (setq direction -1)
            })
            (if (= (get-config 'led-mall-grab-enabled) 1){
                (if (> pitch-angle 70) (setq led-mall-grab 1) (setq led-mall-grab 0))
				;(print pitch-angle)
            })
            (update-leds (secs-since led-last-activity-time) previous-direction direction idle-timeout-shutoff-event led-mall-grab)
            (setq previous-direction direction)
            (if (or (!= direction 0) (= led-mall-grab 1)) {
                (setq idle-timeout-shutoff-event 0)
                (setq led-last-activity-time (systime))
            })
            (if (< idle-timeout-shutoff-event 2) (led-update direction))
        } {
            (clear-leds)
			(led-update direction)
        })
        ; Capture end time and calculate actual loop time
        ;(setq loop-end-time (secs-since 0))
        ;(setq actual-loop-time (- loop-end-time loop-start-time))

        ; Timing control using secs-since 0 and sleep2 with time parameter
        (setq next-run-time (+ next-run-time 0.1))
        (var time-to-wait (- next-run-time (secs-since 0)))
        (if (> time-to-wait 0) {
            (sleep2 time-to-wait)
            (setq next-run-time (secs-since 0))
		}) ; Reset if we're behind
        ; Optionally, log or use the actual_loop_time for analysis or adaptive timing
        ; (print (list 'actual-loop-time actual-loop-time))
    })
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
  (send-data "Settings Read!")
})

(defun init-pubmote (src) {
	;(wifi-set-chan (wifi-get-chan))
	(esp-now-start)
	(esp-now-del-peer esp-now-remote-mac)
	(esp-now-add-peer esp-now-remote-mac)
	;(print (list "starting" (get-mac-addr) (wifi-get-chan)))
	;(print esp-now-remote-mac)
})
(def pairing-state 0)
(defun pair-pubmote (pairing) {
	(if (= (conf-get 'wifi-mode) 0) {
		(send-msg "WiFi is disabled. Please enable and reboot.")
	}{
		(cond
			((>= pairing 0) {
				(set-config 'esp-now-secret-code pairing)
				(setq pubmote-pairing-timer (systime))
				(setq pairing-state 1)
			})
			((= pairing -1) { ;paring accepted
				(init-pubmote esp-now-remote-mac)
				(write-val-eeprom 'esp-now-remote-mac-a (ix esp-now-remote-mac 0))
				(write-val-eeprom 'esp-now-remote-mac-b (ix esp-now-remote-mac 1))
				(write-val-eeprom 'esp-now-remote-mac-c (ix esp-now-remote-mac 2))
				(write-val-eeprom 'esp-now-remote-mac-d (ix esp-now-remote-mac 3))
				(write-val-eeprom 'esp-now-remote-mac-e (ix esp-now-remote-mac 4))
				(write-val-eeprom 'esp-now-remote-mac-f (ix esp-now-remote-mac 5))
				(write-val-eeprom 'esp-now-secret-code esp-now-secret-code)
				(write-val-eeprom 'crc (config-crc))
				(setq pairing-state 0)
			})
			((= pairing -2) { ;paring rejected
				(setq esp-now-remote-mac '())
				(write-val-eeprom 'esp-now-remote-mac-a -1)
				(write-val-eeprom 'crc (config-crc))
				(setq pairing-state 0)
			})
		)
	})
})

(defun pubmote-loop () {
	(var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    ;(var actual-loop-time 0)
    (loopwhile-thd 100 t {
		(setq loop-start-time (secs-since 0))
		(if (and (> (secs-since pubmote-pairing-timer) 30 ) (>= pairing-state 2)){
			(atomic (pair-pubmote -2))
		}) ;timeout pairing process after 30 seconds
		(if (= pairing-state 1){
			(esp-now-del-peer esp-now-remote-mac)
			(setq esp-now-remote-mac '(255 255 255 255 255 255))
			(esp-now-add-peer esp-now-remote-mac)
			(esp-now-send esp-now-remote-mac "42069") ;TODO client side
			(esp-now-del-peer esp-now-remote-mac)
			(setq pairing-state 2)
			;(print "hi")
		})
		(if (and (= pairing-state 0) (>= (get-config 'esp-now-remote-mac-a) 0) (<= (secs-since pubmote-last-activity-time) 5) (>= (get-config 'can-id) 0)){
			(float-cmd (get-config 'can-id) (list (assoc float-cmds 'COMMAND_GET_ALLDATA) 3))
			(var data (bufcreate 32))
			(bufset-u8 data 0 69) ; Mode
			(bufset-u8 data 1 fault-code)
			(bufset-i16 data 2 (floor (* pitch-angle 10)))
			(bufset-i16 data 4 (floor (* roll-angle 10)))
			(bufset-u8 data 6 state)
			(bufset-u8 data 7 switch-state)
			(bufset-i16 data 8 (floor (* input-voltage-filtered 10)))
			(bufset-i16 data 10 (floor rpm))
			(bufset-i16 data 12 (floor (* speed 10)))
			(bufset-i16 data 14 (floor (* tot-current 10)))
			(bufset-u8 data 16 (floor (* (+ duty-cycle-now 0.5) 100)))
			(bufset-f32 data 17 distance-abs 'little-endian)
			(bufset-u8 data 21 (floor (* fet-temp-filtered 2)))
			(bufset-u8 data 22 (floor (* motor-temp-filtered 2)))
			(bufset-u32 data 23 odometer)
			(bufset-u8 data 27 (floor (* battery-level 2)))
			(bufset-i32 data 28 (get-config 'esp-now-secret-code)) ;TODO client side buffers changed
			(esp-now-send esp-now-remote-mac data)
			(free data)
		})
		; Capture end time and calculate actual loop time
		(setq loop-end-time (secs-since 0))
		;(setq actual-loop-time (- loop-end-time loop-start-time))
		; Timing control using secs-since 0 and sleep2 with time parameter
		(setq next-run-time (+ next-run-time 0.05))
		(var time-to-wait (- next-run-time (secs-since 0)))
		(if (> time-to-wait 0) {
			(sleep2 time-to-wait)
			(setq next-run-time (secs-since 0))
		}) ; Reset if we're behind
		; Optionally, log or use the actual_loop_time for analysis or adaptive timing
		; (print (list 'actual-loop-time actual-loop-time))
	})
})

(defun event-handler ()
    (loopwhile t
        (recv
            ((event-esp-now-rx (? src) (? des) (? data) (? rssi)) (pubmote-rx src des data rssi))
            ((event-data-rx . (? data)) (float-command-rx data))
            (_ nil)
        )
    )
)
(defun set-config (name value) {
    (let ((pair (assoc eeprom-addrs name)))
        (if pair
            (loopforeach item eeprom-addrs {
                (if (eq (car item) name)
                    (setcdr item (list (car (cdr item)) 
                                       (car (cdr (cdr item))) 
                                       (car (cdr (cdr (cdr item)))) 
                                       value))
                )
            })
            (setq eeprom-addrs (cons (cons name (list 0 'type 0 value)) eeprom-addrs))
        )
    )
})

(defun pubmote-rx (src des data rssi) {
	(if (= pairing-state 2) {
		(setq esp-now-remote-mac src)
		(esp-now-add-peer esp-now-remote-mac)
		(esp-now-send esp-now-remote-mac (get-config 'esp-now-secret-code)) ;TODO client side
		(esp-now-del-peer esp-now-remote-mac)
		(setq pairing-state 3)
	}{
		(if (and (= (buflen data) 15) (= (bufget-i32 data 11) (get-config 'esp-now-secret-code))) { ;TODO client side buffers changed
			(setq pubmote-last-activity-time (systime))
			;(print (list "Received" src des data rssi))
			(var jsx (bufget-f32 data 0 'little-endian))
			(var jsy (bufget-f32 data 4 'little-endian))
			(var bt-c (bufget-u8 data 8))
			(var bt-z (bufget-u8 data 9))
			(var is-rev (bufget-u8 data 10))
			;(print (list jsy jsx bt-c bt-z is-rev))
			;(rcode-run-noret (get-config 'can-id) `(set-remote-state ,jsy ,jsx ,bt-c ,bt-z ,is-rev))
			(can-cmd (get-config 'can-id) (str-replace (to-str(list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
		})
	})
	(free data)
})
(defun sleep2 (x) (yield (* x 1000000)))
@const-end
(defun update-leds (last-activity-sec previous-direction direction idle-timeout-shutoff-event led-mall-grab) {
	(var idle-timeout (get-config 'idle-timeout))
	(var can-last-activity-time-sec (secs-since can-last-activity-time))
    (if (or (> can-last-activity-time-sec idle-timeout) (< last-activity-sec (get-config 'idle-timeout-shutoff))) {
		(if (> (length led-status-color) 0){
			(if (= (get-config 'led-mode-status) 0)
				(update-status-leds)
			)
        })
        (var current-led-mode (get-config 'led-mode))
        (setq led-current-brightness (get-config 'led-brightness))
		(var idle-timeout (get-config 'idle-timeout))
		(if (>= can-last-activity-time-sec idle-timeout){
			(setq current-led-mode 0)
		}{
			(if (and (<= (secs-since 0) idle-timeout) (= direction 0)) (setq current-led-mode (get-config 'led-mode-startup)))
			(if (> last-activity-sec idle-timeout) {
				(setq current-led-mode (get-config 'led-mode-idle))
				(setq led-current-brightness (get-config 'led-brightness-idle))
			})
		})
        (var led-forward-color 0x00)
        (var led-backward-color 0x00)
		(if (and (> (length led-front-color) 0) (> (length led-rear-color) 0)){
			(cond
				((or (= current-led-mode 1) (= led-mall-grab 1)) {
					(battery-pattern led-front-color)
					(battery-pattern led-rear-color)
				})
				((= current-led-mode 0) {
					(setq led-forward-color 0xFFFFFFFFu32)
					(setq led-backward-color 0x00FF0000u32)
					;(if (or (and (> direction 0) (<= previous-direction 0)) (and (<= direction 0) (> previous-direction 0))) {
					(update-direction-leds direction led-forward-color led-backward-color)
					;})
				})
				((= current-led-mode 2) {
					(setq led-forward-color 0x0000FFFFu32)
					(setq led-backward-color 0x00FF00FFu32)
					(update-direction-leds direction led-forward-color led-backward-color)
				})
				((= current-led-mode 3) {
					(setq led-forward-color 0x000000FFu32)
					(setq led-backward-color 0x0000FF00u32)
					(update-direction-leds direction led-forward-color led-backward-color)
				})
				((= current-led-mode 4) {
					(setq led-forward-color 0x00FFFF00u32)
					(setq led-backward-color 0x0000FF00u32)
					(update-direction-leds direction led-forward-color led-backward-color)
				})
				((= current-led-mode 5) {
					(rainbow-pattern)
				})
				((= current-led-mode 6) {
					(strobe-pattern)
				})
				((= current-led-mode 7) {
					(rave-pattern 0)
				})
				((= current-led-mode 8) {
					(rave-pattern 1)
				})
				((= current-led-mode 9) {
					(knight-rider-pattern)
				})
				((= current-led-mode 10) {
					(felony-pattern)
				})
			)
			;(setq tot-current -4.2)
				;(print (get-config 'led-brake-light-min-amps))
			(if (and (= (get-config 'led-brake-light-enabled) 1) (<= tot-current (get-config 'led-brake-light-min-amps))) (brake-pattern))
		})
    }
    {;else
        (if (< idle-timeout-shutoff-event 2) {
            (clear-leds)
            (setq idle-timeout-shutoff-event (+ idle-timeout-shutoff-event 1))
        })
    })
})

(defun bms-loop () {
    (var bms-timeout 5)
    (var next-run-time (secs-since 0))
    (var loop-start-time 0)
    (var loop-end-time 0)
    ;(var actual-loop-time 0)
    (var bms-buf (bufcreate 64))
    (loopwhile-thd 100 t {
        (setq loop-start-time (secs-since 0))
        (if (>= (get-config 'bms-rs485-a-pin) 0) {
            (var bytes-read (uart-read bms-buf (buflen bms-buf) nil nil 0.5))
            (if (> bytes-read 5) {  ; Only process if we have at least 6 bytes
            (var packet0-found 0)
            (var packet2-found 0)
            (var packet3-found 0)
            (var packet4-found 0)
            (var packet5-found 0)
            (var i 0)
            (loopwhile (and (< i (- bytes-read 5)) (not (and (= packet0-found 1) (= packet2-found 1) (= packet3-found 1) (= packet4-found 1) (= packet5-found 1)))) { ; Need at least 6 bytes for header + type + CRC ; Exit the loop if all packet types have been found
                (if (and (< (+ i 2) bytes-read)
                         (= (bufget-u8 bms-buf i) 0xFF) 
                         (= (bufget-u8 bms-buf (+ i 1)) 0x55)
                         (= (bufget-u8 bms-buf (+ i 2)) 0xAA)) {
                    (setq bms-last-activity-time (systime))
                    (if (< (+ i 3) bytes-read) {
                        (var packet-type (bufget-u8 bms-buf (+ i 3)))
                        (var j (+ i 4)) ; Start after the packet type
                        (loopwhile (and (< j (- bytes-read 2)) ; Leave room for CRC
                                    (not (and (< (+ j 2) bytes-read)
                                              (= (bufget-u8 bms-buf j) 0xFF) 
                                              (= (bufget-u8 bms-buf (+ j 1)) 0x55) 
                                              (= (bufget-u8 bms-buf (+ j 2)) 0xAA)))) {
                            (setq j (+ j 1))
                        })
                        (var len (- j i))
                        (if (>= len 6) { ; Ensure we have at least header + type + CRC
                            (var packet (bufcreate len))
                            (bufcpy packet 0 bms-buf i len)
                            (if (>= (buflen packet) 2) {
                                (var crc (bufget-u16 bms-buf (- j 2)))
                                (var calc-crc 0)
                                (looprange k 0 (- (buflen packet) 2) {
                                    (setq calc-crc (+ calc-crc (bufget-u8 packet k)))
                                })
                                (if (eq crc calc-crc) {
								;(print packet-type)
									(cond
										((and (= packet-type 2) (= packet2-found 0)) {
											(setq packet2-found 1)
											(var cell-index 0)
											(var total-voltage 0)
											(looprange k 4 (- (buflen packet) 4) {
												(if (eq (mod k 2) 0) {
													;calculate voltage based soc based on first cell mv
													(if (and (= cell-index 0) (= (get-config 'bms-override-soc) 1)) {
														(set-bms-val 'bms-soc (/ (soc (bufget-i16 packet k)) 100.0))
													})
													(var current-cell (/ (bufget-i16 packet k) 1000.0))
													(set-bms-val 'bms-v-cell cell-index current-cell)
													(setq cell-index (+ cell-index 1))
													;(print "HI")
													(setq total-voltage (+ total-voltage current-cell))
												})
											})
											(set-bms-val 'bms-v-tot total-voltage)
											
											
										})
										((and (= packet-type 3) (= packet3-found 0)) {
											(setq packet3-found 1)
											(if (= (get-config 'bms-override-soc) 0) {
												(set-bms-val 'bms-soc (/ (bufget-u8 packet 4) 100.0))
											})
										})
										((and (= packet-type 5) (= packet5-found 0)) {
											(setq packet5-found 1)
											(var CURRENT_SCALER 0.055)
											(var current (* (bufget-i16 packet 4) CURRENT_SCALER))
											(set-bms-val 'bms-i-in-ic current)
										})
										((and (= packet-type 0) (= packet0-found 0)) {
											 (setq packet0-found 1)
										;    (var status (bufget-i8 packet 4))
										;    (var isCharging (if (= (bitwise-and status 0x20) 0) 0 1))
										;    (var isBatteryEmpty (if (= (bitwise-and status 0x4) 0) 0 1))
										;    (var isBatteryTempOutOfRange (if (= (bitwise-and status 0x3) 0) 0 1))
										;    (var isBatteryOvercharged (if (= (bitwise-and status 0x8) 0) 0 1))
										})
										((and (= packet-type 4) (= packet4-found 0)) {
											(setq packet4-found 1)
											(set-bms-val 'bms-temp-ic (bufget-i8 packet (- (buflen packet) 3)))
											(looprange k 4 (- (buflen packet) 3) {
												(set-bms-val 'bms-temps-adc (- k 4) (bufget-i8 packet k))
											})
										})
									)
									(setq i j) ; Move i to the start of the next potential packet
                                })
                            })
                            (free packet)
                        })
                        (setq i j) ; Move i to the start of the next potential packet
                    } {
                        (setq i (+ i 1)) ; Move past the header if we can't read the packet type
                    })
                } {
                    (setq i (+ i 1)) ; If no packet header found, move to next byte
                })
            })
            (send-bms-can)
			}{
				;(sleep2 0.1)  ; or (yield 1) depending on your system
			})

    (if (>= (get-config 'bms-wakeup-pin) 0) {
        (if (and (> (secs-since bms-last-activity-time) bms-timeout) (< (secs-since can-last-activity-time) 1) (or (= state 0) (> state 5))) { ;also check can bus has fresh data and float pacakge state is not running just incase someone with discharge enabled configures the bms-wakeup-button for some reason.
            (gpio-write (get-config 'bms-wakeup-pin) 1)
            (sleep2 0.1)
            (gpio-write (get-config 'bms-wakeup-pin) 0)
        })
    })
        })
		; Capture end time and calculate actual loop time
		(setq loop-end-time (secs-since 0))
		;(setq actual-loop-time (- loop-end-time loop-start-time))
		; Timing control using secs-since 0 and sleep2 with time parameter
		(setq next-run-time (+ next-run-time 0.05))
		(var time-to-wait (- next-run-time (secs-since 0)))
		(if (> time-to-wait 0) {
			(sleep2 time-to-wait)
			(setq next-run-time (secs-since 0))
		}) ; Reset if we're behind
		; Optionally, log or use the actual_loop_time for analysis or adaptive timing
		; (print (list 'actual-loop-time actual-loop-time))
    })
})



; Start the main
(main)