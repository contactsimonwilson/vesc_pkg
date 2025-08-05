;@const-symbol-strings

@const-start
(def rainbow-colors '(0x00FF0000 0x00FFFF00 0x0000FF00 0x0000FFFF 0x000000FF 0x00FF00FF))

(defun led-float-disabled (color-list) {
    (var led-num (length color-list))
    (var lit-width (floor (/ led-num 1.8)))
    (if (!= (mod led-num 2.0) (mod lit-width 2.0)) {
        (setq lit-width (- lit-width 1))
    })
    (var unlit-gutter (floor (/ (- led-num lit-width) 2.0)))
    (var start unlit-gutter)
    (var end (+ start lit-width))
    ; Single loop for LEDs
    (looprange i 0 led-num {
        (if (and (>= i start) (< i end)) {
            (if (or (= i start) (= i (- end 1))) {
                (setix color-list i 0x007F0000)  ; Dimmed red for first and last
            }{
                (setix color-list i 0x00FF0000)  ; Full red for center
            })
        }{
            (setix color-list i 0x00000000)  ; Black for outer LEDs
        })
    })
})

(defun led-handtest (color-list switch-state switch-led-count time led-mode-status) {
    (var led-num (length color-list))
    (var left-color  (if (or (= switch-state 1) (= switch-state 3)) 0xFF 0x00))
    (var right-color (if (or (= switch-state 2) (= switch-state 3)) 0xFF 0x00))

    (if (!= led-mode-status 0) {
        (var tmp left-color)
        (setq left-color right-color)
        (setq right-color tmp)
    })

    (var pulse-speed 0.5)
    (var pulse-index (floor (mod (* time 255 pulse-speed) 255)))
    (var center-color (color-make 255 pulse-index 0))

    (looprange i 0 led-num {
        (cond
            ((< i switch-led-count)
                (setix color-list i left-color)
            )
            ((>= i (- led-num switch-led-count))
                (setix color-list i right-color)
            )
            ((or (= i switch-led-count) (= i (- led-num switch-led-count 1)))
                (setix color-list i 0)
            )
            ((and (> i switch-led-count) (< i (- led-num switch-led-count 1)))
                (setix color-list i center-color)
            )
        )
    })
})

(defun led-connecting (color-list time) {
    (var led-num (length color-list))
    (var speed 4.0)
    (var index (floor (mod (* time speed) (+ led-num 1))))
    (looprange i 0 led-num {
        (if (< i index)
            (setix color-list i (color-make 255 0 0))
            (setix color-list i 0)
        )
    })
})

(defun strobe-pattern (color-list color time) {
    (var freq 5.0) ; flashes per second
    (var phase (mod (floor (* time freq)) 2)) ; toggle 0/1
    (var led-color (if (= phase 0) color 0x00000000))
    (set-led-strip-color color-list led-color)
})

(defun rave-pattern (color-list time) {
    (var colors '(0x00FF0000 0x00FFFF00 0x0000FF00 0x0000FFFF 0x000000FF 0x00FF00FF))
    (var idx (floor (mod (* time 4.0) (length colors)))) ; cycle every 0.25s
    (set-led-strip-color color-list (ix colors idx))
})

(defun knight-rider-pattern (color-list time) {
    (var num-leds (length color-list))
    (var tail (+ 1 (/ num-leds 3.0)))
    (var speed 0.7)
    (setq time (* time speed))

    (var backlight (if (> (mod time 2.0) 0.3) 0.08 0.0))
    (var x1 (- (* num-leds (mod time 2.0)) (* 0.5 num-leds) 1.0))
    (var x2 (- (* 1.5 num-leds) (* num-leds (mod (- time 1.0) 2.0))))

    (looprange i 0 num-leds {
        (var k1 backlight)
        (var dist1 (abs (- x1 i)))
        (if (<= i x1) {
            (if (<= dist1 tail) (setq k1 (/ (- tail dist1) tail)))
        }{
            (if (< i (+ x1 1)) (setq k1 (- x1 (floor x1))))
        })

        (var k2 backlight)
        (var dist2 (abs (- x2 i)))
        (if (>= i x2) {
            (if (<= dist2 tail) (setq k2 (/ (- tail dist2) tail)))
        }{
            (if (> i (- x2 1)) (setq k2 (- 1 x2 (floor x2))))
        })

        (var blend (max k1 k2))
        (setix color-list i (color-make (floor (* blend 255)) 0 0))
    })
})

(defun battery-pattern (color-list charging time) {
    (var led-num (length color-list))
    (var num-lit-leds (floor (* led-num battery-percent-remaining)))

    ; Optional pulse factor if charging
    (var pulse-factor (if charging
       (+ 0.55 (* 0.45 (cos (* time 3.14159)))) ; Pulses between 0.0 and 1.0
        1.0))

    (looprange led-index 0 led-num {
        (var color
            (if (or (< led-index num-lit-leds)
                   (and (= led-index 0) (<= num-lit-leds 1))) {
                ; LED should be lit
                (if (or (< battery-percent-remaining 0.2)
                       (and (= led-index 0) (<= num-lit-leds 1))) {
                    ; Low battery - red color
                    (color-make (floor (* 255 pulse-factor)) 0 0)
                } {
                    ; Normal battery - gradient from green to yellow to red
                    (let ((red-ratio (- 1 (/ battery-percent-remaining 0.8)))
                          (green-ratio (/ battery-percent-remaining 0.8))) {
                        (color-make
                            (floor (* 255 red-ratio pulse-factor))
                            (floor (* 255 green-ratio pulse-factor))
                            0)
                    })
                })
            } {
                ; LED should be off
                (color-make 0 0 0)
            }))
        (setix color-list led-index color)
    })
})

(defun battery-pattern-button (color-list charging time) {
    (var pulse-factor (if charging
        (+ 0.55 (* 0.45 (cos (* time 3.14159)))) ; Half speed pulse
        1.0))
    (let ((red-ratio (- 1 (/ battery-percent-remaining 1.0)))
          (green-ratio (/ battery-percent-remaining 1.0))) {
        (setix color-list 0 (color-make
            (floor (* 255 red-ratio pulse-factor))
            (floor (* 255 green-ratio pulse-factor))
            0))
    })
})

(defun rainbow-pattern (color-list time) {
    (var num-leds (length color-list))
    (var speed 0.5)         ; scroll speed
    (var rainbows 1.0)      ; how many rainbows fit on the strip
    (var base-hue (* time speed)) ; how fast it scrolls
    (var per-pixel-offset (/ rainbows num-leds))
    (looprange i 0 num-leds {
        (var hue (mod (+ base-hue (* i per-pixel-offset)) 1.0))
        (setix color-list i (hue-to-color hue))
    })
})

(defun hue-to-color (hue) {
    ; Convert [0..1] to 0..2π
    (var angle (* hue 6.28318)) ; 2π
    ; Shift phases 120° apart
    (var r (+ 1 (cos angle)))           ; 0°
    (var g (+ 1 (cos (- angle 2.0944)))) ; 120° offset
    (var b (+ 1 (cos (- angle 4.1888)))) ; 240° offset
    ; Normalize 0..255
    (var r8 (floor (* (/ r 2.0) 255)))
    (var g8 (floor (* (/ g 2.0) 255)))
    (var b8 (floor (* (/ b 2.0) 255)))
    (color-make r8 g8 b8)
})

(defun trans-pattern (color-list time) {
    (var num-leds (length color-list))
    (var pixels-per-strip (/ num-leds 5.0))
    (var shift (mod (* time 10.0) (* pixels-per-strip 5)))
    (looprange i 0 num-leds {
        (var shifted-index (mod (+ i shift) (* pixels-per-strip 5)))
        (var strip (floor (/ shifted-index pixels-per-strip)))
        (var color (cond
            ((= strip 0) 0x0000FF)
            ((= strip 1) 0xFF69B4)
            ((= strip 2) 0xFFFFFF)
            ((= strip 3) 0xFF69B4)
            ((= strip 4) 0x0000FF)
        ))
        (setix color-list i color)
    })
})

(defun felony-pattern (color-list time) {
    (var led-num (length color-list))
    (var half (floor (/ led-num 2)))
    (var state-duration 0.2)
    (var state (floor (mod (/ time state-duration) 3)))

    (cond
        ((= state 0) {
            (looprange i 0 half (setix color-list i 0x00000000))
            (looprange i half led-num (setix color-list i 0x00FF0000)) ; RED
        })
        ((= state 1) {
            (looprange i 0 half (setix color-list i 0x00FF0000)) ; RED
            (looprange i half led-num (setix color-list i 0x000000FF)) ; BLUE
        })
        ((= state 2) {
            (looprange i 0 half (setix color-list i 0x000000FF)) ; BLUE
            (looprange i half led-num (setix color-list i 0x00000000))
        })
    )

    ; Handle odd LED counts
    (if (= (mod led-num 2) 1) {
        (setix color-list half 0x00000000) ; Set center LED to OFF
    })
})

(defun duty-cycle-pattern (color-list) {
    (var scaled-duty-cycle (* (abs duty-cycle-now) 1.1112))
    (var clamped-duty-cycle 0.0)

    (if (< scaled-duty-cycle 1.0) {
        (setq clamped-duty-cycle scaled-duty-cycle)
    } {
        (setq clamped-duty-cycle 1.0)
    })
    (var led-num (length color-list))
    (var duty-leds (floor (* clamped-duty-cycle led-num)))

    (var duty-color 0x00FFFF00u32)

    (if (> (abs duty-cycle-now) 0.85) {
        (setq duty-color 0x00FF0000u32)
    } {
        (if (> (abs duty-cycle-now) 0.7) {
            (setq duty-color 0x00FF8800u32)
        })
    })

    (looprange led-index 0 led-num {
        (setix color-list led-index (if (< led-index duty-leds) duty-color 0x00000000u32))
    })
})

(defun footpad-pattern (color-list switch-state led-mode-status){
    (var led-num (length color-list))
    (var is-odd (= (mod led-num 2.0) 1))
    (var center-index (floor (/ led-num 2.0)))
    (var color-status-half1 (if (or (= switch-state 1) (= switch-state 3)) 0xFF 0x00))
    (var color-status-half2 (if (or (= switch-state 2) (= switch-state 3)) 0xFF 0x00))
    (var half1-color (if (= led-mode-status 0) color-status-half1 color-status-half2))
    (var half2-color (if (= led-mode-status 0) color-status-half2 color-status-half1))
    (looprange led-index 0 led-num {
        (if (and is-odd (= led-index center-index)) {
            (setix color-list led-index (bitwise-or half1-color half2-color))
        }{
            (if (< led-index center-index) {
                (setix color-list led-index half1-color)
            }{
                (setix color-list led-index half2-color)
            })
        })
    })
})

(defun set-led-strip-color (color-list color) {
    (looprange led-index 0 (length color-list) {
        (setix color-list led-index color)
    })
})
@const-end