;@const-symbol-strings

@const-start

; LED state. Rendering is done by the espled_strip native lib; this module
; only keeps the configuration cache and the state-machine variables the
; LED loop uses to drive it.

; espled effect ids
(def FX-SOLID 0)
(def FX-BREATHE 1)
(def FX-CHASE 2)
(def FX-RAINBOW 3)
(def FX-SPARKLE 4)
(def FX-COMET 5)
(def FX-GAUGE 6)
(def FX-STROBE 7)
(def FX-LARSON 8)
(def FX-FELONY 9)

(def led-loop-delay)
;config vars
(def led-enabled)
(def led-mode)
(def led-mode-idle)
(def led-mode-status)
(def led-mode-startup)
(def led-mode-button)
(def led-mode-footpad)
(def led-mall-grab-enabled)
(def led-brake-light-enabled)
(def led-brake-light-min-amps)
(def idle-timeout)
(def idle-timeout-shutoff)
(def led-status-pin)
(def led-status-num)
(def led-status-type)
(def led-status-reversed)
(def led-front-pin)
(def led-front-num)
(def led-front-type)
(def led-front-reversed)
(def led-front-strip-type)
(def led-rear-pin)
(def led-rear-num)
(def led-rear-type)
(def led-rear-reversed)
(def led-rear-strip-type)
(def led-button-pin)
(def led-button-strip-type)
(def led-footpad-pin)
(def led-footpad-num)
(def led-footpad-type)
(def led-footpad-reversed)
(def led-footpad-strip-type)
(def led-startup-timeout)
(def led-dim-on-highbeam-ratio 0.0)
(def led-status-strip-type)
(def led-max-brightness)
(def led-update-not-running)
(def led-show-battery-charging 0)
(def led-front-highbeam-pin)
(def led-rear-highbeam-pin)

;runtime vars
(def led-current-brightness 0.0)
(def led-smoothed-brightness 0.0)
(def direction 1)
(def led-mall-grab 0)

; espled segment index per strip, -1 when the strip is not present
(def seg-front -1)
(def seg-rear -1)
(def seg-status -1)
(def seg-footpad -1)
(def seg-button -1)

@const-end
