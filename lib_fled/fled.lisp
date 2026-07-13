(import "fled/fled_esp32c3.bin" 'lib-esp32c3)
(import "fled/fled_esp32c6.bin" 'lib-esp32c6)
(import "fled/fled_esp32s3.bin" 'lib-esp32s3)
(import "fled/fled_esp32p4.bin" 'lib-esp32p4)

; Native libs only run on the chip they were built for. Requires firmware
; with support for (sysinfo 'hw-target).
(def target (sysinfo 'hw-target))

(def lib (cond
    ((= (str-cmp target "esp32c3") 0) lib-esp32c3)
    ((= (str-cmp target "esp32c6") 0) lib-esp32c6)
    ((= (str-cmp target "esp32s3") 0) lib-esp32s3)
    ((= (str-cmp target "esp32p4") 0) lib-esp32p4)
    (t nil)
))

(if (eq lib nil)
    (print (str-merge "fled: no native lib for target " target))
    (load-native-lib lib)
)

; Convenience used by the test UI: single strip on one pin as segment 0.
(defun fled-setup (pin len type) {
    (ext-fled-deinit)
    (ext-fled-seg-def 0 pin type len)
    (ext-fled-init 1)
})

; The test UI sends lisp expressions as custom app data - evaluate them.
(defun event-handler ()
    (loopwhile t
        (recv
            ((event-data-rx . (? data)) (trap (eval (read data))))
            (_ nil)
)))

(event-register-handler (spawn event-handler))
(event-enable 'event-data-rx)
