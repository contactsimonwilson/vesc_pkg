@const-start

(defun command-rx (data) {
    (if (< (buflen data) 2) {
        (dbg DBG-CMD "cmd short packet")
    } {
        (var magic-byte (bufget-u8 data 0))

        ; Route to Pubmote BLE RX handler
        (if (= magic-byte PUBMOTE_MAGIC) {
            (pubmote-ble-rx data)
        })

        ; Route to Accessories QML command handler
        (if (= magic-byte FLOAT_ACCESSORIES_MAGIC) {
            (float-accessories-command-rx data)
        })

        ; Route to VESC Float package telemetry handler.
        ; Dropped while the bench simulator owns the telemetry globals: letting
        ; it parse would have a real board on the bus fighting the scripted
        ; values at the CAN poll rate, flickering the status LED between them.
        (if (and (= magic-byte FLOAT_MAGIC) (not sim-active)) {
            (float-pkg-telemetry-rx data)
        })

        ; Anything else is a packet meant for someone else (or a mismatched
        ; QML page). Logging it is the only way to tell those two apart.
        (if (and (!= magic-byte PUBMOTE_MAGIC)
                 (!= magic-byte FLOAT_ACCESSORIES_MAGIC)
                 (!= magic-byte FLOAT_MAGIC)
                 (dbg-tick DBG-CMD 'cmd-rx 2.0))
            (dbg DBG-CMD (str-merge "cmd unrouted magic "
                (str-from-n magic-byte "%d"))))
    })
})

@const-end
