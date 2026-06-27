;@const-symbol-strings

@const-start

(def pubmote-loop-delay)  ; Loop delay in microseconds (100ms)
(def pairing-state 0)
(def pubmote-remote-mac '())
(def pubmote-pairing-timer 31)
(def pubmote-pairing-timer-timeout 30) ; How many seconds to wait before aborting pairing
(def uni-mac '(255 255 255 255 255 255)) ; Universal mac (all devices)
(def channel-locked 0)
(def channel-locked-timeout 10) ; How many seconds of no activity to wait before unlocking locked wifi channel
(def pubmote-version-major 0)
(def pubmote-version-minor 0)
(def pubmote-version-patch 0)

(def last-log-time-telemetry-tx 0)
(def last-log-time-telemetry-rx 0)

(def rem-cmds '(
    ; Remote version commands
    (REM_VERSION . 0)
    ; Receiver version commands
    (REM_VERSION_REC. 5)
    ; Bonding commands
    (REM_PAIR_INIT . 10)
    (REM_PAIR_BOND . 11)
    (REM_PAIR_COMPLETE . 12)
    ; Remote specific commands
    (REM_SET_CORE_DATA . 100)
    ; Receiver specific commands
    (REM_SET_INPUT_STATE . 150)
))

(defunret init-pubmote () {
    ; Escape without wifi
    (if (not wifi-enabled-on-boot) {
        (send-msg "WiFi was disabled on boot. Please enable and reboot to use Pubmote.")
        (return false)
    })

    ;(if (is-606-or-newer) {
    ;    (eval '(ble-set-max-clients 2))
    ;})

    (setq pubmote-remote-mac (append (unpack-uint32-to-bytes (get-config 'pubmote-remote-mac-a)) (take (unpack-uint32-to-bytes (get-config 'pubmote-remote-mac-b)) 2)))

    ; Read as bytes, convert to i so we can compare lists
    (loopfor i 0 (< i (length pubmote-remote-mac)) (+ i 1) {
        (setix pubmote-remote-mac i (to-i (ix pubmote-remote-mac i)))
    })

    (esp-now-start)
    (esp-now-del-peer pubmote-remote-mac)
    (esp-now-add-peer pubmote-remote-mac)
    (esp-now-del-peer uni-mac)
    (esp-now-add-peer uni-mac)
    (return true)
})

(defunret pair-pubmote (pairing) {
    (if (= (conf-get 'wifi-mode) 0) {
        (send-msg "WiFi is disabled. Please enable and reboot.")
        (return false)
    })

    (cond
        ((>= pairing 0) {
            (set-config 'pubmote-secret-code (to-i32 pairing))
            (setq pubmote-pairing-timer (systime))
            (setq pairing-state 1)
        })

        ; Pairing accepted
        ((= pairing -1) {
            (if (= (length pubmote-remote-mac) 6) {
                (set-config 'pubmote-remote-mac-a (pack-bytes-to-uint32 (take pubmote-remote-mac 4)))
                (set-config 'pubmote-remote-mac-b (pack-bytes-to-uint32 (append (drop pubmote-remote-mac 4) '(0 0))))
                (atomic {
                    (write-val-eeprom 'pubmote-remote-mac-a (get-config 'pubmote-remote-mac-a))
                    (write-val-eeprom 'pubmote-remote-mac-b (get-config 'pubmote-remote-mac-b))
                })
            })
            (atomic {
                (write-val-eeprom 'pubmote-secret-code (get-config 'pubmote-secret-code))
                (write-val-eeprom 'crc (config-crc cfg-len))
            })
            (init-pubmote)
            (var tmpbuf (bufcreate 2))
            (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_COMPLETE)))
            (bufset-u8 tmpbuf 1 1)
            (print "Sending pairing success message to:" pubmote-remote-mac)
            (pubmote-send-packet pubmote-remote-mac tmpbuf nil)
            (if (connected-ble) {
                (pubmote-send-packet '() tmpbuf t)
            })
            (free tmpbuf)
            (setq pairing-state 0)
        })

        ; Pairing rejected
        ((= pairing -2) {
            (set-config 'pubmote-remote-mac-a -1)
            (atomic {
                (write-val-eeprom 'pubmote-remote-mac-a (get-config 'pubmote-remote-mac-a) -1)
                (write-val-eeprom 'crc (config-crc cfg-len))
            })
            (var tmpbuf (bufcreate 2))
            (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_COMPLETE)))
            (bufset-u8 tmpbuf 1 0)
            (print "Sending pairing rejected message")
            (pubmote-send-packet pubmote-remote-mac tmpbuf nil)
            (if (connected-ble) {
                (pubmote-send-packet '() tmpbuf t)
            })
            (free tmpbuf)
            (setq pubmote-remote-mac '())
            (setq pairing-state 0)

            ; Unlock wifi channel hopping
            (should-unlock-channel pubmote-last-activity-time)
        })
    )

    (return true)
})

(defun lock-channel (reason) {
    (print (str-merge "Channel switching disabled. Reason: " reason))
    (setq channel-locked (wifi-get-chan))
    (wifi-disconnect)
    (wifi-auto-reconnect nil)
})

(defun unlock-channel (reason) {
    (print (str-merge "Channel switching enabled. Reason: " reason))
    (setq channel-locked 0)
    (wifi-auto-reconnect true)
    (wifi-connect (conf-get `wifi-sta-ssid) (conf-get `wifi-sta-key))
})

(defun is-station-mode () {
    (eq (conf-get 'wifi-mode) 1)
})

(defun is-wifi-connected () {
    (eq (wifi-status) 'connected)
})

(defun should-lock-channel () {
    ; Channel is not locked
    ; Station mode
    ; Wifi is not connected
    (and (eq channel-locked 0) (is-station-mode) (not (is-wifi-connected)))
})

(defun should-unlock-channel (last-activity-time) {
    ; Channel is locked
    ; Station mode
    ; Last activity time is not set or more than set time passed since last rx
    (if (and (> channel-locked 0) (is-station-mode) (> (secs-since last-activity-time) channel-locked-timeout)) {
        (unlock-channel (str-from-n pubmote-last-activity-time "Last activity time greater than set time"))
    })
})

(defun should-send-message () {
    (and 
        (= pairing-state 0) 
        (!= (get-config 'pubmote-remote-mac-a) -1)
        (< (secs-since pubmote-last-activity-time) 1.0)
    )
})

(defun pubmote-loop () {
    (if (init-pubmote) {
        (setq pubmote-loop-delay (get-config 'pubmote-loop-delay))
        (var next-run-time (secs-since 0))
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 33))

        (loopwhile t {
            (if (get-config 'pubmote-enabled) {
                ; Check last pubmote activity
                (should-unlock-channel pubmote-last-activity-time)

                (setq loop-start-time  (secs-since 0))

                ; Escape as needed
                (if pubmote-exit-flag {
                    (break)
                })

                ; Timeout pairing process after set time has passed
                (if (and (> (secs-since pubmote-pairing-timer) pubmote-pairing-timer-timeout) (>= pairing-state 1)) {
                    (pair-pubmote -2)
                })

                ; Pairing search 
                (if (= pairing-state 1) {
                    ; Update last activity time for pairing duration
                    (setq pubmote-last-activity-time (systime))

                    (if (should-lock-channel) {
                        (lock-channel "Begin pairing")
                    })

                    (var pairing-data (bufcreate 7))

                    (bufset-u8 pairing-data 0 (to-byte (assoc rem-cmds 'REM_PAIR_INIT)))
                    (var local-mac (get-mac-addr))

                    (looprange i 0 (- (buflen pairing-data) 1) {
                        (bufset-u8 pairing-data (+ i 1) (ix local-mac i))
                    })

                    ; (bufset-u8 data 0 69)
                    ; (print "Sending pairing mac address")
                    (pubmote-send-packet uni-mac pairing-data nil)
                    (if (connected-ble) {
                        (pubmote-send-packet '() pairing-data t)
                    })
                    (free pairing-data)
                })

                ; Bond in progress
                (if (= pairing-state 2) {
                    ; Update last activity time for pairing duration
                    (setq pubmote-last-activity-time (systime))
                })

                ; Connected, send data
                (if (should-send-message) {                
                    (bufset-u8 data 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 data 1 (get-config 'pubmote-secret-code))
                    (bufset-u8 data 5 fault-code)
                    (bufset-i16 data 6 (floor (* pitch-angle 10)))
                    (bufset-i16 data 8 (floor (* roll-angle 10)))
                    (bufset-u8 data 10 state)
                    (bufset-u8 data 11 switch-state)
                    (bufset-i16 data 12 (floor (* vin 10)))
                    (bufset-i16 data 14 (floor rpm))
                    (bufset-i16 data 16 (floor (* speed 10)))
                    (bufset-i16 data 18 (floor (* tot-current 10)))
                    (bufset-u8 data 20 (floor (* (+ (abs duty-cycle-now) 0.5) 100)))
                    (bufset-f32 data 21 distance-abs 'little-endian)
                    (bufset-u8 data 25 (floor (* fet-temp-filtered 2)))
                    (bufset-u8 data 26 (floor (* motor-temp-filtered 2)))
                    (bufset-u32 data 27 odometer)
                    (bufset-u8 data 31 (floor (* battery-percent-remaining 200)))
                    
                    (if (> (- (systime) last-log-time-telemetry-tx) 2000) {
                        (print "Tx REM_SET_CORE_DATA to remote" pubmote-remote-mac)
                        (setq last-log-time-telemetry-tx (systime))
                    })
                    (pubmote-send-packet pubmote-remote-mac data nil)
                })

                (setq loop-end-time (secs-since 0))
                (var actual-loop-time (- loop-end-time loop-start-time))
                (var time-to-wait (- next-run-time (secs-since 0)))

                (if (> time-to-wait 0) {
                    (yield (* time-to-wait 1000000))
                }{
                    (setq next-run-time (secs-since 0))
                })

                (setq next-run-time (+ next-run-time pubmote-loop-delay-sec))
            })
        })

        (free data)
        (setq pubmote-exit-flag nil)
    })
})

(defun should-process-message (src data) {
    (and (= pairing-state 0) (eq pubmote-remote-mac src) (= (bufget-i32 data 1 'little-endian) (get-config 'pubmote-secret-code)))
})

(defun reset-last-activity-time () {
    (setq pubmote-last-activity-time (systime))
})

(defun pubmote-send-packet (dest-mac packet-buf is-ble) {
    (var send-buf (bufcreate (+ (buflen packet-buf) 1)))
    (bufset-u8 send-buf 0 169)
    (bufcpy send-buf 1 packet-buf 0 (buflen packet-buf))

    (if is-ble {
        (send-data send-buf)
    } {
        (esp-now-send dest-mac send-buf)
    })
    (free send-buf)
})

(defun process-pubmote-packet (data is-ble) {
    (var cmd (bufget-u8 data 0))

    (match (cossa rem-cmds cmd)
        (REM_VERSION {
            (if (= (buflen data) 8) {
                (reset-last-activity-time)
                (setq pubmote-version-major (bufget-u8 data 5))
                (setq pubmote-version-minor (bufget-u8 data 6))
                (setq pubmote-version-patch (bufget-u8 data 7))
                (print (str-merge (if is-ble "Remote BLE version: " "Remote version: ") (to-str pubmote-version-major) "." (to-str pubmote-version-minor) "." (to-str pubmote-version-patch)))
            })
        })

        (REM_VERSION_REC {
            (reset-last-activity-time)
            (var tmpbuf (bufcreate 8))
            (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_VERSION_REC)))
            (bufset-i32 tmpbuf 1 (get-config 'pubmote-secret-code))

            (var version (get-version))
            (bufset-u8 tmpbuf 5 (first version))
            (bufset-u8 tmpbuf 6 (second version))
            (bufset-u8 tmpbuf 7 (third version))

            (pubmote-send-packet (if is-ble '() pubmote-remote-mac) tmpbuf is-ble)
            (free tmpbuf)
        })

        (REM_SET_INPUT_STATE {
            (if (> (- (systime) last-log-time-telemetry-rx) 2000) {
                (print "Rx REM_SET_INPUT_STATE from" (if is-ble "BLE" "ESP-NOW"))
                (setq last-log-time-telemetry-rx (systime))
            })
            (if (= (buflen data) 17) {
                (reset-last-activity-time)

                (var jsy (bufget-f32 data 5 'little-endian))
                (var jsx (bufget-f32 data 9 'little-endian))
                (var bt-c (bufget-u8 data 13))
                (var bt-z (bufget-u8 data 14))
                (var is-rev (bufget-u8 data 15))

                (if (>= (get-config 'can-id) 0) {
                    (can-cmd (get-config 'can-id) (str-replace (to-str(list jsy jsx bt-c bt-z is-rev)) "(" "(set-remote-state "))
                })

                (if is-ble {
                    ; Send back telemetry response immediately for BLE
                    (var resp (bufcreate 33))
                    (bufset-u8 resp 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 resp 1 (get-config 'pubmote-secret-code))
                    (bufset-u8 resp 5 fault-code)
                    (bufset-i16 resp 6 (floor (* pitch-angle 10)))
                    (bufset-i16 resp 8 (floor (* roll-angle 10)))
                    (bufset-u8 resp 10 state)
                    (bufset-u8 resp 11 switch-state)
                    (bufset-i16 resp 12 (floor (* vin 10)))
                    (bufset-i16 resp 14 (floor rpm))
                    (bufset-i16 resp 16 (floor (* speed 10)))
                    (bufset-i16 resp 18 (floor (* tot-current 10)))
                    (bufset-u8 resp 20 (floor (* (+ (abs duty-cycle-now) 0.5) 100)))
                    (bufset-f32 resp 21 distance-abs 'little-endian)
                    (bufset-u8 resp 25 (floor (* fet-temp-filtered 2)))
                    (bufset-u8 resp 26 (floor (* motor-temp-filtered 2)))
                    (bufset-u32 resp 27 odometer)
                    (bufset-u8 resp 31 (floor (* battery-percent-remaining 200)))

                    (pubmote-send-packet '() resp t)
                    (free resp)
                })
            })
        })

        (_ {
            (if (not is-ble) {
                (print (str-join (list "No command found: " (to-str cmd))))
            })
        })
    )
})

(defun pubmote-rx (src des data rssi) {
    (if (and (get-config 'pubmote-enabled) wifi-enabled-on-boot) {
        ; Verify and strip PUBMOTE_MAGIC (169)
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) 169)) {
            (bufcpy data 0 data 1 (-(buflen data) 1))
            (buf-resize data -1)

            (if (should-lock-channel) {
                ; Update last activity time in case it does not establish a connection
                (setq pubmote-last-activity-time (systime))

                (lock-channel "ESP-NOW packet received")
            })

            (var cmd (bufget-u8 data 0))
            (if (should-process-message src data) {
                (process-pubmote-packet data nil)
            } {
                ; ESP-NOW specific pairing
                (if (= cmd (to-byte (assoc rem-cmds 'REM_PAIR_BOND))) {
                    (if (= pairing-state 1) {
                        (setq pubmote-remote-mac src)
                        (esp-now-add-peer pubmote-remote-mac)
                        (var tmpbuf (bufcreate 5))
                        (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_BOND)))
                        (bufset-i32 tmpbuf 1 (get-config 'pubmote-secret-code))

                        (print "Responding with pairing code")
                        (pubmote-send-packet pubmote-remote-mac tmpbuf nil)
                        (free tmpbuf)
                        (esp-now-del-peer pubmote-remote-mac)

                        (setq pairing-state 2)
                    })
                })
            })
        })
    })
})

(defun pubmote-ble-rx (data) {
    (if (get-config 'pubmote-enabled) {
        ; Verify and strip PUBMOTE_MAGIC (169)
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) 169)) {
            (var payload-len (- (buflen data) 1))
            (var payload (bufcreate payload-len))
            (bufcpy payload 0 data 1 payload-len)

            (var cmd (bufget-u8 payload 0))
            (print (str-join (list "pubmote-ble-rx: cmd=" (to-str cmd) " len=" (to-str payload-len) " pairing-state=" (to-str pairing-state))))
            ; BLE doesn't check src/mac, but we verify pairing-state and secret code
            (if (and (= pairing-state 0) (= payload-len 5) (= (bufget-i32 payload 1 'little-endian) (get-config 'pubmote-secret-code))) {
                (process-pubmote-packet payload t)
            } {
                ; BLE pairing request
                (if (= cmd (to-byte (assoc rem-cmds 'REM_PAIR_BOND))) {
                    (print "pubmote-ble-rx: Received REM_PAIR_BOND")
                    (if (= pairing-state 1) {
                        (var tmpbuf (bufcreate 5))
                        (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_BOND)))
                        (bufset-i32 tmpbuf 1 (get-config 'pubmote-secret-code))

                        (print "Responding with pairing code over BLE")
                        (pubmote-send-packet '() tmpbuf t)
                        (free tmpbuf)

                        (setq pairing-state 2)
                    })
                })
            })
            (free payload)
        } {
            (print (str-join (list "pubmote-ble-rx invalid magic: " (to-str (bufget-u8 data 0)))))
        })
    })
})
@const-end