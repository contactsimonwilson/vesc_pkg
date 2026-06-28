;@const-symbol-strings

@const-start

(def pubmote-loop-delay)  ; Loop delay in microseconds (100ms)
(def PAIR_STATE_IDLE 0)
(def PAIR_STATE_INITIATED 1)
(def PAIR_STATE_BONDING 2)
(def VEHICLE_TYPE_UNSPECIFIED 0)
(def VEHICLE_TYPE_ONEWHEEL 1)
(def VEHICLE_TYPE_ESKATE 2)
(def VEHICLE_TYPE_SCOOTER 3)
(def VEHICLE_TYPE_EUC 4)
(def pairing-state PAIR_STATE_IDLE)
(def pubmote-exit-flag nil)
(def pubmote-last-activity-time (systime))
(def wifi-enabled-on-boot nil)
(def pubmote-remote-mac '())
(def pubmote-pairing-timer 31)
(def pubmote-pairing-timer-timeout 60) ; How many seconds to wait before aborting pairing (increased to 60s)
(def uni-mac '(255 255 255 255 255 255)) ; Universal mac (all devices)
(def channel-locked 0)
(def channel-locked-timeout 10) ; How many seconds of no activity to wait before unlocking locked wifi channel
(def pubmote-version '(0 0 0))
(def pubmote-api-version 1)
(def pubmote-vehicle-type VEHICLE_TYPE_UNSPECIFIED)
(def PUBMOTE_MAGIC 169)

(def pubmote-on-control nil)
(def pubmote-get-telemetry nil)
(def pubmote-send-msg-cb nil)
(def pubmote-get-config nil)
(def pubmote-set-config nil)
(def pubmote-save-config nil)

(defun setup-pubmote (vehicle-type on-control get-telemetry send-msg-cb get-cfg-cb set-cfg-cb save-cfg-cb) {
    (setq pubmote-vehicle-type vehicle-type)
    (setq pubmote-on-control on-control)
    (setq pubmote-get-telemetry get-telemetry)
    (setq pubmote-send-msg-cb send-msg-cb)
    (setq pubmote-get-config get-cfg-cb)
    (setq pubmote-set-config set-cfg-cb)
    (setq pubmote-save-config save-cfg-cb)
}

(defun pubmote-send-msg (text) {
    (if (not-eq pubmote-send-msg-cb nil) {
        (pubmote-send-msg-cb text)
    } {
        (print text)
    })
})

(defun pubmote-get-cfg (name) {
    (if (not-eq pubmote-get-config nil) {
        (pubmote-get-config name)
    })
})

(defun pubmote-set-cfg (name val) {
    (if (not-eq pubmote-set-config nil) {
        (pubmote-set-config name val)
    })
})

(defun pubmote-save-cfg () {
    (if (not-eq pubmote-save-config nil) {
        (pubmote-save-config)
    })
})

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

(defun set-pairing-state (new-state) {
    (setq pairing-state new-state)
    (send-data (str-merge "pairing-status " (to-str new-state)))
})

(defunret init-pubmote () {
    ;(if (is-606-or-newer) {
    ;    (eval '(ble-set-max-clients 2))
    ;})
    (setq wifi-enabled-on-boot (> (conf-get 'wifi-mode) 0))
    (setq pubmote-remote-mac (append (unpack-uint32-to-bytes (pubmote-get-cfg 'pubmote-remote-mac-a)) (take (unpack-uint32-to-bytes (pubmote-get-cfg 'pubmote-remote-mac-b)) 2)))

    ; Read as bytes, convert to i so we can compare lists
    (loopfor i 0 (< i (length pubmote-remote-mac)) (+ i 1) {
        (setix pubmote-remote-mac i (to-i (ix pubmote-remote-mac i)))
    })

    (if (not wifi-enabled-on-boot) {
        (pubmote-send-msg "WiFi disabled. Pubmote running in BLE-only mode.")
    } {
        (esp-now-start)
        (esp-now-del-peer pubmote-remote-mac)
        (esp-now-add-peer pubmote-remote-mac)
        (esp-now-del-peer uni-mac)
        (esp-now-add-peer uni-mac)
    })
    (return true)
})

(defunret pair-pubmote (pairing) {
    (cond
        ((>= pairing 0) {
            (pubmote-set-cfg 'pubmote-secret-code (to-i32 pairing))
            (setq pubmote-pairing-timer (systime))
            (set-pairing-state PAIR_STATE_INITIATED)
        })

        ; Pairing accepted
        ((= pairing -1) {
            (if (= (length pubmote-remote-mac) 6) {
                (pubmote-set-cfg 'pubmote-remote-mac-a (pack-bytes-to-uint32 (take pubmote-remote-mac 4)))
                (pubmote-set-cfg 'pubmote-remote-mac-b (pack-bytes-to-uint32 (append (drop pubmote-remote-mac 4) '(0 0))))
            })
            (pubmote-save-cfg)
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
            (set-pairing-state PAIR_STATE_IDLE)
        })

        ; Pairing rejected
        ((= pairing -2) {
            (pubmote-set-cfg 'pubmote-remote-mac-a -1)
            (pubmote-save-cfg)
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
            (set-pairing-state PAIR_STATE_IDLE)

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
        (= pairing-state PAIR_STATE_IDLE) 
        (!= (pubmote-get-cfg 'pubmote-remote-mac-a) -1)
        (< (secs-since pubmote-last-activity-time) 1.0)
    )
})

(defun pubmote-loop () {
    (if (init-pubmote) {
        (setq pubmote-loop-delay (pubmote-get-cfg 'pubmote-loop-delay))
        (var next-run-time (secs-since 0))
        (var loop-start-time 0)
        (var loop-end-time 0)
        (var pubmote-loop-delay-sec (/ 1.0 pubmote-loop-delay))
        (var data (bufcreate 33))

        (loopwhile t {
            (if (pubmote-get-cfg 'pubmote-enabled) {
                ; Check last pubmote activity
                (should-unlock-channel pubmote-last-activity-time)

                (setq loop-start-time  (secs-since 0))

                ; Escape as needed
                (if pubmote-exit-flag {
                    (break)
                })

                ; Timeout pairing process after set time has passed
                (if (and (> (secs-since pubmote-pairing-timer) pubmote-pairing-timer-timeout) (>= pairing-state PAIR_STATE_INITIATED)) {
                    (pair-pubmote -2)
                })

                ; Pairing search 
                (if (= pairing-state PAIR_STATE_INITIATED) {
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
                (if (= pairing-state PAIR_STATE_BONDING) {
                    ; Update last activity time for pairing duration
                    (setq pubmote-last-activity-time (systime))
                })

                ; Connected, send data
                (if (should-send-message) {                
                    (bufset-u8 data 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 data 1 (pubmote-get-cfg 'pubmote-secret-code))
                    
                    (if (not-eq pubmote-get-telemetry nil) {
                        (var telemetry (pubmote-get-telemetry))
                        (bufset-u8 data 5 (ix telemetry 0))       ; fault-code
                        (bufset-i16 data 6 (floor (* (ix telemetry 1) 10))) ; pitch-angle
                        (bufset-i16 data 8 (floor (* (ix telemetry 2) 10))) ; roll-angle
                        (bufset-u8 data 10 (ix telemetry 3))      ; state
                        (bufset-u8 data 11 (ix telemetry 4))      ; switch-state
                        (bufset-i16 data 12 (floor (* (ix telemetry 5) 10))) ; vin
                        (bufset-i16 data 14 (floor (ix telemetry 6)))     ; rpm
                        (bufset-i16 data 16 (floor (* (ix telemetry 7) 10))) ; speed
                        (bufset-i16 data 18 (floor (* (ix telemetry 8) 10))) ; tot-current
                        (bufset-u8 data 20 (floor (* (+ (abs (ix telemetry 9)) 0.5) 100))) ; duty-cycle-now
                        (bufset-f32 data 21 (ix telemetry 10) 'little-endian) ; distance-abs
                        (bufset-u8 data 25 (floor (* (ix telemetry 11) 2))) ; fet-temp-filtered
                        (bufset-u8 data 26 (floor (* (ix telemetry 12) 2))) ; motor-temp-filtered
                        (bufset-u32 data 27 (ix telemetry 13))    ; odometer
                        (bufset-u8 data 31 (floor (* (ix telemetry 14) 200))) ; battery-percent-remaining
                    })
                    
                    (if (> (- (systime) last-log-time-telemetry-tx) 2000) {
                        ; (print "Tx REM_SET_CORE_DATA to remote" pubmote-remote-mac)
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
    (and (= pairing-state PAIR_STATE_IDLE) (eq pubmote-remote-mac src) (= (bufget-i32 data 1 'little-endian) (pubmote-get-cfg 'pubmote-secret-code)))
})

(defun reset-last-activity-time () {
    (setq pubmote-last-activity-time (systime))
})

(defun pubmote-send-packet (dest-mac packet-buf is-ble) {
    (var send-buf (bufcreate (+ (buflen packet-buf) 1)))
    (bufset-u8 send-buf 0 PUBMOTE_MAGIC)
    (bufcpy send-buf 1 packet-buf 0 (buflen packet-buf))

    (if is-ble {
        (send-data send-buf 8)
    } {
        (if wifi-enabled-on-boot {
            (esp-now-send dest-mac send-buf)
        })
    })
    (free send-buf)
})

(defun process-pubmote-packet (data is-ble) {
    (var cmd (bufget-u8 data 0))

    (match (cossa rem-cmds cmd)
        (REM_VERSION {
            (if (= (buflen data) 8) {
                (reset-last-activity-time)
                (setq pubmote-version (list (bufget-u8 data 5) (bufget-u8 data 6) (bufget-u8 data 7)))
                (print (str-merge (if is-ble "Remote BLE version: " "Remote version: ") (to-str (ix pubmote-version 0)) "." (to-str (ix pubmote-version 1)) "." (to-str (ix pubmote-version 2))))
            })
        })

        (REM_VERSION_REC {
            (reset-last-activity-time)
            (var tmpbuf (bufcreate 8))
            (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_VERSION_REC)))
            (bufset-i32 tmpbuf 1 (pubmote-get-cfg 'pubmote-secret-code))
            (bufset-u16 tmpbuf 5 pubmote-api-version 'little-endian)
            (bufset-u8 tmpbuf 7 pubmote-vehicle-type)

            (pubmote-send-packet (if is-ble '() pubmote-remote-mac) tmpbuf is-ble)
            (free tmpbuf)
        })

        (REM_SET_INPUT_STATE {
            (if (> (- (systime) last-log-time-telemetry-rx) 2000) {
                ; (print "Rx REM_SET_INPUT_STATE from" (if is-ble "BLE" "ESP-NOW"))
                (setq last-log-time-telemetry-rx (systime))
            })
            (if (= (buflen data) 17) {
                (reset-last-activity-time)

                (var jsy (bufget-f32 data 5 'little-endian))
                (var jsx (bufget-f32 data 9 'little-endian))
                (var bt-c (bufget-u8 data 13))
                (var bt-z (bufget-u8 data 14))
                (var is-rev (bufget-u8 data 15))

                (if (not-eq pubmote-on-control nil) {
                    (pubmote-on-control jsy jsx bt-c bt-z is-rev)
                })

                (if is-ble {
                    ; Send back telemetry response immediately for BLE
                    (var resp (bufcreate 33))
                    (bufset-u8 resp 0 (to-byte (assoc rem-cmds 'REM_SET_CORE_DATA)))
                    (bufset-i32 resp 1 (pubmote-get-cfg 'pubmote-secret-code))
                    
                    (if (not-eq pubmote-get-telemetry nil) {
                        (var telemetry (pubmote-get-telemetry))
                        (bufset-u8 resp 5 (ix telemetry 0))       ; fault-code
                        (bufset-i16 resp 6 (floor (* (ix telemetry 1) 10))) ; pitch-angle
                        (bufset-i16 resp 8 (floor (* (ix telemetry 2) 10))) ; roll-angle
                        (bufset-u8 resp 10 (ix telemetry 3))      ; state
                        (bufset-u8 resp 11 (ix telemetry 4))      ; switch-state
                        (bufset-i16 resp 12 (floor (* (ix telemetry 5) 10))) ; vin
                        (bufset-i16 resp 14 (floor (ix telemetry 6)))     ; rpm
                        (bufset-i16 resp 16 (floor (* (ix telemetry 7) 10))) ; speed
                        (bufset-i16 resp 18 (floor (* (ix telemetry 8) 10))) ; tot-current
                        (bufset-u8 resp 20 (floor (* (+ (abs (ix telemetry 9)) 0.5) 100))) ; duty-cycle-now
                        (bufset-f32 resp 21 (ix telemetry 10) 'little-endian) ; distance-abs
                        (bufset-u8 resp 25 (floor (* (ix telemetry 11) 2))) ; fet-temp-filtered
                        (bufset-u8 resp 26 (floor (* (ix telemetry 12) 2))) ; motor-temp-filtered
                        (bufset-u32 resp 27 (ix telemetry 13))    ; odometer
                        (bufset-u8 resp 31 (floor (* (ix telemetry 14) 200))) ; battery-percent-remaining
                    })

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
    (if (and (pubmote-get-cfg 'pubmote-enabled) wifi-enabled-on-boot) {
        ; Verify and strip PUBMOTE_MAGIC
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) PUBMOTE_MAGIC)) {
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
                    (if (= pairing-state PAIR_STATE_INITIATED) {
                        (setq pubmote-remote-mac src)
                        (esp-now-add-peer pubmote-remote-mac)
                        (var tmpbuf (bufcreate 5))
                        (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_BOND)))
                        (bufset-i32 tmpbuf 1 (pubmote-get-cfg 'pubmote-secret-code))

                        (print "Responding with pairing code")
                        (pubmote-send-packet pubmote-remote-mac tmpbuf nil)
                        (free tmpbuf)
                        (esp-now-del-peer pubmote-remote-mac)

                        (set-pairing-state PAIR_STATE_BONDING)
                    })
                })
            })
        })
    })
})

(defun pubmote-ble-rx (data) {
    ; (print "pubmote-ble-rx: entered!")
    (if (pubmote-get-cfg 'pubmote-enabled) {
        ; (print "pubmote-ble-rx: pubmote-enabled is true")
        ; Verify and strip PUBMOTE_MAGIC
        (if (and (> (buflen data) 1) (= (bufget-u8 data 0) PUBMOTE_MAGIC)) {
            ; (print "pubmote-ble-rx: Magic byte found, preparing payload")
            (var payload-len (- (buflen data) 1))
            (var payload (bufcreate payload-len))
            (bufcpy payload 0 data 1 payload-len)

            (var cmd (bufget-u8 payload 0))
            (print (str-join (list "pubmote-ble-rx: cmd=" (to-str cmd) " len=" (to-str payload-len) " pairing-state=" (to-str pairing-state))))
            ; BLE doesn't check src/mac, but we verify pairing-state and secret code
            (if (and (= pairing-state PAIR_STATE_IDLE) (>= payload-len 5) (= (bufget-i32 payload 1 'little-endian) (pubmote-get-cfg 'pubmote-secret-code))) {
                ; (print "pubmote-ble-rx: calling process-pubmote-packet")
                (process-pubmote-packet payload t)
            } {
                ; BLE pairing request
                (if (= cmd (to-byte (assoc rem-cmds 'REM_PAIR_BOND))) {
                    (print "pubmote-ble-rx: Received REM_PAIR_BOND")
                    (if (= pairing-state PAIR_STATE_INITIATED) {
                        (setq pubmote-remote-mac '(0 0 0 0 0 0)) ; Initialize with dummy all-zeros MAC for BLE
                        (var tmpbuf (bufcreate 5))
                        (bufset-u8 tmpbuf 0 (to-byte (assoc rem-cmds 'REM_PAIR_BOND)))
                        (bufset-i32 tmpbuf 1 (pubmote-get-cfg 'pubmote-secret-code))

                        (print "pubmote-ble-rx: Responding with pairing code over BLE")
                        (pubmote-send-packet '() tmpbuf t)
                        (free tmpbuf)

                        (set-pairing-state PAIR_STATE_BONDING)
                    } {
                        (print "pubmote-ble-rx: Ignored REM_PAIR_BOND because pairing-state != PAIR_STATE_INITIATED")
                    })
                } {
                    (print "pubmote-ble-rx: Unhandled command")
                })
            })
            (free payload)
        } {
            (print (str-join (list "pubmote-ble-rx invalid magic: " (to-str (bufget-u8 data 0)))))
        })
    } {
        (print "pubmote-ble-rx: pubmote-enabled is false!")
    })
})
@const-end