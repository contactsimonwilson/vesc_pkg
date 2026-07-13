/*
    Float Accessories - status and tools page.

    Settings live in VESC Tool's standard parameter UI ("Float Accessories
    Cfg", provided by the fa_cfg native lib) - this page only offers status,
    quick LED controls and the pairing/maintenance tools that need custom
    flows.
*/

import QtQuick 2.15
import QtQuick.Controls 2.15
import QtQuick.Layouts 1.15

import Vedder.vesc.commands 1.0
import Vedder.vesc.utility 1.0

Item {
    id: container
    anchors.fill: parent
    anchors.margins: 10

    property Commands mCommands: VescIf.commands()

    // Status fields (float-stats)
    property int canConnected: 0
    property int pubmoteConnected: 0
    property int bmsConnected: 0
    property int bmsStatus: -1
    property int bmsBatteryType: -1
    property int bmsCycles: -1
    property int wifiChan: -1
    property real humidity: 0
    property real humidityTemp: 0
    property real bmsHumidity: 0
    property real bmsHumidityTemp: 0
    property int logRunning: 0
    property int tosAccepted: 1
    property string pubmoteVersion: ""
    property string lastMsg: ""

    // Control state
    property int ledOn: 1
    property int ledHighbeamOn: 1
    property real ledBrightness: 0.8
    property real ledBrightnessHighbeam: 0.8
    property real ledBrightnessIdle: 0.5
    property real ledBrightnessStatus: 0.2
    property int bmsChargeState: 0
    property bool controlSynced: false

    // Pairing
    property int pairingStatus: 0
    property int pairCode: -1

    function sendCode(str) {
        mCommands.sendCustomAppData(String.fromCharCode(102) + String.fromCharCode(1) + str + "\0")
    }

    function sendControl() {
        sendCode("(recv-control " + ledOn + " " + ledHighbeamOn + " " +
                 ledBrightness.toFixed(2) + " " + ledBrightnessHighbeam.toFixed(2) + " " +
                 ledBrightnessIdle.toFixed(2) + " " + ledBrightnessStatus.toFixed(2) + " " +
                 bmsChargeState + ")")
    }

    function hexStringToLispList(hex) {
        var out = "(list"
        for (var i = 0; i < hex.length; i += 2) {
            out += " " + parseInt(hex.substr(i, 2), 16)
        }
        return out + ")"
    }

    Timer {
        interval: 1000
        repeat: true
        running: true
        onTriggered: {
            sendCode("(status)")
            if (!controlSynced) {
                sendCode("(send-control)")
            }
        }
    }

    Timer {
        id: inputPreviewTimer
        interval: 100
        repeat: true
        running: pubmotePreviewEnabled.checked
        onTriggered: sendCode("(input-state)")
    }

    Connections {
        target: mCommands

        function onCustomAppDataReceived(data) {
            var str = data.toString()

            if (str.startsWith("float-stats")) {
                var tk = str.split(" ")
                if (tk.length >= 13) {
                    canConnected = parseInt(tk[1])
                    pubmoteConnected = parseInt(tk[2])
                    bmsConnected = parseInt(tk[3])
                    bmsStatus = parseInt(tk[4])
                    bmsBatteryType = parseInt(tk[5])
                    bmsCycles = parseInt(tk[6])
                    wifiChan = parseInt(tk[7])
                    humidity = parseFloat(tk[8])
                    humidityTemp = parseFloat(tk[9])
                    bmsHumidity = parseFloat(tk[10])
                    bmsHumidityTemp = parseFloat(tk[11])
                    logRunning = parseInt(tk[12])
                }
                if (tk.length >= 14) {
                    tosAccepted = parseInt(tk[13])
                }
            } else if (str.startsWith("control")) {
                var ct = str.split(" ")
                if (ct.length >= 8) {
                    ledOn = parseInt(ct[1])
                    ledHighbeamOn = parseInt(ct[2])
                    ledBrightness = parseFloat(ct[3])
                    ledBrightnessHighbeam = parseFloat(ct[4])
                    ledBrightnessIdle = parseFloat(ct[5])
                    ledBrightnessStatus = parseFloat(ct[6])
                    bmsChargeState = parseInt(ct[7])
                    controlSynced = true
                }
            } else if (str.startsWith("pairing-status")) {
                pairingStatus = parseInt(str.split(" ")[1])
            } else if (str.startsWith("pubmote-info")) {
                pubmoteVersion = str.split(" ")[1]
            } else if (str.startsWith("input-state")) {
                var it = str.split(" ")
                if (it.length >= 7) {
                    previewConn.text = parseInt(it[1]) === 1 ? "connected" : "not connected"
                    previewJs.text = "JS: " + it[2] + " / " + it[3]
                    previewBt.text = "C: " + it[4] + "  Z: " + it[5] + "  Rev: " + it[6]
                }
            } else if (str.startsWith("msg") || str.startsWith("status")) {
                lastMsg = str.substring(str.indexOf(" ") + 1)
            }
        }
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 4

        TabBar {
            id: tabBar
            Layout.fillWidth: true

            TabButton { text: "Status" }
            TabButton { text: "Pubmote" }
            TabButton { text: "BMS" }
            TabButton { text: "About" }
        }

        StackLayout {
            Layout.fillWidth: true
            Layout.fillHeight: true
            currentIndex: tabBar.currentIndex

            // ---- Status / control -------------------------------------
            ScrollView {
                contentWidth: availableWidth
                clip: true

                ColumnLayout {
                    width: parent.width
                    spacing: 8

                    GroupBox {
                        title: "Status"
                        Layout.fillWidth: true

                        GridLayout {
                            anchors.fill: parent
                            columns: 2

                            Label { text: "CAN" }
                            Label { text: canConnected === 1 ? "connected" : "searching..." }

                            Label { text: "Pubmote" }
                            Label { text: pubmoteConnected === 1 ? ("connected " + pubmoteVersion) : "not connected" }

                            Label { text: "BMS" }
                            Label { text: bmsConnected === 1 ? ("connected (status " + bmsStatus + ")") : "not connected" }

                            Label { text: "Humidity" }
                            Label { text: humidity.toFixed(0) + " %  " + humidityTemp.toFixed(1) + " C" }

                            Label { text: "Logging" }
                            Label { text: logRunning === 1 ? "running" : "stopped" }

                            Label { text: "Message" }
                            Label { text: lastMsg; Layout.fillWidth: true; elide: Text.ElideRight }
                        }
                    }

                    GroupBox {
                        title: "LED Control"
                        Layout.fillWidth: true

                        ColumnLayout {
                            anchors.fill: parent

                            RowLayout {
                                Layout.fillWidth: true
                                Switch {
                                    text: "LEDs"
                                    checked: ledOn === 1
                                    onToggled: { ledOn = checked ? 1 : 0; sendControl() }
                                }
                                Switch {
                                    text: "Highbeam"
                                    checked: ledHighbeamOn === 1
                                    onToggled: { ledHighbeamOn = checked ? 1 : 0; sendControl() }
                                }
                            }

                            Label { text: "Brightness " + briSlider.value.toFixed(2) }
                            Slider {
                                id: briSlider
                                from: 0.0; to: 1.0
                                value: ledBrightness
                                Layout.fillWidth: true
                                onPressedChanged: if (!pressed) { ledBrightness = value; sendControl() }
                            }

                            Label { text: "Idle brightness " + briIdleSlider.value.toFixed(2) }
                            Slider {
                                id: briIdleSlider
                                from: 0.0; to: 1.0
                                value: ledBrightnessIdle
                                Layout.fillWidth: true
                                onPressedChanged: if (!pressed) { ledBrightnessIdle = value; sendControl() }
                            }

                            Label {
                                text: "All other settings: VESC Tool -> Float Accessories Cfg"
                                font.italic: true
                                Layout.fillWidth: true
                                wrapMode: Text.WordWrap
                            }
                        }
                    }

                    GroupBox {
                        title: "Logging"
                        Layout.fillWidth: true

                        RowLayout {
                            anchors.fill: parent
                            Button {
                                text: "Start Log"
                                Layout.fillWidth: true
                                onClicked: sendCode("(start-log (get-config 'log-append-gnss) (get-config 'log-rate))")
                            }
                            Button {
                                text: "Stop Log"
                                Layout.fillWidth: true
                                onClicked: sendCode("(stop-log)")
                            }
                        }
                    }
                }
            }

            // ---- Pubmote -----------------------------------------------
            ScrollView {
                contentWidth: availableWidth
                clip: true

                ColumnLayout {
                    width: parent.width
                    spacing: 8

                    GroupBox {
                        title: "Pairing"
                        Layout.fillWidth: true

                        ColumnLayout {
                            anchors.fill: parent

                            Label {
                                text: pairingStatus === 0 ? "Not pairing" :
                                      pairingStatus === 1 ? "Searching for remote..." :
                                      pairingStatus === 2 ? "Remote found - accept?" : "Paired"
                            }

                            RowLayout {
                                Layout.fillWidth: true

                                TextField {
                                    id: pairCodeField
                                    placeholderText: "Pairing code"
                                    validator: IntValidator { bottom: 0; top: 9999 }
                                    Layout.fillWidth: true
                                }
                                Button {
                                    text: "Start Pairing"
                                    onClicked: sendCode("(pair-pubmote " + (pairCodeField.text.length > 0 ? pairCodeField.text : "0") + ")")
                                }
                            }

                            RowLayout {
                                Layout.fillWidth: true
                                Button {
                                    text: "Accept"
                                    enabled: pairingStatus === 2
                                    Layout.fillWidth: true
                                    onClicked: sendCode("(pair-pubmote -1)")
                                }
                                Button {
                                    text: "Reject / Unpair"
                                    Layout.fillWidth: true
                                    onClicked: sendCode("(pair-pubmote -2)")
                                }
                            }
                        }
                    }

                    GroupBox {
                        title: "Input Preview"
                        Layout.fillWidth: true

                        ColumnLayout {
                            anchors.fill: parent

                            Switch {
                                id: pubmotePreviewEnabled
                                text: "Poll input"
                            }
                            Label { id: previewConn; text: "-" }
                            Label { id: previewJs; text: "-" }
                            Label { id: previewBt; text: "-" }
                        }
                    }
                }
            }

            // ---- BMS ---------------------------------------------------
            ScrollView {
                contentWidth: availableWidth
                clip: true

                ColumnLayout {
                    width: parent.width
                    spacing: 8

                    GroupBox {
                        title: "BMS Status"
                        Layout.fillWidth: true

                        GridLayout {
                            anchors.fill: parent
                            columns: 2

                            Label { text: "Connected" }
                            Label { text: bmsConnected === 1 ? "yes" : "no" }

                            Label { text: "Status" }
                            Label { text: "" + bmsStatus }

                            Label { text: "Battery type" }
                            Label { text: "" + bmsBatteryType }

                            Label { text: "Cycles" }
                            Label { text: "" + bmsCycles }

                            Label { text: "BMS humidity" }
                            Label { text: bmsHumidity.toFixed(0) + " %  " + bmsHumidityTemp.toFixed(1) + " C" }
                        }
                    }

                    GroupBox {
                        title: "BMS Keys"
                        Layout.fillWidth: true

                        ColumnLayout {
                            anchors.fill: parent

                            TextField {
                                id: keyInput
                                placeholderText: "Key (16 hex bytes)"
                                Layout.fillWidth: true
                            }
                            TextField {
                                id: counterInput
                                placeholderText: "Counter (16 hex bytes)"
                                Layout.fillWidth: true
                            }
                            Button {
                                text: "Submit Keys"
                                Layout.fillWidth: true
                                onClicked: {
                                    var keyHex = keyInput.text.replace(/[^0-9A-Fa-f]/g, '')
                                    var counterHex = counterInput.text.replace(/[^0-9A-Fa-f]/g, '')
                                    if (keyHex.length === 32 && counterHex.length === 32) {
                                        sendCode("(send-keys " + hexStringToLispList(keyHex) + " " + hexStringToLispList(counterHex) + ")")
                                    } else {
                                        VescIf.emitMessageDialog("BMS Keys", "Key and counter must each be 16 hex bytes.", false, false)
                                    }
                                }
                            }
                            Button {
                                text: "Factory Init"
                                Layout.fillWidth: true
                                onClicked: sendCode("(bms-trigger-factory-init)")
                            }
                        }
                    }
                }
            }

            // ---- About -------------------------------------------------
            ScrollView {
                contentWidth: availableWidth
                clip: true

                ColumnLayout {
                    width: parent.width
                    spacing: 8

                    GroupBox {
                        title: "Float Accessories"
                        Layout.fillWidth: true

                        Label {
                            anchors.fill: parent
                            wrapMode: Text.WordWrap
                            text: "Smart LED control, Pubmote tilt remote and stock OW BMS bridge " +
                                  "for the VESC Express.\n\n" +
                                  "LEDs are rendered by the ESPLED Strip native library. All settings " +
                                  "are in VESC Tool's standard parameter pages under 'Float Accessories " +
                                  "Cfg' and are stored by the firmware.\n\n" +
                                  "Credits: Syler Clayton, Benjamin Vedder, surfdado, NuRxG, Siwoz, " +
                                  "lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools & marcos (avaspark), " +
                                  "auden_builds (pubmote)."
                        }
                    }

                    GroupBox {
                        title: "Terms"
                        Layout.fillWidth: true
                        visible: tosAccepted !== 1

                        ColumnLayout {
                            anchors.fill: parent

                            Label {
                                Layout.fillWidth: true
                                wrapMode: Text.WordWrap
                                text: "This package controls vehicle hardware. Use at your own risk; " +
                                      "verify your LED wiring and pins before enabling features."
                            }
                            Button {
                                text: "Accept"
                                Layout.fillWidth: true
                                onClicked: { sendCode("(accept-tos)"); tosAccepted = 1 }
                            }
                        }
                    }
                }
            }
        }
    }
}
