import Vedder.vesc.vescinterface 1.0;import "qrc:/mobile";import QtQuick 2.12
import QtQuick.Controls 2.12
import QtQuick.Layouts 1.3

import Vedder.vesc.commands 1.0
import Vedder.vesc.configparams 1.0
import Vedder.vesc.utility 1.0

Item {
    id: container
    anchors.fill: parent
    anchors.margins: 10

    property Commands mCommands: VescIf.commands()
    property int floatAccessoriesMagic: 102
    Component.onCompleted: {
        if (! (VescIf.getLastFwRxParams().hw.includes("Express") || VescIf.getLastFwRxParams().hw.includes("Avaspark"))) {
            VescIf.emitMessageDialog("Float Accessories","Warning: It doesn't look like this is installed on a VESC Express or Avaspark board", false, false)
        }
        sendCode("f"+"(send-config)")
    }

    ScrollView {
        anchors.fill: parent
        contentHeight: columnLayout.height

        ColumnLayout {
            id: columnLayout
            spacing: 1
            Layout.preferredWidth: 400

            Text {
                color: Utility.getAppHexColor("lightText")
                horizontalAlignment: Text.AlignHCenter
                font.pointSize: 20
                text: "Float Accessories"
            }
            // New: Float Package Connection Status
            Text {
                id: floatPackageStatus
                color: Utility.getAppHexColor("lightText")
                horizontalAlignment: Text.AlignHCenter
                font.pointSize: 12
                text: "Float Package Status: Unknown"
            }
            // New: Pubmote Connection Status
            Text {
                id: pubmoteStatus
                color: Utility.getAppHexColor("lightText")
                text: "Pubmote Status: Unknown"
            }
            // New: BMS Connection Status
            Text {
                id: bmsStatus
                color: Utility.getAppHexColor("lightText")
                text: "BMS Status: Unknown"
            }

            Button {
                text: "Check Status"
                onClicked: {
                sendCode("f"+"(status)")
                }
            }

            // LED Control
            GroupBox {
                title: "LED Control"
                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    CheckBox {
                        id: ledOn
                        text: "LED On"
                        checked: true
                    }

                    CheckBox {
                        id: ledHighbeamOn
                        text: "LED Highbeam On"
                        checked: true
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Brightness"
                    }

                    Slider {
                        id: ledBrightness
                        from: 0.0
                        to: 1.0
                        value: 0.5
                    }
                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Highbeam Brightness"
                    }

                    Slider {
                        id: ledBrightnessHighbeam
                        from: 0.0
                        to: 1.0
                        value: 0.5
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Brightness Idle"
                    }

                    Slider {
                        id: ledBrightnessIdle
                        from: 0.0
                        to: 1.0
                        value: 0.1
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Brightness Status"
                    }

                    Slider {
                        id: ledBrightnessStatus
                        from: 0.0
                        to: 1.0
                        value: 0.2
                    }
                }
            }
            // Pubmote
            GroupBox {
                title: "Pubmote"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 5

                    RowLayout {
                        spacing: 5

                    Button {
                            text: "Pair Pubmote"

                            onClicked: {
                        sendCode("f"+"(pair-pubmote 420)")
                            }
                        }
                    // Pubmote MAC Address
                    Text {
                        id: pubmoteMacAddress
                        color: Utility.getAppHexColor("lightText")
                        text: "Pubmote MAC: Unknown"
                    }
                    }
                }
            }
            // LED General Config
            GroupBox {
                title: "LED General Config"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode"
                    }

                    ComboBox {
                        id: ledMode
                        model: [
                            {text: "White/Red", value: 0},
                            {text: "Battery Meter", value: 1},
                            {text: "Cyan/Magenta", value: 2},
                            {text: "Blue/Green", value: 3},
                            {text: "Yellow/Green", value: 4},
                            {text: "RGB Fade", value: 5},
                            {text: "Strobe", value: 6},
                            {text: "Rave", value: 7},
                            {text: "Mullet", value: 8},
                            {text: "Knight Rider", value: 9},
                            {text: "Felony", value: 10}
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Idle"
                    }

                    ComboBox {
                        id: ledModeIdle
                        model: [
                            {text: "White/Red", value: 0},
                            {text: "Battery Meter", value: 1},
                            {text: "Cyan/Magenta", value: 2},
                            {text: "Blue/Green", value: 3},
                            {text: "Yellow/Green", value: 4},
                            {text: "RGB Fade", value: 5},
                            {text: "Strobe", value: 6},
                            {text: "Rave", value: 7},
                            {text: "Mullet", value: 8},
                            {text: "Knight Rider", value: 9},
                            {text: "Felony", value: 10}
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Startup"
                    }

                    ComboBox {
                        id: ledModeStartup
                        model: [
                            {text: "White/Red", value: 0},
                            {text: "Battery Meter", value: 1},
                            {text: "Cyan/Magenta", value: 2},
                            {text: "Blue/Green", value: 3},
                            {text: "Yellow/Green", value: 4},
                            {text: "RGB Fade", value: 5},
                            {text: "Strobe", value: 6},
                            {text: "Rave", value: 7},
                            {text: "Mullet", value: 8},
                            {text: "Knight Rider", value: 9},
                            {text: "Felony", value: 10}
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Status"
                    }

                    ComboBox {
                        id: ledModeStatus
                        model: [
                            {text: "Green->Red Voltage, Blue Sensor, Yellow->Red Duty", value: 0},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }



                    CheckBox {
                        id: ledMallGrabEnabled
                        text: "LED Mall Grab Enabled"
                        checked: true
                    }

                    CheckBox {
                        id: ledBrakeLightEnabled
                        text: "LED Brake Light Enabled"
                        checked: false
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Brake Light Min Amps"
                    }

                    SpinBox {
                        id: ledBrakeLightMinAmps
                        from: -20.0
                        to: 0.0
                        value: -4.0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "Idle Timeout (sec)"
                    }

                    SpinBox {
                        id: idleTimeout
                        from: 0
                        to: 100
                        value: 5
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "Idle Timeout Shutoff (sec)"
                    }

                    SpinBox {
                        id: idleTimeoutShutoff
                        from: 0
                        to: 1000
                        value: 120
                    }
                }
            }

            // LED Status Config
            GroupBox {
                title: "LED Status Config"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Status Pin"
                    }

                    SpinBox {
                        id: ledStatusPin
                        from: -1
                        to: 100
                        value: 18
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Status Num"
                    }

                    SpinBox {
                        id: ledStatusNum
                        from: 0
                        to: 100
                        value: 5
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Status Type"
                    }

                    ComboBox {
                        id: ledStatusType
                        model: [
                            {text: "GRB", value: 0},
                            {text: "RGB", value: 1},
                            {text: "GRBW", value: 2},
                            {text: "RGBW", value: 3},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    CheckBox {
                        id: ledStatusReversed
                        text: "LED Status Reversed"
                        checked: false
                    }
                }
            }

            // LED Front Config
            GroupBox {
                title: "LED Front Config"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Front Pin"
                    }

                    SpinBox {
                        id: ledFrontPin
                        from: -1
                        to: 100
                        value: 17
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Front Num"
                    }

                    SpinBox {
                        id: ledFrontNum
                        from: 0
                        to: 100
                        value: 13
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Front Type"
                    }

                    ComboBox {
                        id: ledFrontType
                        model: [
                            {text: "GRB", value: 0},
                            {text: "RGB", value: 1},
                            {text: "GRBW", value: 2},
                            {text: "RGBW", value: 3},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Front Highbeam Type"
                    }
                    ComboBox {
                        id: ledFrontHighbeamType
                        model: [
                            {text: "None", value: 0},
                            {text: "Avaspark Laserbeam", value: 1},
                            {text: "JetFleet H4", value: 2},
                            {text: "JetFleet GT", value: 3},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }
                    CheckBox {
                        id: ledFrontReversed
                        text: "LED Front Reversed"
                        checked: false
                    }
                }
            }

            // LED Rear Config
            GroupBox {
                title: "LED Rear Config"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Rear Pin"
                    }

                    SpinBox {
                        id: ledRearPin
                        from: -1
                        to: 100
                        value: 18
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Rear Num"
                    }

                    SpinBox {
                        id: ledRearNum
                        from: 0
                        to: 100
                        value: 7
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Rear Type"
                    }

                    ComboBox {
                        id: ledRearType
                        model: [
                            {text: "GRB", value: 0},
                            {text: "RGB", value: 1},
                            {text: "GRBW", value: 2},
                            {text: "RGBW", value: 3},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Rear Highbeam Type"
                    }
                    ComboBox {
                        id: ledRearHighbeamType
                        model: [
                            {text: "None", value: 0},
                            {text: "Avaspark Laserbeam", value: 1},
                            {text: "JetFleet H4", value: 2},
                            {text: "JetFleet GT", value: 3},
                        ]
                        textRole: "text"
                        valueRole: "value"
                        onCurrentIndexChanged: {
                            value = model[currentIndex].value
                        }
                        property int value: 0
                    }
                    CheckBox {
                        id: ledRearReversed
                        text: "LED Rear Reversed"
                        checked: false
                    }
                }
            }

            // BMS Config
            GroupBox {
                title: "BMS Config"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "BMS RS485 A Pin"
                    }

                    SpinBox {
                        id: bmsRs485APin
                        from: -1
                        to: 100
                        value: -1
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "BMS Wakeup Pin"
                    }

                    SpinBox {
                        id: bmsWakeupPin
                        from: -1
                        to: 100
                        value: -1
                    }

                    CheckBox {
                        id: bmsOverrideSOC
                        text: "BMS Override SOC (Voltage)"
                        checked: false
                    }
                }
            }

            // Save and Restore Buttons
            RowLayout {
                spacing: 10

                Button {
                    text: "Read Config"

                    onClicked: {
                        sendCode("f"+"(send-config)")
                    }
                }

                Button {
                    text: "Write Config"

                    onClicked: {
                        sendCode("f"+"(recv-config " + makeArgStr() + " )")
                        sendCode("f"+"(save-config)")
                    }
                }

                Button {
                    text: "Restore Defaults"

                    onClicked: {
                        sendCode("f"+"(restore-config)")
                        sendCode("f"+"(send-config)")
                    }
                }
            }
        }
    }

function makeArgStr() {
  return (
    ledOn.checked * 1 + " " +
    ledHighbeamOn.checked * 1 + " " +
    ledMode.value + " " +
    ledModeIdle.value + " " +
    ledModeStatus.value + " " +
    ledModeStartup.value + " " +
    ledMallGrabEnabled.checked * 1 + " " +
    ledBrakeLightEnabled.checked * 1 + " " +
    parseFloat(ledBrakeLightMinAmps.value).toFixed(2) + " " +
    idleTimeout.value + " " +
    idleTimeoutShutoff.value + " " +
    parseFloat(ledBrightness.value).toFixed(2) + " " +
    parseFloat(ledBrightnessHighbeam.value).toFixed(2) + " " +
    parseFloat(ledBrightnessIdle.value).toFixed(2) + " " +
    parseFloat(ledBrightnessStatus.value).toFixed(2) + " " +
    ledStatusPin.value + " " +
    ledStatusNum.value + " " +
    ledStatusType.value + " " +
    ledStatusReversed.checked * 1 + " " +
    ledFrontPin.value + " " +
    ledFrontNum.value + " " +
    ledFrontType.value + " " +
    ledFrontReversed.checked * 1 + " " +
    ledFrontHighbeamType.value * 1 + " " +
    ledRearPin.value + " " +
    ledRearNum.value + " " +
    ledRearType.value + " " +
    ledRearReversed.checked * 1 + " " +
    ledRearHighbeamType.value * 1 + " " +
    bmsRs485APin.value + " " +
    bmsWakeupPin.value + " " +
    bmsOverrideSOC.checked * 1
  );
}
    function sendCode(str) {
        mCommands.sendCustomAppData(str + '\0')
    }

    Connections {
        target: mCommands

        function onCustomAppDataReceived(data) {
            var str = data.toString()

            if (str.startsWith("settings")) {
                var tokens = str.split(" ")
                ledOn.checked = Number(tokens[4])
                ledHighbeamOn.checked = Number(tokens[5])
                ledMode.currentIndex = Number(tokens[6])
                ledModeIdle.currentIndex = Number(tokens[7])
                ledModeStatus.currentIndex = Number(tokens[8])
                ledModeStartup.currentIndex = Number(tokens[9])
                ledMallGrabEnabled.checked = Number(tokens[10])
                ledBrakeLightEnabled.checked = Number(tokens[11])
                ledBrakeLightMinAmps.value = Number(tokens[12])
                idleTimeout.value = Number(tokens[13])
                idleTimeoutShutoff.value = Number(tokens[14])
                ledBrightness.value = Number(tokens[15])
                ledBrightnessHighbeam.value = Number(tokens[16])
                ledBrightnessIdle.value = Number(tokens[17])
                ledBrightnessStatus.value = Number(tokens[18])
                ledStatusPin.value = Number(tokens[19])
                ledStatusNum.value = Number(tokens[20])
                ledStatusType.currentIndex = Number(tokens[21])
                ledStatusReversed.checked = Number(tokens[22])
                ledFrontPin.value = Number(tokens[23])
                ledFrontNum.value = Number(tokens[24])
                ledFrontType.currentIndex = Number(tokens[25])
                ledFrontReversed.checked = Number(tokens[26])
                ledFrontHighbeamType.currentIndex = Number(tokens[27])
                ledRearPin.value = Number(tokens[28])
                ledRearNum.value = Number(tokens[29])
                ledRearType.currentIndex = Number(tokens[30])
                ledRearReversed.checked = Number(tokens[31])
                ledRearHighbeamType.currentIndex = Number(tokens[32])
                bmsRs485APin.value = Number(tokens[33])
                bmsWakeupPin.value = Number(tokens[34])
                bmsOverrideSOC.checked = Number(tokens[35])
                // Format and display MAC address
                var macAddress = tokens.slice(36, 42).map(function(token) {
                return ("0" + Number(token).toString(16)).slice(-2);
                }).join(":");

                pubmoteMacAddress.text = "Pubmote MAC: " + ((Number(tokens[36]) == -1) ? "Not Paired" : macAddress.toUpperCase());
            } else if (str.startsWith("msg")) {
                var msg = str.substring(4)
                VescIf.emitMessageDialog("Float Accessories", msg, false, false)
            } else if (str.startsWith("status")) {
                var tokens = str.split(" ")
                // Update Float Package Connection Status
                var floatPackageConnected = Number(tokens[1])
                floatPackageStatus.text = "Float Package Status: " + (floatPackageConnected ? "Connected" : "Not Connected")
                floatPackageStatus.color = floatPackageConnected ? "green" : "red"
                var pubmoteConnected = Number(tokens[2])
                pubmoteStatus.text = "Pubmote Status: " + (pubmoteConnected ? "Connected" : "Not Connected")
                pubmoteStatus.color = pubmoteConnected ? "green" : "red"
                var bmsConnected = Number(tokens[3])
                bmsStatus.text = "BMS Status: " + (bmsConnected ? "Connected" : "Not Connected")
                bmsStatus.color = bmsConnected ? "green" : "red"
            } else {
                VescIf.emitStatusMessage(str, true)
            }
        }
    }
}