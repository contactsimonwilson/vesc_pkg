import QtQuick 2.12
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
        if (VescIf.getLastFwRxParams().hw.includes("Express")) {
            canId.value = -2
        }
        sendCode("f"+"(send-config)")
    }

    ScrollView {
        anchors.fill: parent
        contentHeight: columnLayout.height

        ColumnLayout {
            id: columnLayout
            spacing: 10
            Layout.preferredWidth: 400

            Text {
                color: Utility.getAppHexColor("lightText")
                horizontalAlignment: Text.AlignHCenter
                font.pointSize: 20
                text: "Float Accessories"
            }

            // Features
            GroupBox {
                title: "Features"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    CheckBox {
                        id: ledEnable
                        text: "LED Enable"
                        checked: true
                    }

                    CheckBox {
                        id: bmsEnable
                        text: "BMS Enable"
                        checked: true
                    }

                    CheckBox {
                        id: pubmoteEnable
                        text: "Pubmote Enable"
                        checked: false
                    }
                }
            }

            // VESC Configuration
            GroupBox {
                title: "VESC Configuration"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "CAN ID"
                    }

                    SpinBox {
                        id: canId
                        from: -2
                        to: 254
                        value: 98
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "Cell Series"
                    }

                    SpinBox {
                        id: cellsSeries
                        from: 1
                        to: 100
                        value: 18
                    }
                }
            }

            // LED Settings
            GroupBox {
                title: "LED Settings"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 10

                    CheckBox {
                        id: ledHighbeamOn
                        text: "LED Highbeam On"
                        checked: true
                    }

                    ComboBox {
                        id: ledType
                        model: ["WS2811", "WS2812", "SK6812", "WS2813"]
                        currentIndex: 3
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode"
                    }

                    SpinBox {
                        id: ledMode
                        from: 0
                        to: 10
                        value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Idle"
                    }

                    SpinBox {
                        id: ledModeIdle
                        from: 0
                        to: 10
                        value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Status"
                    }

                    SpinBox {
                        id: ledModeStatus
                        from: 0
                        to: 10
                        value: 0
                    }

                    Text {
                        color: Utility.getAppHexColor("lightText")
                        text: "LED Mode Startup"
                    }

                    SpinBox {
                        id: ledModeStartup
                        from: 0
                        to: 10
                        value: 0
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

            // LED Status Settings
            GroupBox {
                title: "LED Status Settings"

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

                    SpinBox {
                        id: ledStatusType
                        from: 0
                        to: 3
                        value: 1
                    }

                    CheckBox {
                        id: ledStatusReversed
                        text: "LED Status Reversed"
                        checked: false
                    }
                }
            }

            // LED Front Settings
            GroupBox {
                title: "LED Front Settings"

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

                    SpinBox {
                        id: ledFrontType
                        from: 0
                        to: 3
                        value: 2
                    }

                    CheckBox {
                        id: ledFrontReversed
                        text: "LED Front Reversed"
                        checked: false
                    }

                    CheckBox {
                        id: ledFrontHasLaserbeam
                        text: "LED Front Has Laserbeam"
                        checked: false
                    }
                }
            }

            // LED Rear Settings
            GroupBox {
                title: "LED Rear Settings"

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

                    SpinBox {
                        id: ledRearType
                        from: 0
                        to: 3
                        value: 2
                    }

                    CheckBox {
                        id: ledRearReversed
                        text: "LED Rear Reversed"
                        checked: false
                    }

                    CheckBox {
                        id: ledRearHasLaserbeam
                        text: "LED Rear Has Laserbeam"
                        checked: false
                    }
                }
            }

            // BMS Settings
            GroupBox {
                title: "BMS Settings"

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
                }
            }

            // Pubmote MAC Address
            GroupBox {
                title: "Pubmote MAC Address"

                ColumnLayout {
                    anchors.fill: parent
                    spacing: 5

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "A:"
                        }

                        SpinBox {
                            id: espNowRemoteMacA
                            from: 0
                            to: 255
                            value: 132
                        }
                    }

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "B:"
                        }

                        SpinBox {
                            id: espNowRemoteMacB
                            from: 0
                            to: 255
                            value: 252
                        }
                    }

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "C:"
                        }

                        SpinBox {
                            id: espNowRemoteMacC
                            from: 0
                            to: 255
                            value: 230
                        }
                    }

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "D:"
                        }

                        SpinBox {
                            id: espNowRemoteMacD
                            from: 0
                            to: 255
                            value: 80
                        }
                    }

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "E:"
                        }

                        SpinBox {
                            id: espNowRemoteMacE
                            from: 0
                            to: 255
                            value: 168
                        }
                    }

                    RowLayout {
                        spacing: 5

                        Text {
                            color: Utility.getAppHexColor("lightText")
                            text: "F:"
                        }

                        SpinBox {
                            id: espNowRemoteMacF
                            from: 0
                            to: 255
                            value: 12
                        }
                    }
                }
            }

            Item {
                Layout.fillHeight: true
            }

            // Save and Restore Buttons
            RowLayout {
                spacing: 10

                Button {
                    text: "Save Config"

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
    ledEnable.checked * 1 + " " +
    bmsEnable.checked * 1 + " " +
    pubmoteEnable.checked * 1 + " " +
    canId.value + " " +
    cellsSeries.value + " " +
    ledHighbeamOn.checked * 1 + " " +
    ledType.currentIndex + " " +
    ledMode.value + " " +
    ledModeIdle.value + " " +
    ledModeStatus.value + " " +
    ledModeStartup.value + " " +
    ledMallGrabEnabled.checked * 1 + " " +
    ledBrakeLightEnabled.checked * 1 + " " +
    idleTimeout.value + " " +
    idleTimeoutShutoff.value + " " +
    parseFloat(ledBrightness.value).toFixed(2) + " " +
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
    ledFrontHasLaserbeam.checked * 1 + " " +
    ledRearPin.value + " " +
    ledRearNum.value + " " +
    ledRearType.value + " " +
    ledRearReversed.checked * 1 + " " +
    ledRearHasLaserbeam.checked * 1 + " " +
    bmsRs485APin.value + " " +
    bmsWakeupPin.value + " " +
    espNowRemoteMacA.value + " " +
    espNowRemoteMacB.value + " " +
    espNowRemoteMacC.value + " " +
    espNowRemoteMacD.value + " " +
    espNowRemoteMacE.value + " " +
    espNowRemoteMacF.value
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
                ledEnable.checked = Number(tokens[2])
                bmsEnable.checked = Number(tokens[3])
                pubmoteEnable.checked = Number(tokens[4])
                canId.value = Number(tokens[5])
                cellsSeries.value = Number(tokens[6])
                ledHighbeamOn.checked = Number(tokens[7])
                ledType.currentIndex = Number(tokens[8])
                ledMode.value = Number(tokens[9])
                ledModeIdle.value = Number(tokens[10])
                ledModeStatus.value = Number(tokens[11])
                ledModeStartup.value = Number(tokens[12])
                ledMallGrabEnabled.checked = Number(tokens[13])
                ledBrakeLightEnabled.checked = Number(tokens[14])
                idleTimeout.value = Number(tokens[15])
                idleTimeoutShutoff.value = Number(tokens[16])
                ledBrightness.value = Number(tokens[17])
                ledBrightnessIdle.value = Number(tokens[18])
                ledBrightnessStatus.value = Number(tokens[19])
                ledStatusPin.value = Number(tokens[20])
                ledStatusNum.value = Number(tokens[21])
                ledStatusType.value = Number(tokens[22])
                ledStatusReversed.checked = Number(tokens[23])
                ledFrontPin.value = Number(tokens[24])
                ledFrontNum.value = Number(tokens[25])
                ledFrontType.value = Number(tokens[26])
                ledFrontReversed.checked = Number(tokens[27])
                ledFrontHasLaserbeam.checked = Number(tokens[28])
                ledRearPin.value = Number(tokens[29])
                ledRearNum.value = Number(tokens[30])
                ledRearType.value = Number(tokens[31])
                ledRearReversed.checked = Number(tokens[32])
                ledRearHasLaserbeam.checked = Number(tokens[33])
                bmsRs485APin.value = Number(tokens[34])
                bmsWakeupPin.value = Number(tokens[35])
                espNowRemoteMacA.value = Number(tokens[36])
                espNowRemoteMacB.value = Number(tokens[37])
                espNowRemoteMacC.value = Number(tokens[38])
                espNowRemoteMacD.value = Number(tokens[39])
                espNowRemoteMacE.value = Number(tokens[40])
                espNowRemoteMacF.value = Number(tokens[41])
            } else if (str.startsWith("msg")) {
                var msg = str.substring(4)
                VescIf.emitMessageDialog("Float Accessories", msg, false, false)
            } else {
                VescIf.emitStatusMessage(str, true)
            }
        }
    }
}