import Vedder.vesc.vescinterface 1.0
import "qrc:/mobile"
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
        if (!(VescIf.getLastFwRxParams().hw.includes("Express") || VescIf.getLastFwRxParams().hw.includes("Avaspark"))) {
            VescIf.emitMessageDialog("Float Accessories", "Warning: It doesn't look like this is installed on a VESC Express or Avaspark board", false, false)
        }
        sendCode("f" + "(send-config)")
    }

    ColumnLayout {
        anchors.fill: parent
        spacing: 10

        // Header
        ColumnLayout {
            Layout.fillWidth: true
            spacing: 10

            Text {
                Layout.alignment: Qt.AlignHCenter
                color: Utility.getAppHexColor("lightText")
                font.pointSize: 20
                text: "Float Accessories"
            }

            // Status Section
            RowLayout {
                Layout.fillWidth: true
                spacing: 10

                // Status Texts Column
                ColumnLayout {
                    Layout.fillWidth: true
                    spacing: 5

                    Text {
                        id: floatPackageStatus
                        Layout.fillWidth: true
                        color: Utility.getAppHexColor("lightText")
                        text: "Float Package Status: Unknown"
                    }

                    Text {
                        id: pubmoteStatus
                        Layout.fillWidth: true
                        color: Utility.getAppHexColor("lightText")
                        text: "Pubmote Status: Unknown"
                    }

                    Text {
                        id: bmsStatus
                        Layout.fillWidth: true
                        color: Utility.getAppHexColor("lightText")
                        text: "BMS Status: Unknown"
                    }
                }

                // Check Status Button Column
                ColumnLayout {
                    Layout.alignment: Qt.AlignVCenter | Qt.AlignRight

                    Button {
                        text: "Check Status"
                        onClicked: {
                            sendCode("f" + "(status)")
                        }
                    }
                }
            }
        }
        // Tab Bar
        TabBar {
            id: tabBar
            Layout.fillWidth: true

            TabButton {
                text: qsTr("Control")
            }
            TabButton {
                text: qsTr("Config")
            }
            TabButton {
                text: qsTr("Pubmote")
            }
            TabButton {
                text: qsTr("BMS")
            }
            TabButton {
                text: qsTr("About")
            }
        }

        // Stack Layout
        StackLayout {
            id: stackLayout
            Layout.fillWidth: true
            Layout.fillHeight: true
            currentIndex: tabBar.currentIndex

            // LED Control Tab
            ScrollView {
                clip: true
                ScrollBar.vertical.policy: ScrollBar.AsNeeded

                ColumnLayout {
                    width: stackLayout.width
                    spacing: 10

                    GroupBox {
                        title: "LED Control"
                        Layout.fillWidth: true


                        ColumnLayout {
                            anchors.fill: parent
                            spacing: 10

                            CheckBox {
                                id: ledOn
                                text: "LED On"
                                checked: true
                            }

                            ColumnLayout {
                                id: ledOnLayout
                                visible: ledOn.checked
                                spacing: 10

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

                                ColumnLayout {
                                    id: ledStatusBrightnessLayout
                                    visible: ledStatusStripType.currentValue > 0
                                    spacing: 10
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

                            ColumnLayout {
                                id: ledHighBeamLayout
                                visible: ledOn.checked && (ledFrontStripType.currentValue > 1 || ledRearStripType.currentValue > 1)
                                spacing: 10

                                CheckBox {
                                    id: ledHighbeamOn
                                    text: "LED Highbeam On"
                                    checked: true
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
                            }
                        }
                    }
                }
            }
           // LED Configuration Tab
            ScrollView {
                clip: true
                ScrollBar.vertical.policy: ScrollBar.AsNeeded

                ColumnLayout {
                    width: stackLayout.width
                    spacing: 10

                    GroupBox {
                        title: "LED General Config"
                        Layout.fillWidth: true

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
                                    model: ledMode.model
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
                                    model: ledMode.model
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

                                ColumnLayout {
                                    id: ledBrakeLightLayout
                                    visible: ledBrakeLightEnabled.checked
                                    spacing: 10

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

                        GroupBox {
                            title: "LED Status Config"
                            Layout.fillWidth: true

                            ColumnLayout {
                                anchors.fill: parent
                                spacing: 10

                                Text {
                                    color: Utility.getAppHexColor("lightText")
                                    text: "LED Status Strip"
                                }

                                ComboBox {
                                    id: ledStatusStripType
                                    model: [
                                        {text: "None", value: 0},
                                        {text: "Custom", value: 1},
                                    ]
                                    textRole: "text"
                                    valueRole: "value"
                                    onCurrentIndexChanged: {
                                        value = model[currentIndex].value
                                        updateStatusLEDSettings()
                                    }
                                    property int value: 0
                                }

                                ColumnLayout {
                                    id: ledStatusPinLayout
                                    visible: ledStatusStripType.currentValue > 0
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
                        }

                        GroupBox {
                            title: "LED Front Config"
                            Layout.fillWidth: true

                            ColumnLayout {
                                anchors.fill: parent
                                spacing: 10

                                Text {
                                    color: Utility.getAppHexColor("lightText")
                                    text: "LED Front Strip"
                                }

                                ComboBox {
                                    id: ledFrontStripType
                                    model: [
                                        {text: "None", value: 0},
                                        {text: "Custom", value: 1},
                                        {text: "Avaspark Laserbeam", value: 2},
                                        {text: "Avaspark Laserbeam Pint", value: 3},
                                        {text: "JetFleet H4", value: 4},
                                        {text: "JetFleet GT", value: 5},
                                    ]
                                    textRole: "text"
                                    valueRole: "value"
                                    onCurrentIndexChanged: {
                                        value = model[currentIndex].value
                                        updateFrontLEDSettings()
                                    }
                                    property int value: 0
                                }

                                ColumnLayout {
                                    id: ledFrontPinLayout
                                    visible: ledFrontStripType.currentValue > 0
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
                                }

                                ColumnLayout {
                                    id: ledFrontCustomSettings
                                    visible: ledFrontStripType.currentValue === 1
                                    spacing: 10

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
                                }

                                ColumnLayout {
                                    id: ledFrontReversedLayout
                                    visible: ledFrontStripType.currentValue > 0
                                    spacing: 10

                                    CheckBox {
                                        id: ledFrontReversed
                                        text: "LED Front Reversed"
                                        checked: false
                                    }
                                }
                            }
                        }

                        GroupBox {
                            title: "LED Rear Config"
                            Layout.fillWidth: true

                            ColumnLayout {
                                anchors.fill: parent
                                spacing: 10

                                Text {
                                    color: Utility.getAppHexColor("lightText")
                                    text: "LED Rear Strip"
                                }

                                ComboBox {
                                    id: ledRearStripType
                                    model: [
                                        {text: "None", value: 0},
                                        {text: "Custom", value: 1},
                                        {text: "Avaspark Laserbeam", value: 2},
                                        {text: "Avaspark Laserbeam Pint", value: 3},
                                        {text: "JetFleet H4", value: 4},
                                        {text: "JetFleet GT", value: 5},
                                    ]
                                    textRole: "text"
                                    valueRole: "value"
                                    onCurrentIndexChanged: {
                                        value = model[currentIndex].value
                                        updateRearLEDSettings()
                                    }
                                    property int value: 0
                                }

                                ColumnLayout {
                                    id: ledRearPinLayout
                                    visible: ledRearStripType.currentValue > 0
                                    spacing: 10

                                    Text {
                                        color: Utility.getAppHexColor("lightText")
                                        text: "LED Rear Pin"
                                    }

                                    SpinBox {
                                        id: ledRearPin
                                        from: -1
                                        to: 100
                                        value: 17
                                    }
                                }

                                ColumnLayout {
                                    id: ledRearCustomSettings
                                    visible: ledRearStripType.currentValue === 1
                                    spacing: 10

                                    Text {
                                        color: Utility.getAppHexColor("lightText")
                                        text: "LED Rear Num"
                                    }

                                    SpinBox {
                                        id: ledRearNum
                                        from: 0
                                        to: 100
                                        value: 13
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
                                }

                                ColumnLayout {
                                    id: ledRearReversedLayout
                                    visible: ledRearStripType.currentValue > 0
                                    spacing: 10

                                    CheckBox {
                                        id: ledRearReversed
                                        text: "LED Rear Reversed"
                                        checked: false
                                    }
                                }
                            }
                        }
                    }
                }

            // Pubmote Tab
            ScrollView {
                clip: true
                ScrollBar.vertical.policy: ScrollBar.AsNeeded

                ColumnLayout {
                    width: stackLayout.width
                    spacing: 10

                    GroupBox {
                        title: "Pubmote"
                        Layout.fillWidth: true

                        ColumnLayout {
                            anchors.fill: parent
                            spacing: 5

                            RowLayout {
                                spacing: 5

                                Button {
                                    text: "Pair Pubmote"
                                    onClicked: {
                                        sendCode("f" + "(pair-pubmote 420)")
                                    }
                                }

                                Text {
                                    id: pubmoteMacAddress
                                    color: Utility.getAppHexColor("lightText")
                                    text: "Pubmote MAC: Unknown"
                                }
                            }
                        }
                    }
                }
            }

            // BMS Config Tab
            ScrollView {
                clip: true
                ScrollBar.vertical.policy: ScrollBar.AsNeeded

                ColumnLayout {
                    width: stackLayout.width
                    spacing: 10

                    GroupBox {
                        title: "BMS Config"
                        Layout.fillWidth: true

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
                }
            }
        // About Tab
        ScrollView {
            clip: true
            ScrollBar.vertical.policy: ScrollBar.AsNeeded

            ColumnLayout {
                width: stackLayout.width
                spacing: 20

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    font.pointSize: 16
                    font.bold: true
                    text: "FLOAT ACCESSORIES PACKAGE"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    text: "A VESC Express package for controlling LEDs, BMS, and Pubmote."
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    font.bold: true
                    text: "DISCLAIMER"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    text: "This package is not endorsed by the vesc-project. Use at your own risk."
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    font.bold: true
                    text: "CREDITS"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    text: "Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools (avaspark), auden_builds (pubmote)\n\n" +
                          "gr33tz: outlandnish, exphat, datboig42069\n\n" +
                          "Beta Testers: Koddex, Pickles"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    font.bold: true
                    text: "RELEASE NOTES"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    text: "Requires 6.05 firmware on VESC Express. Checkout the fancy appUI\n\n" +
                          "The VESC Express seems to run out of memory if too many things are going on. I've tried to cut down on memory with lisbm_minimizer.py. " +
                          "Try disabling WiFi or removing some functionality in the script if you're having an issue. " +
                          "Ideally someone will look into improvements with vesc express since wifi is needed for pubmote feature."
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    font.bold: true
                    text: "BUILD INFO"
                }

                Text {
                    Layout.fillWidth: true
                    wrapMode: Text.WordWrap
                    color: Utility.getAppHexColor("lightText")
                    text: "Source code can be found here: https://github.com/relys/vesc_pkg"
                }
            }
        }
    }

        // Save and Restore Buttons
        RowLayout {
            Layout.fillWidth: true
            spacing: 10

            Button {
                text: "Read Config"
                onClicked: {
                    sendCode("f" + "(send-config)")
                }
            }

            Button {
                text: "Write Config"
                onClicked: {
                    sendCode("f" + "(recv-config " + makeArgStr() + " )")
                    sendCode("f" + "(save-config)")
                }
            }

            Button {
                text: "Restore Defaults"
                onClicked: {
                    sendCode("f" + "(restore-config)")
                    sendCode("f" + "(send-config)")
                }
            }
        }
    }

    function updateStatusLEDSettings() {
        switch(ledStatusStripType.value) {
            case 0: // None
                ledStatusPin.value = -1
                break
            case 1: // Custom
                break
            default:
                // Do nothing, keep user-defined values
        }
    }

    function updateFrontLEDSettings() {
        switch(ledFrontStripType.value) {
            case 0: // None
                ledFrontPin.value = -1
                break
            case 1: // Custom
                break
            case 2: // Avaspark Laserbeam
                ledFrontNum.value = 13
                ledFrontType.currentIndex = 0
                break
            case 3: // Avaspark Laserbeam Pint
                ledFrontNum.value = 10
                ledFrontType.currentIndex = 0
                break
            case 4: // JetFleet H4
                ledFrontNum.value = 17
                ledFrontType.currentIndex = 1
                break
            case 5: // JetFleet GT
                ledFrontNum.value = 11
                ledFrontType.currentIndex = 1
                break
            default:
                // Do nothing, keep user-defined values
        }
    }

    function updateRearLEDSettings() {
        switch(ledRearStripType.value) {
            case 0: // None
                ledRearPin.value = -1
                break
            case 1: // Custom
                break
            case 2: // Avaspark Laserbeam
                ledRearNum.value = 13
                ledRearType.currentIndex = 0
                break
            case 3: // Avaspark Laserbeam Pint
                ledRearNum.value = 10
                ledRearType.currentIndex = 0
                break
            case 4: // JetFleet H4
                ledRearNum.value = 17
                ledRearType.currentIndex = 1
                break
            case 5: // JetFleet GT
                ledRearNum.value = 11
                ledRearType.currentIndex = 1
                break
            default:
                // Do nothing, keep user-defined values
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
            ledFrontStripType.value * 1 + " " +
            ledRearPin.value + " " +
            ledRearNum.value + " " +
            ledRearType.value + " " +
            ledRearReversed.checked * 1 + " " +
            ledRearStripType.value * 1 + " " +
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
                ledFrontStripType.currentIndex = Number(tokens[27])
                ledRearPin.value = Number(tokens[28])
                ledRearNum.value = Number(tokens[29])
                ledRearType.currentIndex = Number(tokens[30])
                ledRearReversed.checked = Number(tokens[31])
                ledRearStripType.currentIndex = Number(tokens[32])
                bmsRs485APin.value = Number(tokens[33])
                bmsWakeupPin.value = Number(tokens[34])
                bmsOverrideSOC.checked = Number(tokens[35])
                // Format and display MAC address
                var macAddress = tokens.slice(36, 42).map(function(token) {
                    return ("0" + Number(token).toString(16)).slice(-2);
                }).join(":");

                pubmoteMacAddress.text = "Pubmote MAC: " + ((Number(tokens[36]) == -1) ? "Not Paired" : macAddress.toUpperCase());
                ledStatusStripType.currentIndex = (ledStatusPin.value === -1) ? 0 : 1;
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