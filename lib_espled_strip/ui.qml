/*
    Copyright 2026 VESC project

    This file is part of the VESC firmware.

    The VESC firmware is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    The VESC firmware is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

    // The espled_strip.lisp script evaluates expressions sent as custom app data.
    function sendCode(str) {
        mCommands.sendCustomAppData(str + "\0")
    }

    function packedColor() {
        return (rSlider.value << 16) | (gSlider.value << 8) | bSlider.value
    }

    function sendColor() {
        sendCode("(ext-espled-seg-col 0 " + packedColor() + ")")
    }

    ScrollView {
        anchors.fill: parent
        contentWidth: availableWidth
        clip: true

        ColumnLayout {
            width: parent.width
            spacing: 8

            GroupBox {
                title: "Strip Setup"
                Layout.fillWidth: true

                GridLayout {
                    anchors.fill: parent
                    columns: 2

                    Label { text: "Pin" }
                    SpinBox {
                        id: pinBox
                        from: 0; to: 48; value: 20
                        editable: true
                        Layout.fillWidth: true
                    }

                    Label { text: "LEDs" }
                    SpinBox {
                        id: lenBox
                        from: 1; to: 512; value: 30
                        editable: true
                        Layout.fillWidth: true
                    }

                    Label { text: "Type" }
                    ComboBox {
                        id: typeBox
                        model: ["GRB (WS2812)", "RGB", "GRBW (SK6812 RGBW)", "RGBW"]
                        Layout.fillWidth: true
                    }

                    Button {
                        text: "Start"
                        Layout.fillWidth: true
                        onClicked: sendCode("(espled-setup " + pinBox.value + " " +
                                            lenBox.value + " " + typeBox.currentIndex + ")")
                    }
                    Button {
                        text: "Stop"
                        Layout.fillWidth: true
                        onClicked: sendCode("(ext-espled-deinit)")
                    }
                }
            }

            GroupBox {
                title: "Effect"
                Layout.fillWidth: true

                GridLayout {
                    anchors.fill: parent
                    columns: 2

                    Label { text: "Effect" }
                    ComboBox {
                        id: fxBox
                        model: ["Solid", "Breathe", "Chase", "Rainbow", "Sparkle", "Comet"]
                        Layout.fillWidth: true
                        onActivated: sendCode("(ext-espled-seg-fx 0 " + currentIndex + ")")
                    }

                    Label { text: "Palette" }
                    ComboBox {
                        id: palBox
                        model: ["RGBW", "Fire", "Ocean", "Neon", "Ember", "Traffic", "Strobe", "Police"]
                        Layout.fillWidth: true
                        onActivated: sendCode("(ext-espled-seg-pal 0 " + currentIndex + ")")
                    }

                    Label { text: "Speed " + spdSlider.value.toFixed(0) }
                    Slider {
                        id: spdSlider
                        from: 1; to: 255; value: 32
                        stepSize: 1
                        Layout.fillWidth: true
                        onPressedChanged: if (!pressed) sendCode("(ext-espled-seg-spd 0 " + value.toFixed(0) + ")")
                    }

                    Label { text: "Size " + sizeSlider.value.toFixed(0) }
                    Slider {
                        id: sizeSlider
                        from: 1; to: 64; value: 8
                        stepSize: 1
                        Layout.fillWidth: true
                        onPressedChanged: if (!pressed) sendCode("(ext-espled-seg-size 0 " + value.toFixed(0) + ")")
                    }
                }
            }

            GroupBox {
                title: "Color"
                Layout.fillWidth: true

                ColumnLayout {
                    anchors.fill: parent

                    Rectangle {
                        Layout.fillWidth: true
                        height: 24
                        radius: 4
                        color: Qt.rgba(rSlider.value / 255, gSlider.value / 255, bSlider.value / 255, 1)
                        border.color: "#808080"
                    }

                    GridLayout {
                        Layout.fillWidth: true
                        columns: 2

                        Label { text: "R " + rSlider.value.toFixed(0) }
                        Slider {
                            id: rSlider
                            from: 0; to: 255; value: 255
                            stepSize: 1
                            Layout.fillWidth: true
                            onPressedChanged: if (!pressed) sendColor()
                        }

                        Label { text: "G " + gSlider.value.toFixed(0) }
                        Slider {
                            id: gSlider
                            from: 0; to: 255; value: 0
                            stepSize: 1
                            Layout.fillWidth: true
                            onPressedChanged: if (!pressed) sendColor()
                        }

                        Label { text: "B " + bSlider.value.toFixed(0) }
                        Slider {
                            id: bSlider
                            from: 0; to: 255; value: 0
                            stepSize: 1
                            Layout.fillWidth: true
                            onPressedChanged: if (!pressed) sendColor()
                        }
                    }

                    RowLayout {
                        Layout.fillWidth: true

                        Button {
                            text: "Red"
                            Layout.fillWidth: true
                            onClicked: sendCode("(ext-espled-col-rgb 255 0 0)")
                        }
                        Button {
                            text: "Green"
                            Layout.fillWidth: true
                            onClicked: sendCode("(ext-espled-col-rgb 0 255 0)")
                        }
                        Button {
                            text: "Blue"
                            Layout.fillWidth: true
                            onClicked: sendCode("(ext-espled-col-rgb 0 0 255)")
                        }
                        Button {
                            text: "White"
                            Layout.fillWidth: true
                            onClicked: sendCode("(ext-espled-col-rgb 255 255 255)")
                        }
                        Button {
                            text: "Off"
                            Layout.fillWidth: true
                            onClicked: sendCode("(ext-espled-col 0)")
                        }
                    }
                }
            }

            GroupBox {
                title: "Global"
                Layout.fillWidth: true

                GridLayout {
                    anchors.fill: parent
                    columns: 2

                    Label { text: "Brightness " + briSlider.value.toFixed(0) }
                    Slider {
                        id: briSlider
                        from: 0; to: 255; value: 255
                        stepSize: 1
                        Layout.fillWidth: true
                        onPressedChanged: if (!pressed) sendCode("(ext-espled-bri " + value.toFixed(0) + ")")
                    }

                    Label { text: "Auto white" }
                    Switch {
                        id: awSwitch
                        onToggled: sendCode("(ext-espled-auto-white " + (checked ? 1 : 0) + ")")
                    }

                    Label { text: "Current limit (mA)" }
                    SpinBox {
                        id: abBox
                        from: 0; to: 20000; value: 0
                        stepSize: 100
                        editable: true
                        Layout.fillWidth: true
                        onValueModified: sendCode("(ext-espled-ablimit " + value + ")")
                    }
                }
            }
        }
    }
}
