# FLOAT ACESSORIES PACKAGE

A VESC Express package for controlling LEDs, BMS, and Pubmote.

<H2>DISCLAIMER</H2>

This package is not endorsed by the vesc-project. Use at your own risk.

<H2>CREDITS</H2>

Special Thanks: Benjamin Vedder, surfdado, NuRxG, Siwoz, lolwheel (OWIE), ThankTheMaker (rESCue), 4_fools (avaspark), auden_builds (pubmote)
gr33tz: outlandnish, exphat, datboig42069
Beta Testers: Koddex, Pickles

<H2>RELEASE NOTES</H2>

Beta. Pubmote and and advanced LED control requires 6.05 firmware on VESC Express and VESC. Run float-accessories-code-server.lisp on the VESC to load code-server library before float package. There is a fallback mode for a basic LED feature without the firmware requirement on VESC side (i.e. no code-server)

There's a ui too.

The VESC Express seems to run out of memory if too many things are going on. Try disabling WiFi or removing some functionality in the script. Ideally someone will look into impreovments with vesc express since wifi is needed for pubmote feature.

<H3>BUILD INFO</H3>

Source code can be found here: https://github.com/relys/vesc_pkg

#### &nbsp;
#### Build Info
