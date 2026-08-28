#!/usr/bin/env python3
"""Generate esp_led_version.lisp from the `version` file.

float_accessories imports the compiled .bin files by path, so it needs a way
to name which build of the lib it carries. Both packages read this one file,
so neither can claim a version the binaries do not have.
"""

import pathlib
import re
import sys

HERE = pathlib.Path(__file__).parent
VERSION = HERE / "version"
OUT = HERE / "esp_led_version.lisp"

TEMPLATE = """; Generated from the `version` file by gen_version.py - do not edit.

@const-start

(def esp_led-version '({major} {minor} {patch}))

@const-end
"""


def main():
    if not VERSION.exists():
        sys.exit("gen_version.py: missing version")

    raw = VERSION.read_text(encoding="utf-8").strip()
    m = re.fullmatch(r"(\d+)\.(\d+)\.(\d+)", raw)
    if not m:
        sys.exit(f"gen_version.py: version must be MAJOR.MINOR.PATCH, got {raw!r}")

    major, minor, patch = m.groups()
    OUT.write_text(
        TEMPLATE.format(major=major, minor=minor, patch=patch), encoding="utf-8"
    )
    print(f"wrote {OUT.name} ({raw})")


if __name__ == "__main__":
    main()
