#!/usr/bin/env python3
"""Generate lib/version-gen.lisp from the `version` file.

The package version lives in one place: the `version` file. The Makefile
stamps it into README-gen.md, gen_ui.py substitutes it into the About box,
and this puts the same number in front of the lisp - which used to carry its
own hand-written copy in get-version and had already drifted a major version
behind.
"""

import pathlib
import re
import sys

HERE = pathlib.Path(__file__).parent
VERSION = HERE / "version"
OUT = HERE / "lib" / "version-gen.lisp"

TEMPLATE = """; Generated from the `version` file by gen_version.py - do not edit.

@const-start

(def pkg-version '({major} {minor} {patch}))

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
