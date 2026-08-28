#!/usr/bin/env python3
"""Generate ui.qml from ui.qml.in.

Fills {{VERSION}} from the `version` file and {{ABOUT_MD}} from README-gen.md,
escaped as a QML string literal. The About dialog runs that through
Utility.md2html(), the converter VESC Tool uses for the store page, so the two
cannot drift - and the build info comes along with it.

In Python rather than sed because the markdown is arbitrary text: sed would
need escaping for its delimiter, backslashes and '&'.
"""

import pathlib
import sys

HERE = pathlib.Path(__file__).parent
TEMPLATE = HERE / "ui.qml.in"
ABOUT = HERE / "README-gen.md"
VERSION = HERE / "version"
OUT = HERE / "ui.qml"

PLACEHOLDERS = ("{{VERSION}}", "{{ABOUT_MD}}")


def qml_string(text):
    """Escape text so it survives inside a QML double-quoted string literal."""
    return (
        text.replace("\\", "\\\\")
        .replace('"', '\\"')
        .replace("\r\n", "\n")
        .replace("\r", "\n")
        .replace("\n", "\\n")
    )


def main():
    for path in (TEMPLATE, ABOUT, VERSION):
        if not path.exists():
            sys.exit(f"gen_ui.py: missing {path.name}")

    out = TEMPLATE.read_text(encoding="utf-8")
    out = out.replace("{{VERSION}}", VERSION.read_text(encoding="utf-8").strip())
    out = out.replace("{{ABOUT_MD}}", qml_string(ABOUT.read_text(encoding="utf-8")))

    left = [p for p in PLACEHOLDERS if p in out]
    if left:
        sys.exit(f"gen_ui.py: placeholder(s) not substituted: {', '.join(left)}")

    OUT.write_text(out, encoding="utf-8")
    print(f"wrote {OUT.name} (about: {len(ABOUT.read_text(encoding='utf-8'))} chars of markdown)")


if __name__ == "__main__":
    main()
