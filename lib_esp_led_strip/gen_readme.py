#!/usr/bin/env python3
"""Rewrite README.md into the markdown dialect VESC Tool actually renders.

README.md is canonical and GitHub-flavoured. VESC Tool renders with the
bundled maddy parser (Utility::md2html), which disagrees with GFM twice:

  Tables. maddy has no GFM pipe tables - a `| a | b |` block falls through
  to the paragraph parser and every row joins into one run-on <p>. Its own
  syntax is `|table>` / `- | - | -` / `|<table`, which GitHub cannot read.

  Underscores. maddy pairs `_` into <em> across the whole line, code spans
  included, and has no escape - a backslash stays literal. Any line naming
  two ext-esp_led-* extensions lost its underscores. The numeric entity
  survives maddy, but shows literally inside GitHub's code spans.

Neither dialect serves both renderers, so the translation happens here and
README-gen.md is what ships. Fenced code blocks pass through untouched:
maddy does not inline-parse them, so an entity there would show literally.
"""

import pathlib
import re
import sys

HERE = pathlib.Path(__file__).parent
SRC = HERE / "README.md"
OUT = HERE / "README-gen.md"

FENCE = re.compile(r"^\s*```")
# A GFM delimiter row: |---|---| or | --- | :--: |, any column count.
TABLE_DELIM = re.compile(r"^\s*\|(?:\s*:?-{2,}:?\s*\|)+\s*$")
TABLE_ROW = re.compile(r"^\s*\|.*\|\s*$")


def split_row(line):
    """GFM row -> its cells, without the leading/trailing pipe's empty ends."""
    cells = line.strip().split("|")
    if cells and not cells[0].strip():
        cells = cells[1:]
    if cells and not cells[-1].strip():
        cells = cells[:-1]
    return [c.strip() for c in cells]


def convert_tables(lines):
    """Rewrite GFM pipe tables as maddy `|table>` blocks.

    The delimiter row is dropped rather than translated: maddy has no
    alignment, and its separator is the literal `- | - | -` at any width.
    """
    out = []
    i = 0
    while i < len(lines):
        if (
            TABLE_ROW.match(lines[i])
            and i + 1 < len(lines)
            and TABLE_DELIM.match(lines[i + 1])
        ):
            out.append("|table>")
            out.append("|".join(split_row(lines[i])))
            out.append("- | - | -")
            i += 2
            while i < len(lines) and TABLE_ROW.match(lines[i]):
                out.append("|".join(split_row(lines[i])))
                i += 1
            out.append("|<table")
        else:
            out.append(lines[i])
            i += 1
    return out


def convert(md):
    out = []
    block = []
    in_fence = False

    def flush():
        if block:
            out.extend(convert_tables(block))
            block.clear()

    for line in md.splitlines():
        if FENCE.match(line):
            flush()
            in_fence = not in_fence
            out.append(line)
            continue
        if in_fence:
            out.append(line)
        else:
            # No intentional _emphasis_ in this README, so every underscore
            # is part of a name that has to survive verbatim.
            block.append(line.replace("_", "&#95;"))

    flush()
    if in_fence:
        sys.exit("gen_readme.py: unclosed ``` fence in README.md")
    return "\n".join(out) + "\n"


def main():
    if not SRC.exists():
        sys.exit("gen_readme.py: missing README.md")

    md = SRC.read_text(encoding="utf-8")
    OUT.write_text(convert(md), encoding="utf-8")
    print(f"wrote {OUT.name} ({len(md)} chars in)")


if __name__ == "__main__":
    main()
