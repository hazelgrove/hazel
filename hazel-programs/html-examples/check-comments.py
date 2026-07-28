#!/usr/bin/env python3
"""Catch unterminated Hazel comments in .hz files.

Hazel comments are single-line `# ... #`. A comment spanning two lines does not
error at the delimiter — it silently reparses the following text as code, which
surfaces later as a pile of unrelated static errors. A lone `#` on its own line
is the same trap: it opens a comment that closes on the next line's `#`.

Run this before ./hazel analyze while editing; regen-slides.sh also runs it so
a bad comment can never reach an encoded slide.

    ./check-comments.py *.hz
"""

import re
import sys

# Strip string literals first: they legitimately contain `#` (hex colours).
STRING = re.compile(r'"(?:[^"\\]|\\.)*"')


def dangling(path):
    with open(path) as f:
        return [
            (n, line.rstrip())
            for n, line in enumerate(f, 1)
            if STRING.sub('""', line).count("#") % 2
        ]


def main(paths):
    bad = False
    for path in paths:
        for n, line in dangling(path):
            print(f"{path}:{n}: unterminated comment: {line}", file=sys.stderr)
            bad = True
    if bad:
        print(
            "\nHazel comments must open and close on one line. To separate a "
            "title from a description, use a genuinely blank line — not a line "
            "holding a single `#`.",
            file=sys.stderr,
        )
    return 1 if bad else 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print(__doc__, file=sys.stderr)
        sys.exit(2)
    sys.exit(main(sys.argv[1:]))
