#!/usr/bin/env python3
"""The documentation slides to time, as the deep-link slugs `?slide=` takes.

  python3 scripts/slide_timing/slides.py [--decks reference,livelits,playground,mvu,b2t2]

Read from the decks' Slides.re files, which Init.re puts in the
documentation in this order: the reference slides (src/docslides), the
livelits (src/livelitdemos, "Livelits / "), the Livelit Playground
("Livelit Playground / "), MVU ("MVU / ") and B2T2. The slug is
DeepLink.slug's: lowercase letters and digits, every other run of
characters one "-", none at the end. Default decks: reference, livelits,
playground (^ticket(470): "the documentation slides ... including each
livelit slide")."""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
ENTRY = re.compile(r'\(\s*"((?:[^"\\]|\\.)*)"\s*,\s*\[%blob\s+"[^"]+"\]\s*,?\s*\)', re.S)


def slug(name):
    out = re.sub(r"[^a-z0-9]+", "-", name.lower())
    return out.lstrip("-").rstrip("-")


def entries(path, start=None, end=None):
    s = open(os.path.join(ROOT, path)).read()
    if start:
        s = s[s.index(start):]
    if end and end in s:
        s = s[:s.index(end)]
    return [m.group(1) for m in ENTRY.finditer(s)]


DECKS = {
    "reference": lambda: entries("src/docslides/Slides.re", "let all_slides", "let mod_slides"),
    "livelits": lambda: ["Livelits / " + n for n in entries("src/livelitdemos/Slides.re", "let all_slides", "let playground_slides")],
    "playground": lambda: ["Livelit Playground / " + n for n in entries("src/livelitdemos/Slides.re", "let playground_slides")],
    "mvu": lambda: ["MVU / " + n for n in entries("src/mvu/Slides.re")],
    "b2t2": lambda: entries("src/b2t2/Slides.re"),
}

if __name__ == "__main__":
    decks = "reference,livelits,playground"
    if "--decks" in sys.argv:
        decks = sys.argv[sys.argv.index("--decks") + 1]
    for deck in decks.split(","):
        for name in DECKS[deck]():
            print(f"{deck}\t{slug(name)}\t{name}")
