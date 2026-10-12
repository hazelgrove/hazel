#!/usr/bin/env python3
"""Burn captions into the Probes tutorial's intro video.

    python3 scripts/render_intro_captions.py

reads src/web/www/img/hazel-probes-intro.ass and writes
src/web/www/img/hazel-probes-intro.mp4: H.264, yuv420p, faststart, the
source's size and timing, and no audio track.

The caption-free source is the original narrated recording, which stays in
git history (blob SOURCE_BLOB below); its audio is dropped. Pass --source to
use another file, --out to write elsewhere, --keep DIR to keep the caption
images.

Needs ffmpeg and Pillow. ffmpeg's subtitles filter needs libass, which the
Homebrew ffmpeg (8.0) this was made with lacks, so this script draws each
caption with Pillow (in Hazel's own Helvetica Neue) and overlays the images.
It reads the subset of the .ass format the caption file uses: one style,
BorderStyle 3 (a box in OutlineColour, padded by Outline), and the
{\\pos(x,y)}, {\\anN} and \\N tags.
"""

import argparse
import os
import re
import shutil
import subprocess
import sys
import tempfile

from PIL import Image, ImageDraw, ImageFont

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
IMG = os.path.join(ROOT, "src", "web", "www", "img")
FONTS = os.path.join(ROOT, "src", "web", "www", "style", "fonts")
# src/web/www/img/hazel-probes-intro.mp4 as first committed (fbc25e012a), with narration.
SOURCE_BLOB = "ad5abf4f15f327b6b7cef605ddc6051aef76dc88"
CORNER_RADIUS = 10
LINE_SPACING = 1.3


def ass_colour(s):
    """&HAABBGGRR (AA: 00 opaque, FF transparent) -> (R, G, B, A)."""
    v = int(s.strip().lstrip("&Hh").rstrip("&"), 16)
    a, b, g, r = (v >> 24) & 255, (v >> 16) & 255, (v >> 8) & 255, v & 255
    return (r, g, b, 255 - a)


def ass_time(s):
    h, m, sec = s.strip().split(":")
    return int(h) * 3600 + int(m) * 60 + float(sec)


def parse_ass(path):
    info, styles, events = {}, {}, []
    section, fmt = None, None
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if not line.strip() or line.startswith(";"):
                continue
            if line.startswith("["):
                section, fmt = line.strip(), None
                continue
            key, _, val = line.partition(":")
            key, val = key.strip(), val.strip()
            if section == "[Script Info]":
                info[key] = val
            elif key == "Format":
                fmt = [x.strip() for x in val.split(",")]
            elif section == "[V4+ Styles]" and key == "Style":
                style = dict(zip(fmt, [x.strip() for x in val.split(",")]))
                styles[style["Name"]] = style
            elif section == "[Events]" and key == "Dialogue":
                fields = dict(zip(fmt, val.split(",", len(fmt) - 1)))
                events.append(fields)
    return info, styles, events


def font_for(style):
    bold = style.get("Bold", "0") not in ("0", "")
    name = "HelveticaNeue-Bold.woff2" if bold else "HelveticaNeue-Regular.woff2"
    return ImageFont.truetype(os.path.join(FONTS, name), int(float(style["Fontsize"])))


def render_event(event, style, size):
    """One transparent full-frame PNG for one caption."""
    text = event["Text"]
    tags = "".join(re.findall(r"\{([^}]*)\}", text))
    text = re.sub(r"\{[^}]*\}", "", text).replace("\\h", " ")
    lines = text.split("\\N")
    align = int(style["Alignment"])
    m = re.search(r"\\an(\d)", tags)
    if m:
        align = int(m.group(1))
    m = re.search(r"\\pos\(\s*([-\d.]+)\s*,\s*([-\d.]+)\s*\)", tags)
    if not m:
        sys.exit(f"caption without \\pos: {event['Text']}")
    px, py = float(m.group(1)), float(m.group(2))

    font = font_for(style)
    pad = float(style["Outline"])
    ascent, descent = font.getmetrics()
    line_h = round(float(style["Fontsize"]) * LINE_SPACING)
    widths = [font.getlength(l) for l in lines]
    block_w = max(widths)
    block_h = (len(lines) - 1) * line_h + ascent + descent

    # The anchor (numpad layout, as in .ass) places the text block; the box pads it.
    col, row = (align - 1) % 3, (align - 1) // 3  # col 0 left, 1 center, 2 right; row 0 bottom
    left = px - block_w * (0, 0.5, 1)[col]
    top = py - block_h * (1, 0.5, 0)[row]

    img = Image.new("RGBA", size, (0, 0, 0, 0))
    draw = ImageDraw.Draw(img)
    box = [left - pad, top - pad, left + block_w + pad, top + block_h + pad]
    if box[0] < 0 or box[1] < 0 or box[2] > size[0] or box[3] > size[1]:
        sys.exit(f"caption box {box} leaves the {size} frame: {event['Text']}")
    colour = style["OutlineColour"] if style["BorderStyle"] == "3" else style["BackColour"]
    draw.rounded_rectangle(box, radius=CORNER_RADIUS, fill=ass_colour(colour))
    ink = ass_colour(style["PrimaryColour"])
    for i, (line, w) in enumerate(zip(lines, widths)):
        x = left + (block_w - w) * (0, 0.5, 1)[col]
        draw.text((x, top + i * line_h), line, font=font, fill=ink)
    return img


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--captions", default=os.path.join(IMG, "hazel-probes-intro.ass"))
    ap.add_argument("--source", help="caption-free video (default: the narrated original from git)")
    ap.add_argument("--out", default=os.path.join(IMG, "hazel-probes-intro.mp4"))
    ap.add_argument("--crf", default="20")
    ap.add_argument("--keep", help="directory to keep the caption PNGs in")
    args = ap.parse_args()

    info, styles, events = parse_ass(args.captions)
    size = (int(info["PlayResX"]), int(info["PlayResY"]))
    work = tempfile.mkdtemp(prefix="intro-captions-")
    try:
        source = args.source
        if not source:
            source = os.path.join(work, "source.mp4")
            with open(source, "wb") as f:
                subprocess.run(["git", "-C", ROOT, "cat-file", "blob", SOURCE_BLOB], stdout=f, check=True)

        inputs, chain, last = [], [], "0:v"
        for i, ev in enumerate(events, 1):
            png = os.path.join(work, f"caption-{i:02d}.png")
            render_event(ev, styles[ev["Style"]], size).save(png)
            start, end = ass_time(ev["Start"]), ass_time(ev["End"])
            inputs += ["-i", png]
            label = f"v{i}"
            chain.append(f"[{last}][{i}:v]overlay=0:0:enable='gte(t,{start})*lt(t,{end})'[{label}]")
            last = label

        cmd = (
            ["ffmpeg", "-y", "-hide_banner", "-loglevel", "error", "-i", source]
            + inputs
            + ["-filter_complex", ";".join(chain), "-map", f"[{last}]", "-an",
               "-c:v", "libx264", "-preset", "slow", "-crf", args.crf, "-profile:v", "high",
               "-pix_fmt", "yuv420p", "-fps_mode", "passthrough", "-video_track_timescale", "15360",
               "-map_metadata", "-1", "-map_chapters", "-1", "-movflags", "+faststart", args.out]
        )
        subprocess.run(cmd, check=True)
        if args.keep:
            os.makedirs(args.keep, exist_ok=True)
            for name in sorted(os.listdir(work)):
                if name.endswith(".png"):
                    shutil.copy(os.path.join(work, name), args.keep)
        print(f"wrote {args.out} ({os.path.getsize(args.out) / 1e6:.2f} MB, {len(events)} captions)")
    finally:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
