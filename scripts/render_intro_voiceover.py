#!/usr/bin/env python3
"""Add a synthesized voiceover to the Probes tutorial's intro video.

    KOKORO_DIR=<dir> <venv>/bin/python scripts/render_intro_voiceover.py

speaks each phrase of scripts/intro_voiceover.txt with Kokoro (voice
bm_lewis, en-gb) at its start time and speed, mixes the phrases into one
track with ffmpeg, normalizes it to -16 LUFS, and muxes it as 48 kHz AAC
into src/web/www/img/hazel-probes-intro.mp4, replacing any audio there. The
video stream is copied, not re-encoded. After re-rendering the captions with
render_intro_captions.py (which writes a silent video), run this again.

A phrase is one Kokoro call, so it can span several captions and keep its
sentences' natural flow. The script stops if a phrase would run into the
next one (or past the video's end), and prints the schedule with the pause
before each phrase.

Setup (nothing of this is in the repo; Kokoro and its voices are Apache-2.0):
    python3 -m venv <venv> && <venv>/bin/pip install kokoro-onnx
    KOKORO_DIR holds kokoro-v1.0.onnx and voices-v1.0.bin, from the
    "model-files-v1.0" release of github.com/thewh1teagle/kokoro-onnx.
Use an arm64 Python on Apple silicon. Needs ffmpeg.
"""

import argparse
import atexit
import os
import re
import shutil
import subprocess
import sys
import tempfile
import wave

import numpy as np

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
IMG = os.path.join(ROOT, "src", "web", "www", "img")
HERE = os.path.dirname(os.path.abspath(__file__))
TARGET_LUFS = -16.0


def parse_time(s):
    m, sec = s.strip().split(":")
    return int(m) * 60 + float(sec)


def read_script(path):
    lines = []
    with open(path, encoding="utf-8") as f:
        for raw in f:
            raw = raw.rstrip("\n")
            if not raw.strip() or raw.startswith("#"):
                continue
            t, speed, text = raw.split("\t", 2)
            lines.append((parse_time(t), float(speed), text.strip()))
    return lines


def media_duration(path):
    out = subprocess.run(
        ["ffprobe", "-v", "error", "-select_streams", "v:0", "-show_entries",
         "stream=duration", "-of", "csv=p=0", path],
        capture_output=True, text=True, check=True).stdout
    return float(out.strip())


def loudness(path):
    """Integrated loudness (LUFS) by ffmpeg's ebur128 filter."""
    err = subprocess.run(["ffmpeg", "-hide_banner", "-nostats", "-i", path, "-af", "ebur128",
                          "-f", "null", "-"], capture_output=True, text=True, check=True).stderr
    return float(re.findall(r"I:\s+(-?[\d.]+) LUFS", err)[-1])


def make_kokoro(model_dir):
    import espeakng_loader
    from kokoro_onnx import Kokoro
    from kokoro_onnx.config import EspeakConfig
    # espeak-ng exits the process when its data path is long (a 192-character
    # path in a venv failed; 131 characters worked), so use a short copy then.
    data = espeakng_loader.get_data_path()
    if len(data) > 100:
        tmp = tempfile.mkdtemp(prefix="esd")
        atexit.register(shutil.rmtree, tmp, True)
        short = os.path.join(tmp, "espeak-ng-data")
        shutil.copytree(data, short)
        data = short
    config = EspeakConfig(data_path=data, lib_path=espeakng_loader.get_library_path())
    return Kokoro(os.path.join(model_dir, "kokoro-v1.0.onnx"),
                  os.path.join(model_dir, "voices-v1.0.bin"), espeak_config=config)


def trim(samples, sr, threshold=0.01, lead=0.03, tail=0.12):
    loud = np.where(np.abs(samples) > threshold)[0]
    if len(loud) == 0:
        return samples
    a = max(0, loud[0] - int(lead * sr))
    b = min(len(samples), loud[-1] + int(tail * sr))
    return samples[a:b]


def write_wav(path, samples, sr):
    pcm = (np.clip(samples, -1, 1) * 32767).astype("<i2")
    with wave.open(path, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(sr)
        w.writeframes(pcm.tobytes())


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--script", default=os.path.join(HERE, "intro_voiceover.txt"))
    ap.add_argument("--video", default=os.path.join(IMG, "hazel-probes-intro.mp4"),
                    help="video whose video stream is kept")
    ap.add_argument("--out", default=None, help="default: replace --video")
    ap.add_argument("--kokoro", default=os.environ.get("KOKORO_DIR"),
                    help="directory with kokoro-v1.0.onnx and voices-v1.0.bin")
    ap.add_argument("--voice", default="bm_lewis")
    ap.add_argument("--lang", default="en-gb")
    ap.add_argument("--gap", type=float, default=0.2, help="least silence between phrases, s")
    ap.add_argument("--bitrate", default="96k")
    ap.add_argument("--keep", help="directory to keep the line clips and the track in")
    args = ap.parse_args()
    if not args.kokoro:
        sys.exit("set KOKORO_DIR or pass --kokoro (see the header of this script)")
    out = args.out or args.video

    phrases = read_script(args.script)
    video_end = media_duration(args.video)

    kokoro = make_kokoro(args.kokoro)
    work = tempfile.mkdtemp(prefix="intro-voice-")
    try:
        schedule = []
        for i, (start, speed, text) in enumerate(phrases):
            samples, sr = kokoro.create(text, voice=args.voice, speed=speed, lang=args.lang)
            clip = trim(samples, sr)
            wav = os.path.join(work, f"phrase-{i + 1:02d}.wav")
            write_wav(wav, clip, sr)
            schedule.append(dict(n=i + 1, start=start, end=start + len(clip) / sr,
                                 speed=speed, text=text, wav=wav))

        print(f"{'#':>2} {'start':>6} {'end':>6} {'pause':>5} speed  phrase")
        prev_end, problems = 0.0, []
        for s in schedule:
            pause = s["start"] - prev_end
            print(f"{s['n']:>2} {s['start']:6.2f} {s['end']:6.2f} {pause:5.2f} {s['speed']:5.2f}  {s['text']}")
            if s["n"] > 1 and pause < args.gap:
                problems.append(f"phrase {s['n']} starts {args.gap - pause:.2f}s too early")
            prev_end = s["end"]
        if prev_end > video_end:
            problems.append(f"the last phrase ends {prev_end - video_end:.2f}s after the video")
        if problems:
            sys.exit("; ".join(problems))

        # One track: each clip delayed to its start, summed (they never overlap),
        # padded to the video's length.
        inputs, parts = [], []
        for k, s in enumerate(schedule):
            inputs += ["-i", s["wav"]]
            parts.append(f"[{k}:a]adelay={round(s['start'] * 1000)}:all=1[a{k}]")
        mix = "".join(f"[a{k}]" for k in range(len(schedule)))
        graph = ";".join(parts) + (
            f";{mix}amix=inputs={len(schedule)}:normalize=0:duration=longest,"
            f"apad,atrim=0:{video_end:.3f}[mix]")
        track = os.path.join(work, "track.wav")
        subprocess.run(["ffmpeg", "-y", "-hide_banner", "-loglevel", "error"] + inputs +
                       ["-filter_complex", graph, "-map", "[mix]", "-ar", "24000", track], check=True)

        # Normalize to TARGET_LUFS: a gain (loudnorm alone undershoots here, as the
        # gain it needs would clip the peaks) and a limiter at -1.5 dBFS, measured
        # again on the encoded track and corrected up to three times. Then 48 kHz AAC.
        aac = os.path.join(work, "voice.m4a")
        before = loudness(track)
        gain = TARGET_LUFS - before
        for _ in range(4):
            subprocess.run(["ffmpeg", "-y", "-hide_banner", "-loglevel", "error", "-i", track, "-af",
                            f"volume={gain:.2f}dB,alimiter=limit=0.841:level=false,aresample=48000",
                            "-ar", "48000", "-ac", "1", "-c:a", "aac", "-b:a", args.bitrate, aac], check=True)
            after = loudness(aac)
            if abs(after - TARGET_LUFS) <= 0.3:
                break
            gain += TARGET_LUFS - after

        muxed = os.path.join(work, "out.mp4")
        subprocess.run(["ffmpeg", "-y", "-hide_banner", "-loglevel", "error", "-i", args.video, "-i", aac,
                        "-map", "0:v:0", "-map", "1:a:0", "-c", "copy", "-map_metadata", "-1",
                        "-map_chapters", "-1", "-movflags", "+faststart", muxed], check=True)
        shutil.move(muxed, out)
        if args.keep:
            os.makedirs(args.keep, exist_ok=True)
            for name in os.listdir(work):
                if name.endswith((".wav", ".m4a")):
                    shutil.copy(os.path.join(work, name), args.keep)
        print(f"wrote {out} ({os.path.getsize(out) / 1e6:.2f} MB, {len(schedule)} phrases, "
              f"loudness {before:.1f} -> {after:.1f} LUFS)")
    finally:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
