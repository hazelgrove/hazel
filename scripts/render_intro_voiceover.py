#!/usr/bin/env python3
"""Add a synthesized voiceover to the Probes tutorial's intro video.

    KOKORO_DIR=<dir> <venv>/bin/python scripts/render_intro_voiceover.py

speaks each line of scripts/intro_voiceover.txt with Kokoro (voice bm_lewis,
en-gb), starting at its caption's time, mixes the lines into one track with
ffmpeg, normalizes it to -16 LUFS, and muxes it as 48 kHz AAC into
src/web/www/img/hazel-probes-intro.mp4, replacing any audio there. The video
stream is copied, not re-encoded. After re-rendering the captions with
render_intro_captions.py (which writes a silent video), run this again.

A line runs at speed 1.0 unless it would run into the next caption's time;
then it is sped up, by steps of 0.05 up to --max-speed. A line that still
does not fit pushes the next line's start later. Lines never overlap, and the
script prints the schedule with any changes.

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
            t, text = raw.split("\t", 1)
            lines.append((parse_time(t), text.strip()))
    return lines


def caption_starts(ass_path):
    starts = []
    with open(ass_path, encoding="utf-8") as f:
        for line in f:
            if line.startswith("Dialogue:"):
                h, m, s = line.split(",")[1].split(":")
                starts.append(int(h) * 3600 + int(m) * 60 + float(s))
    return starts


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
    ap.add_argument("--captions", default=os.path.join(IMG, "hazel-probes-intro.ass"))
    ap.add_argument("--video", default=os.path.join(IMG, "hazel-probes-intro.mp4"),
                    help="video whose video stream is kept")
    ap.add_argument("--out", default=None, help="default: replace --video")
    ap.add_argument("--kokoro", default=os.environ.get("KOKORO_DIR"),
                    help="directory with kokoro-v1.0.onnx and voices-v1.0.bin")
    ap.add_argument("--voice", default="bm_lewis")
    ap.add_argument("--lang", default="en-gb")
    ap.add_argument("--max-speed", type=float, default=1.15)
    ap.add_argument("--gap", type=float, default=0.12, help="least silence between lines, s")
    ap.add_argument("--bitrate", default="96k")
    ap.add_argument("--keep", help="directory to keep the line clips and the track in")
    args = ap.parse_args()
    if not args.kokoro:
        sys.exit("set KOKORO_DIR or pass --kokoro (see the header of this script)")
    out = args.out or args.video

    lines = read_script(args.script)
    starts = caption_starts(args.captions)
    if [round(t, 2) for t, _ in lines] != [round(t, 2) for t in starts]:
        sys.exit("the spoken script's times do not match the captions' start times")
    video_end = media_duration(args.video)

    kokoro = make_kokoro(args.kokoro)
    work = tempfile.mkdtemp(prefix="intro-voice-")
    try:
        schedule, prev_end = [], 0.0
        for i, (cue, text) in enumerate(lines):
            limit = (lines[i + 1][0] if i + 1 < len(lines) else video_end) - args.gap
            start = max(cue, prev_end + args.gap)
            speed = 1.0
            while True:
                samples, sr = kokoro.create(text, voice=args.voice, speed=speed, lang=args.lang)
                clip = trim(samples, sr)
                dur = len(clip) / sr
                if start + dur <= limit or speed >= args.max_speed - 1e-9:
                    break
                speed = round(min(speed + 0.05, args.max_speed), 2)
            if i + 1 == len(lines) and start + dur > video_end:
                sys.exit(f"the last line runs past the video's end: {text}")
            wav = os.path.join(work, f"line-{i + 1:02d}.wav")
            write_wav(wav, clip, sr)
            schedule.append(dict(n=i + 1, cue=cue, start=start, dur=dur, speed=speed,
                                 end=start + dur, limit=limit + args.gap, text=text, wav=wav))
            prev_end = start + dur

        print(f"{'#':>2} {'caption':>7} {'voice':>7} {'end':>7} {'next':>7} speed  line")
        for s in schedule:
            notes = []
            if s["speed"] != 1.0:
                notes.append(f"sped up to {s['speed']}")
            if s["start"] > s["cue"] + 1e-6:
                notes.append(f"starts {s['start'] - s['cue']:.2f}s late")
            print(f"{s['n']:>2} {s['cue']:7.2f} {s['start']:7.2f} {s['end']:7.2f} {s['limit']:7.2f}"
                  f" {s['speed']:5.2f}  {s['text']}" + (f"   [{'; '.join(notes)}]" if notes else ""))

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
        print(f"wrote {out} ({os.path.getsize(out) / 1e6:.2f} MB, {len(schedule)} lines, "
              f"loudness {before:.1f} -> {after:.1f} LUFS)")
    finally:
        shutil.rmtree(work)


if __name__ == "__main__":
    main()
