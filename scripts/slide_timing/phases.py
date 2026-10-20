#!/usr/bin/env python3
"""Where each slide's script time went, by phase (^ticket(470)): the CPU profiles' frames from time_slides.mjs (self time
by the generated script's line and column, where each function starts) mapped through the bundle's inline source map to
the source file of each function, and the file to a phase.

  python3 scripts/slide_timing/phases.py timing.jsonl hazel.js worker.js > phases.md   (also writes phases.jsonl)
  python3 scripts/slide_timing/phases.py --self-test bundle.js                         (decode a map, print what it holds)

The bundles are the release build's, with --source-map-inline (the timing branch's src/web/dune). Phases, the first that
matches a function's source file: parse (text to tiles and terms), statics (elaboration is part of it here), evaluation (probes too),
projectors (livelits and the other projectors), layout (measuring, pretty-printing), view (the web app, its virtual DOM
and Bonsai), terms (the term types every phase shares), util, runtime (js_of_ocaml's runtime, the OCaml libraries).
Anything else is "other", and its files are listed."""
import base64, bisect, json, re, sys
from collections import defaultdict

PHASES = [
    # (the first that matches a source file; paths as the map writes them: /workspace_root/src/..., an opam library's
    # .../lib/<library>/..., and js_of_ocaml's own runtime as /builtin/...)
    ("parse", r"src/haz3lcore/(lang|tiles|zipper)/|src/menhirParser/|src/language/grammar/|FastParse|CachedSyntax|TermData|TermMap|EditIdentity|ImpliedHole"),
    ("statics", r"src/language/statics/|CachedStatics|DefStatics|src/haz3lcore/TyDi/|ProblemCollection|/Typ\.re|TypHash|Completion"),
    ("evaluation", r"src/language/(dynamics|builtins|proof|derivation)/|Probe|Environment|Filter"),
    ("projectors", r"src/haz3lcore/projectors/|src/livelitdemos/|Livelit|Projector"),
    ("layout", r"src/haz3lcore/pretty/|Measured|Indentation"),
    ("view", r"src/web/|src/mvu/|CompositionView|/lib/(virtual_dom|incr_dom|bonsai|incremental|incr_map|incr_select|async_js)/"),
    ("agent", r"CompositionCore/|Agent"),
    ("terms", r"src/language/|src/haz3lcore/"),
    ("util", r"src/util/"),
    ("runtime", r"^/builtin/|/lib/"),
]
PHASE_RE = [(name, re.compile(pat)) for name, pat in PHASES]
B64 = {c: i for i, c in enumerate("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")}


def phase_of(path):
    for name, rx in PHASE_RE:
        if rx.search(path or ""):
            return name
    return "other"


def vlq(segment):
    out, val, shift = [], 0, 0
    for ch in segment:
        d = B64[ch]
        val += (d & 31) << shift
        if d & 32:
            shift += 5
        else:
            out.append(-(val >> 1) if val & 1 else val >> 1)
            val, shift = 0, 0
    return out


class SourceMap:
    """A map from the generated script's (line, column) to a source file, from a plain or an index (sections) map."""

    def __init__(self, m):
        self.lines = defaultdict(list)  # generated line -> sorted [(column, source path)]
        if "sections" in m:
            for sec in m["sections"]:
                self._add(sec["map"], sec["offset"]["line"], sec["offset"]["column"])
        else:
            self._add(m, 0, 0)
        for segs in self.lines.values():
            segs.sort()
        self.cols = {k: [c for c, _ in v] for k, v in self.lines.items()}

    def _add(self, m, line0, col0):
        sources = [(m.get("sourceRoot") or "") + s for s in m.get("sources", [])]
        src = 0
        for i, line in enumerate(m.get("mappings", "").split(";")):
            col = 0
            for seg in line.split(","):
                if not seg:
                    continue
                f = vlq(seg)
                col += f[0]
                if len(f) >= 4:
                    src += f[1]
                    self.lines[line0 + i].append(((col0 if i == 0 else 0) + col, sources[src] if 0 <= src < len(sources) else None))

    def source(self, line, col):
        segs = self.lines.get(line)
        if not segs:
            return None
        k = bisect.bisect_right(self.cols[line], col) - 1
        return segs[k][1] if k >= 0 else segs[0][1]


def load(path):
    text = open(path, encoding="utf-8", errors="replace").read()
    m = re.findall(r"sourceMappingURL=data:application/json(?:;charset=[^;,]+)?;base64,([A-Za-z0-9+/=]+)", text)
    if not m:
        return None
    return SourceMap(json.loads(base64.b64decode(m[-1]).decode("utf-8", "replace")))


def split(frames, smap):
    """[(url, line, col, fn, ms)] -> ({phase: ms}, {file: ms}) for the frames of this bundle."""
    by_phase, by_file = defaultdict(float), defaultdict(float)
    for url, line, col, fn, ms in frames:
        src = smap.source(line, col) if smap else None
        by_phase[phase_of(src) if src else "unmapped"] += ms
        by_file[src or f"(unmapped) {fn}"] += ms
    return by_phase, by_file


def main():
    if sys.argv[1] == "--self-test":
        smap = load(sys.argv[2])
        if not smap:
            sys.exit("no inline source map")
        files = {s for segs in smap.lines.values() for _, s in segs}
        counts = defaultdict(int)
        for f in files:
            counts[phase_of(f)] += 1
        print(f"{len(smap.lines)} generated lines mapped, {len(files)} source files; by phase: {dict(counts)}")
        for name in [n for n, _ in PHASES] + ["other"]:
            ex = sorted(f for f in files if f and phase_of(f) == name)[:6]
            print(f"  {name}: {ex}")
        return
    rows = [json.loads(l) for l in open(sys.argv[1]) if l.strip()]
    maps = {"hazel.js": load(sys.argv[2]), "worker.js": load(sys.argv[3])}
    names = [n for n, _ in PHASES] + ["other", "unmapped"]
    out = open("phases.jsonl", "w")
    print("### Where the script time went, by phase (from the source maps)\n")
    print("Each slide's page and eval-worker JavaScript, its functions' self time split by the phase of the source file "
          "each function is in. Seconds; the largest phase in bold.\n")
    print("| slide | " + " | ".join(names) + " | dominant |")
    print("|---|" + "---:|" * len(names) + "---|")
    others = defaultdict(float)
    for r in rows:
        total = defaultdict(float)
        for part in [r.get("main")] + list(r.get("workers") or []):
            if not part or not part.get("frames"):
                continue
            for bundle, smap in maps.items():
                fr = [f for f in part["frames"] if f[0].endswith(bundle)]
                p, files = split(fr, smap)
                for k, v in p.items():
                    total[k] += v
                for f, v in files.items():
                    if phase_of(f) == "other" or f.startswith("(unmapped)"):
                        others[f] += v
        busy = sum(total.values()) or 1
        top = max(total.items(), key=lambda kv: kv[1]) if total else ("-", 0)
        verdict = top[0] if top[1] >= busy / 2 else f"mixed ({top[0]} {100 * top[1] // busy:.0f}%)"
        cells = [f"**{total[n] / 1000:.1f}**" if n == top[0] else f"{total[n] / 1000:.1f}" for n in names]
        print(f"| {r.get('name')} | " + " | ".join(cells) + f" | {verdict} |")
        out.write(json.dumps({"slug": r.get("slug"), "name": r.get("name"), "open_ms": r.get("open_ms"),
                              "phases_ms": {k: round(v) for k, v in total.items()}, "dominant": verdict}) + "\n")
    if others:
        print("\nThe files in \"other\" and \"unmapped\", by time over all slides (for the phase table above to learn them):\n")
        for f, v in sorted(others.items(), key=lambda kv: -kv[1])[:25]:
            print(f"- `{f}`: {v / 1000:.1f} s")


if __name__ == "__main__":
    main()
