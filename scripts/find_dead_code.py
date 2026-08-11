#!/usr/bin/env python3
"""Find definitions in src/ and test/ that nothing references.

OCaml's warning 32 ("unused value declaration") is per-compilation-unit. A
top-level `let` in a .re file with no accompanying .rei is part of that module's
public interface, so the compiler cannot conclude it is unused -- another module
might call it. Hazel has ~9 .rei files against ~485 .re, so essentially every
top-level binding is exported and invisible to warning 32. No dune warning
setting changes this, including the strict release profile CI gates on.

This script closes that gap using ocaml-index, which dune already builds (the
`@ocaml-index` alias is wired into the dev-helper, watch, test, test-quick and
watch-test Makefile targets). Each library's index records occurrences located
in its own files but keyed by the *defining* uid, which may belong to another
library, so the union of all indexes is complete cross-library reference data.

    make dead-code

THE PREDICATE

    A uid is dead iff every recorded occurrence lies within the source span of
    its own definition.

The definition site and all self-recursive calls fall inside the span by
construction, so a function whose only callers are itself is correctly reported.
That was the largest blind spot of the text-scanning approach this replaces.

WHAT THIS WILL NOT CATCH

  - Unused variant constructors, record fields, and types. Constructor
    declarations are indexed, but [@deriving] emits ghost occurrences for every
    constructor at the type declaration's span, and the ghost filter discards
    them -- so a constructor matched only by generated code would look unused.
    Detecting these needs a separate, riskier rule. Not attempted.
  - Unused module / module type / exception / external / class bindings.
  - Pattern bindings: `let (a, b) = ...`, `let {x, _} = ...`.
  - Mutually recursive dead groups. `let rec a = ... b ... and b = ... a ...`
    with no external caller: each member's occurrence in the other's span counts
    as a use, so neither is reported. Self-recursive-only functions ARE caught.
  - Anything suppressed below, notably values reached through a
    signature-ascribed module (`module M: Sig = {...}` -- every *Proj.re looks
    like this) or passed to a functor.
  - Modules that have a .rei. Those are deferred to the compiler, which reports
    them correctly via warning 32 (a hard error in the release profile). Note
    the converse when deleting by hand: a .rei declaration does NOT keep an
    implementation alive as far as this tool is concerned, so removing a
    reported definition from such a module means removing its declaration too.
  - Dead modules. This works at definition granularity; a module every one of
    whose members is used only by its siblings is invisible here.

SUPPRESSION

Suppressed candidates are classified, not dropped; pass --show-suppressed to see
them with their reason. The `ppx-name` rule is blunt and will hide a genuinely
dead `pp_foo`. It is also non-negotiable: the index has no record of
ppx-generated call sites at all. RichProbeRegistry.yojson_of_packed_action has
exactly one occurrence in the entire union -- its own definition -- even though
the [@deriving yojson] on ProbeProj.re's `action` type generates a call to it.
Deleting it on that evidence breaks the build.

CAVEATS

  - Results are per-profile. ExerciseSettings_instructor vs _student change what
    is reachable; run both if you care.
  - Never analyse an `--instrument-with bisect_ppx` build; uid numbering differs.
  - This is a "no recorded reference" tool, not a reachability analysis. Delete,
    then build. The compiler is the oracle.
"""

import argparse
import collections
import json
import os
import re
import subprocess
import sys

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
BUILD = os.path.join(REPO, "_build")
DEFAULT = os.path.join(BUILD, "default")

# ocaml-index dump grammar. Kept as a pair of searches rather than a line
# grammar because a uid and its first location can share a line.
UID_RE = re.compile(r"uid: ([^;]+); locs:")
LOC_RE = re.compile(
    r'"((?:[^"\\]|\\.)*)": File "((?:[^"\\]|\\.)*)", line (\d+), characters (\d+)-(\d+)'
)
HEADER_RE = re.compile(r"^\d+ uids:$")

# The identifier must be the first token after a line-initial let / let rec / and.
BINDER = re.compile(r"^\s*(let|and)\s+(rec\s+)?$")
# ...and be followed by a real `=`. Rejects signature declarations inside inline
# module types, e.g. `module Exp: { let pat_of_mpat: ... => AST.pat; }`.
BINDS = re.compile(r"(?<![=<>!:+\-*/&|^])=(?![=>])")
CHAIN = re.compile(r"^(let|type|module|exception|external|and|class)\b")
CLOSER = re.compile(r"^[}\])>;]")

MOD_PLAIN = re.compile(r"^\s*(?:module\s+(?:rec\s+)?|and\s+)([A-Z]\w*)\s*=\s*\{\s*$")
MOD_SIG = re.compile(r"^\s*(?:module\s+(?:rec\s+)?|and\s+)([A-Z]\w*)\s*:")

# Derived from the (pps ...) stanzas across the six library dune files:
# ppx_yojson_conv, ppx_sexp_conv, ppx_deriving.show, ppx_deriving.eq,
# ppx_enumerate, ppx_variants_conv, ppx_deriving_qcheck.
PPX_NAME = re.compile(
    r"^(yojson_of|sexp_of|pp|show|equal|compare|hash_fold|hash|all_of|gen|arb|variants_of)(_|$)"
    r"|_(of_yojson|of_sexp)$"
    r"|^(t_of_sexp|t_of_yojson|of_yojson|to_yojson|all)$"
)
GENSYM = re.compile(r"__\d+_$")  # ppxlib temporaries: arg0__014_, sexp__010_

ENTRY_FILES = {"src/CLI/Cli.re", "src/web/Main.re", "src/web/Worker.re"}
# Copied into place by Makefile:29-35; they look orphaned but are not.
ENTRY_GLOB = re.compile(r"^src/web/exercises/settings/(ExerciseSettings|TutorialSettings)")

LIBDIR = {
    "util": "src/util",
    "language": "src/language",
    "haz3lcore": "src/haz3lcore",
    "b2t2": "src/b2t2",
    "menhirparser": "src/menhirParser",
    "web": "src/web",
}

# Source path -> owning index, most specific first. Used for the freshness check.
INDEX_OWNERS = [
    ("src/web/Main.re", "src/web/.main.eobjs/cctx.ocaml-index"),
    ("src/web/Worker.re", "src/web/.worker.eobjs/cctx.ocaml-index"),
    ("src/web/WorkerServer.re", "src/web/.workerServer.objs/cctx.ocaml-index"),
    ("src/util/", "src/util/.util.objs/cctx.ocaml-index"),
    ("src/language/", "src/language/.language.objs/cctx.ocaml-index"),
    ("src/haz3lcore/", "src/haz3lcore/.haz3lcore.objs/cctx.ocaml-index"),
    ("src/b2t2/", "src/b2t2/.b2t2.objs/cctx.ocaml-index"),
    ("src/menhirParser/", "src/menhirParser/.menhirParser.objs/cctx.ocaml-index"),
    ("src/CLI/", "src/CLI/.cli.eobjs/cctx.ocaml-index"),
    ("src/web/", "src/web/.web.objs/cctx.ocaml-index"),
    ("test/", "test/.haz3ltest.eobjs/cctx.ocaml-index"),
]


def die(msg, code=2):
    print(f"find_dead_code: {msg}", file=sys.stderr)
    sys.exit(code)


def find_indexes():
    out = []
    for root, _dirs, files in os.walk(BUILD):
        for fn in files:
            if fn.endswith(".ocaml-index"):
                out.append(os.path.join(root, fn))
    return sorted(out)


def check_freshness():
    """Advisory per-library mtime check.

    `dune build @ocaml-index` is the actual freshness guarantee -- this runs by
    default and the make targets run it explicitly. This check is a canary for
    the `--no-build` path only, and it is deliberately non-fatal: dune rewrites
    an index only when content changes, so mtime(source) > mtime(index) is
    ambiguous between "the index is stale" and "dune correctly had nothing to
    do" (a `touch`, a git checkout, an editor save that changed nothing). It
    cannot tell those apart, so it warns rather than failing.

    Compare each source against the index that actually owns it. A global
    newest-source-vs-oldest-index check false-alarms, because `make dev` runs
    setup-instructor, which rewrites the settings files after src/util's index
    was built.
    """
    stale = []
    for top in ("src", "test"):
        for root, _dirs, files in os.walk(os.path.join(REPO, top)):
            for fn in files:
                if not fn.endswith((".re", ".rei", ".ml", ".mli")):
                    continue
                path = os.path.join(root, fn)
                rel = os.path.relpath(path, REPO)
                idx = next((i for p, i in INDEX_OWNERS if rel.startswith(p)), None)
                if idx is None:
                    continue
                idx_path = os.path.join(DEFAULT, idx)
                if not os.path.isfile(idx_path):
                    continue
                if os.path.getmtime(path) > os.path.getmtime(idx_path):
                    stale.append(rel)
    if stale:
        print(
            f"find_dead_code: warning: {len(stale)} source file(s) are newer than "
            f"their index. If you edited them, run "
            f"`dune build @ocaml-index --profile dev` first; if you only touched "
            f"them, ignore this.",
            file=sys.stderr,
        )
        for rel in sorted(stale)[:10]:
            print(f"  {rel}", file=sys.stderr)
        if len(stale) > 10:
            print(f"  ... and {len(stale) - 10} more", file=sys.stderr)


def load_occurrences(indexes):
    """uid -> set of (path, line, col_start, col_end, name)."""
    occ = collections.defaultdict(set)
    total_lines = 0
    bad_lines = 0
    for idx in indexes:
        try:
            proc = subprocess.run(
                ["ocaml-index", "dump", idx],
                capture_output=True,
                text=True,
                check=True,
            )
        except FileNotFoundError:
            die("ocaml-index not found on PATH (it is a declared dep in dune-project)")
        except subprocess.CalledProcessError as e:
            die(f"ocaml-index dump failed on {idx}: {e.stderr.strip()}")

        lines = proc.stdout.split("\n")
        if not lines or not HEADER_RE.match(lines[0].strip()):
            die(
                f"ocaml-index dump format changed: {idx} does not start with "
                f"'N uids:' (got {lines[0][:60]!r})"
            )
        cur = None
        for ln in lines:
            if not ln.strip():
                continue
            total_lines += 1
            matched = False
            m = UID_RE.search(ln)
            if m:
                cur = m.group(1).strip()
                matched = True
            for lm in LOC_RE.finditer(ln):
                matched = True
                if cur is None:
                    continue
                name, path, line, a, b = lm.groups()
                occ[cur].add((path, int(line), int(a), int(b), name))
            if not matched:
                bad_lines += 1

    # Three lines per file are legitimately non-conforming (the header, the
    # `{uid:` opener, and the trailer). Anything beyond a rounding error means
    # the grammar moved under us.
    if total_lines and bad_lines / total_lines > 0.001:
        die(
            f"ocaml-index dump format changed: {bad_lines}/{total_lines} lines "
            f"({100 * bad_lines / total_lines:.2f}%) matched neither the uid nor "
            f"the location pattern"
        )
    return occ


def build_module_map():
    """Module basename -> candidate source paths.

    Restricted to src/ and test/ on purpose. Including .claude/worktrees/ (four
    complete repo copies) made ~12,700 uids unresolvable, silently hiding all
    test-side usage.
    """
    by_mod = collections.defaultdict(list)
    for top in ("src", "test"):
        for root, _dirs, files in os.walk(os.path.join(REPO, top)):
            for fn in files:
                if fn.endswith((".re", ".ml")):
                    rel = os.path.relpath(os.path.join(root, fn), REPO)
                    mod = fn.rsplit(".", 1)[0]
                    by_mod[mod[0].upper() + mod[1:]].append(rel)
    return by_mod


# A module packed as a first-class value, or aliased under another name, has a
# contract this tool cannot see: the signature it is checked against lives at
# the use site. `(module WorkerMessagingSection)` makes every member of that
# FILE required by DebugSection.S; `module Input = Input;` inside an
# `Attr.Hooks.Make({...})` argument makes every member of Input required by the
# hook's module type. Both produced build breaks before this rule existed.
# The alias form must be INDENTED to count: a nested `module Input = Input;`
# is a functor/structure argument, whereas a column-0 one is an ordinary
# re-export. Util.re alone has 42 of the latter, and counting those would
# suppress every definition in src/util.
FIRST_CLASS = re.compile(r"\(module\s+([A-Z]\w*)")
MOD_ALIAS = re.compile(r"^\s+module\s+[A-Z]\w*\s*=\s*([A-Z]\w*)\s*;")


class Analyzer:
    def __init__(self, occ, by_mod):
        self.occ = occ
        self.by_mod = by_mod
        self._src = {}
        self._contracted = None
        self._literals = {}

    def contracted_modules(self):
        """Module names whose members are required by an off-site signature."""
        if self._contracted is None:
            names = set()
            for top in ("src", "test"):
                for root, _dirs, files in os.walk(os.path.join(REPO, top)):
                    for fn in files:
                        if not fn.endswith((".re", ".rei", ".ml", ".mli")):
                            continue
                        with open(os.path.join(root, fn), encoding="latin-1") as f:
                            for ln in f:
                                names.update(FIRST_CLASS.findall(ln))
                                m = MOD_ALIAS.match(ln)
                                if m:
                                    names.add(m.group(1))
            self._contracted = names
        return self._contracted

    def src(self, path):
        """Source lines, read as latin-1.

        ocaml-index columns are BYTE offsets. Decoding as UTF-8 shifts every
        column past a multibyte character -- Node.text("·") in TextAreaProj.re
        was enough to produce false positives.
        """
        if path not in self._src:
            self._src[path] = None
            for base in (REPO, DEFAULT):
                fp = os.path.join(base, path)
                if os.path.isfile(fp):
                    with open(fp, encoding="latin-1") as f:
                        self._src[path] = f.read().split("\n")
                    break
        return self._src[path]

    def literal_lines(self, path):
        """Line numbers (1-based) whose content sits inside a multi-line
        `{|...|}` / `{id|...|id}` string literal, excluding the opening line.

        Indentation means nothing inside a quoted-string extension, and this
        repo's tests embed whole Hazel programs at column 0. Treating those as
        dedented terminators truncated a definition mid-literal and produced a
        syntax error in Test_Coverage.re.
        """
        if path not in self._literals:
            lines = self.src(path)
            inside = set()
            delim = None
            for n, line in enumerate(lines or [], start=1):
                if delim is None:
                    m = re.search(r"\{([a-z_]*)\|", line)
                    if m and ("|" + m.group(1) + "}") not in line[m.end():]:
                        delim = "|" + m.group(1) + "}"
                else:
                    inside.add(n)
                    if delim in line:
                        delim = None
            self._literals[path] = inside
        return self._literals[path]

    def has_interface(self, path):
        """Does this module have a sibling .rei/.mli?

        If so, defer to the compiler: warning 32 already reports unused values
        in an interfaced module, and it is right where this tool is not. A
        declaration in the interface is a use the index does not connect to the
        implementation, and the interface may require values indirectly via
        `include SOME_MODULE_TYPE` -- TableRenderer.rei does exactly that, which
        made its 164-line `render` look like the single biggest finding in the
        repo when it is required by RichProbe.RichProbe.
        """
        stem = path.rsplit(".", 1)[0]
        return any(
            os.path.isfile(os.path.join(REPO, stem + ext)) for ext in (".rei", ".mli")
        )

    def deffile(self, uid):
        parts = uid.split(".")[0].split("__")
        mod = parts[-1]
        lib = parts[0].lower() if len(parts) > 1 else None
        if len(parts) >= 3 and parts[0] == "Dune":  # Dune__exe__Cli
            lib = None
        cands = self.by_mod.get(mod, [])
        if len(cands) == 1:
            return cands[0]
        if not cands:
            return None
        # Four basenames collide across libraries: Id, Sort, Secondary, Message.
        if lib in LIBDIR:
            scoped = [p for p in cands if p.startswith(LIBDIR[lib] + "/")]
            if len(scoped) == 1:
                return scoped[0]
        return None

    def is_real(self, o):
        """Reject ghost locations: ppx-generated definitions point at the
        [@deriving] type declaration, whose span is far longer than the name.
        Unreadable files fail safe -- an occurrence we cannot check counts as a
        use."""
        path, line, a, b, name = o
        lines = self.src(path)
        if lines is None:
            return True
        if (b - a) != len(name) or line > len(lines) or b > len(lines[line - 1]):
            return False
        return lines[line - 1][a:b] == name.encode("utf-8").decode("latin-1")

    @staticmethod
    def indent(s):
        return len(s) - len(s.lstrip())

    def span(self, path, defline):
        """Indentation-delimited. Exact for refmt output, which every build
        target enforces via `@src/fmt --auto-promote`.

        A dedented line that opens with a closing delimiter still belongs to
        the definition: Reason puts the `};` of a braced body back at the
        binding's own indent, so a naive indent<=k rule stops one line short
        and would leave a dangling closer behind. Require indent == k for that
        exemption -- a closer at a *lower* indent belongs to an enclosing
        module, and following it would swallow everything after it.
        """
        lines = self.src(path)
        literals = self.literal_lines(path)
        k = self.indent(lines[defline - 1])
        for i in range(defline, len(lines)):
            if not lines[i].strip() or (i + 1) in literals:
                continue
            ind = self.indent(lines[i])
            if ind <= k:
                if ind == k and CLOSER.match(lines[i].lstrip()):
                    continue
                return (defline, i)
        return (defline, len(lines))

    def chain_head(self, path, defline):
        """For an `and` binding, the head keyword of its chain. Distinguishes
        `type a = ... and b = ...` (reject) from `let rec f = ... and g = ...`
        (accept)."""
        lines = self.src(path)
        k = self.indent(lines[defline - 1])
        for i in range(defline - 2, -1, -1):
            s = lines[i]
            if not s.strip() or self.indent(s) > k:
                continue
            if self.indent(s) < k:
                return "?"
            m = CHAIN.match(s.lstrip())
            if not m or m.group(1) == "and":
                continue
            return m.group(1)
        return "?"

    def openers(self, path, defline):
        """Innermost-first, the lines opening each enclosing block."""
        lines = self.src(path)
        k = self.indent(lines[defline - 1])
        out = []
        for i in range(defline - 2, -1, -1):
            s = lines[i]
            if not s.strip():
                continue
            if self.indent(s) < k:
                out.append(s)
                k = self.indent(s)
                if k == 0:
                    break
        return out

    def scope_of(self, path, defline, defindent):
        if defindent == 0:
            return "toplevel", []
        mods = []
        for s in self.openers(path, defline):
            m = MOD_PLAIN.match(s)
            if m:
                mods.append(m.group(1))
                continue
            if MOD_SIG.match(s):
                return "sig-module", mods
            if re.search(r"\(\s*\{?\s*$", s) or "({" in s:
                return "functor-or-call", mods
            return "expression", mods
        return "module", mods

    def run(self):
        rows = []
        skipped = collections.Counter()
        for uid, occs in self.occ.items():
            path = self.deffile(uid)
            if path is None:
                skipped["external/unmapped"] += 1
                continue
            lines = self.src(path)
            if not path.endswith((".re", ".ml")) or lines is None:
                skipped["no-impl-source"] += 1
                continue

            cands = []
            for o in occs:
                if o[0] != path or not self.is_real(o) or o[1] > len(lines):
                    continue
                line = lines[o[1] - 1]
                m = BINDER.match(line[: o[2]])
                if not m:
                    continue
                if not BINDS.search(line[o[3]:]):
                    continue
                if o[4][:1].isupper():  # `and Exp: {...}` in a module rec chain
                    continue
                if m.group(1) == "and" and self.chain_head(path, o[1]) != "let":
                    continue
                cands.append(o)
            if not cands:
                skipped["not-a-let-binding"] += 1
                continue

            d = min(cands, key=lambda o: (o[1], o[2]))
            name = d[4]
            lo, hi = self.span(path, d[1])
            uses = [
                o
                for o in occs
                if self.is_real(o) and not (o[0] == path and lo <= o[1] <= hi)
            ]
            in_test = [o for o in uses if o[0].startswith("test/")]
            if [o for o in uses if not o[0].startswith("test/")]:
                continue
            area = "test" if path.startswith("test/") else "src"
            if area == "test" and uses:
                continue
            kind = "DEAD" if not uses else "TEST-ONLY"

            defindent = self.indent(lines[d[1] - 1])
            scope, mods = self.scope_of(path, d[1], defindent)
            sup = []
            if path in ENTRY_FILES or ENTRY_GLOB.match(path):
                sup.append("entry-point")
            if self.has_interface(path):
                sup.append("has-interface")
            filemod = os.path.basename(path).rsplit(".", 1)[0]
            filemod = filemod[0].upper() + filemod[1:]
            if self.contracted_modules() & set(mods + [filemod]):
                sup.append("contracted-module")
            if PPX_NAME.search(name):
                sup.append("ppx-name")
            if GENSYM.search(name) or name.startswith("_"):
                sup.append("gensym")
            if scope in ("sig-module", "functor-or-call", "expression"):
                sup.append("scope:" + scope)

            rows.append(
                dict(
                    uid=uid,
                    file=path,
                    line=d[1],
                    name=name,
                    qual=".".join(mods + [name]) if mods else name,
                    kind=kind,
                    area=area,
                    loc=hi - lo + 1,
                    span=[lo, hi],
                    scope=scope,
                    sup=sup,
                    n_occurrences=len(occs),
                    n_test_uses=len(in_test),
                )
            )
        return rows, skipped


def sections(rows, include_tests):
    kept = [r for r in rows if not r["sup"]]
    out = [
        ("DEAD IN src/", [r for r in kept if r["area"] == "src" and r["kind"] == "DEAD"]),
        ("USED ONLY BY test/", [r for r in kept if r["kind"] == "TEST-ONLY"]),
    ]
    if include_tests:
        out.append(
            ("DEAD IN test/", [r for r in kept if r["area"] == "test"]),
        )
    return [(t, sorted(rs, key=lambda r: -r["loc"])) for t, rs in out]


def fmt_text(rows, include_tests, show_suppressed):
    lines = []
    for title, rs in sections(rows, include_tests):
        total = sum(r["loc"] for r in rs)
        lines.append(f"\n{title}  ({len(rs)} definitions, ~{total} LOC)")
        if not rs:
            lines.append("  (none)")
        for r in rs:
            where = f"{r['file']}:{r['line']}"
            note = (
                f"{r['n_test_uses']} test refs, 0 src refs"
                if r["kind"] == "TEST-ONLY"
                else f"{r['n_occurrences']} idx occ, all inside "
                f"{r['span'][0]}-{r['span'][1]}"
            )
            lines.append(f"  {where:<62} {r['qual']:<34} ~{r['loc']:>4} LOC  ({note})")
    sup = [r for r in rows if r["sup"]]
    by = collections.Counter(x for r in sup for x in r["sup"])
    lines.append(
        f"\nSUPPRESSED  {len(sup)}  ("
        + ", ".join(f"{k} {v}" for k, v in by.most_common())
        + ")"
    )
    if show_suppressed:
        for r in sorted(sup, key=lambda r: -r["loc"]):
            lines.append(
                f"  {r['file']}:{r['line']:<6} {r['qual']:<40} "
                f"~{r['loc']:>4} LOC  [{', '.join(r['sup'])}]"
            )
    else:
        lines.append("  (--show-suppressed to list them)")
    return "\n".join(lines)


def fmt_markdown(rows, include_tests):
    out = []
    for title, rs in sections(rows, include_tests):
        total = sum(r["loc"] for r in rs)
        out.append(f"\n### {title} — {len(rs)} definitions, ~{total} LOC\n")
        if not rs:
            out.append("_none_\n")
            continue
        out.append("| Location | Definition | LOC |")
        out.append("|---|---|--:|")
        for r in rs[:40]:
            out.append(f"| `{r['file']}:{r['line']}` | `{r['qual']}` | {r['loc']} |")
        if len(rs) > 40:
            out.append(f"| _… and {len(rs) - 40} more_ | | |")
    sup = [r for r in rows if r["sup"]]
    by = collections.Counter(x for r in sup for x in r["sup"])
    out.append(
        "\n_Suppressed "
        + str(len(sup))
        + ": "
        + ", ".join(f"{k} {v}" for k, v in by.most_common())
        + "._"
    )
    return "\n".join(out)


def key(r):
    return f"{r['file']}::{r['qual']}"


def main():
    ap = argparse.ArgumentParser(
        description="Find definitions nothing references, via ocaml-index.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="See the module docstring for the predicate and its limitations.",
    )
    ap.add_argument(
        "--no-build",
        action="store_true",
        help="skip `dune build @ocaml-index` (the Makefile target runs it first)",
    )
    ap.add_argument("--format", choices=("text", "json", "markdown"), default="text")
    ap.add_argument("--show-suppressed", action="store_true")
    ap.add_argument(
        "--include-tests", action="store_true", help="also report dead code in test/"
    )
    ap.add_argument("--baseline", metavar="FILE", help="known findings, one key per line")
    ap.add_argument(
        "--check",
        action="store_true",
        help="with --baseline, exit 1 if any finding is not in the baseline",
    )
    ap.add_argument(
        "--write-baseline", metavar="FILE", help="write current findings as a baseline"
    )
    args = ap.parse_args()

    if not args.no_build:
        subprocess.run(
            ["dune", "build", "@ocaml-index", "--profile", "dev"],
            cwd=REPO,
            check=True,
        )

    indexes = find_indexes()
    if not indexes:
        die("no .ocaml-index files under _build/; run `dune build @ocaml-index`")
    check_freshness()

    rows, skipped = Analyzer(load_occurrences(indexes), build_module_map()).run()

    if args.write_baseline:
        kept = [r for r in rows if not r["sup"]]
        with open(args.write_baseline, "w") as f:
            f.write("\n".join(sorted(key(r) for r in kept)) + "\n")
        print(f"wrote {len(kept)} entries to {args.write_baseline}", file=sys.stderr)
        return

    if args.format == "json":
        json.dump(rows, sys.stdout, indent=1)
        print()
    elif args.format == "markdown":
        print(fmt_markdown(rows, args.include_tests))
    else:
        print(fmt_text(rows, args.include_tests, args.show_suppressed))
        print(f"\nskipped uids: {dict(skipped)}", file=sys.stderr)

    if args.check:
        if not args.baseline:
            die("--check requires --baseline")
        with open(args.baseline) as f:
            known = {ln.strip() for ln in f if ln.strip()}
        new = [r for r in rows if not r["sup"] and key(r) not in known]
        if new:
            print(f"\n{len(new)} finding(s) not in the baseline:", file=sys.stderr)
            for r in new:
                print(f"  {r['file']}:{r['line']}  {r['qual']}", file=sys.stderr)
            sys.exit(1)


if __name__ == "__main__":
    main()
