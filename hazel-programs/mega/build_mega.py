#!/usr/bin/env python3
"""Compose the mega test corpus (plans/modular-editors.md section 8b).

Sources: probe-study tutorial task programs (probes-study-rebuilt),
each converted into a module (bugs fixed, holes implemented, tests
folded into a `selfcheck` member), plus synthesized MVU-style modules,
plus a meta module referencing every other module. Variants:
mega-1k.hz / mega-2k.hz / mega-4k.hz.

Regenerate:  python3 build_mega.py   (writes into this directory)
"""
import os, re, sys

HERE = os.path.dirname(os.path.abspath(__file__))
TUT = os.path.expanduser(
    "~/.claude-worktrees/hazel/probes-study-rebuilt/hazel-programs/tutorial")

def code_of(fname):
    txt = open(os.path.join(TUT, fname)).read()
    m = re.search(r"^@code\n(.*?)(?=^@[a-z]+$)", txt, re.S | re.M)
    body = m.group(1) if m else txt.split("@code\n", 1)[1]
    return body.rstrip() + "\n"

def split_tests(src):
    parts = re.split(r"#\s*=+\s*TESTS\s*=+\s*#\n", src)
    return (parts[0], parts[1] if len(parts) > 1 else "")

FUNLET = re.compile(
    r"^let (\w+)\(([^()]*)\)(?::\s*([^=]+?))?\s*=\s*$")

def defunlet(line):
    """Module members can't use funlet sugar on dev (params don't
    statics-bind): rewrite `let f(a: T, b: U): R =` as
    `let f : (T, U) -> R = fun (a, b) ->`."""
    m = FUNLET.match(line)
    if not m:
        return line
    name, params, ret = m.group(1), m.group(2), m.group(3)
    names, types = [], []
    for p in params.split(","):
        p = p.strip()
        if ":" in p:
            n, t = p.split(":", 1)
            names.append(n.strip())
            types.append(t.strip())
        else:
            names.append(p)
            types.append(None)
    # param-ascribed fun style: `let f = fun (a: T, b: U) -> ...`.
    # NOT let-arrow-ascription — passing constructor values through an
    # arrow-ascribed member and capturing them in closures handed to
    # builtins hits a dev dynamics bug (stuck Asc at match; see
    # mega/BUG-asc-capture.hz). Param ascriptions keep the types and
    # dodge it.
    ps = []
    for n, t in zip(names, types):
        ps.append(f"{n}: {t}" if t else n)
    pat = ps[0] if len(ps) == 1 else "(" + ", ".join(ps) + ")"
    if len(ps) == 1 and ":" in ps[0]:
        pat = "(" + ps[0] + ")"
    return f"let {name} = fun {pat} ->"

def defs_to_members(defs):
    """Column-0 `... in` terminators become `;` member separators."""
    out = []
    for line in defs.rstrip().split("\n"):
        if line == "in":
            # close the previous member
            for i in range(len(out) - 1, -1, -1):
                if out[i].strip():
                    out[i] = out[i] + ";"
                    break
        elif re.match(r"^\S.*\sin$", line):
            # any column-0 closer ending in ` in` (one-line defs,
            # multi-line `) in`, `end in`, ...)
            out.append(defunlet(line[:-3].rstrip()) + ";")
        else:
            out.append(defunlet(line))
    return "\n".join(out).rstrip()

def tests_to_selfcheck(tests):
    """test..end chain -> one boolean conjunction."""
    tests = re.sub(r'^hint\s+"[^"]*"\s*$', "", tests, flags=re.M)
    bodies = re.findall(r"^test\n(.*?)^end;?", tests, re.S | re.M)
    conj = "\n    && ".join(
        "(\n" + b.rstrip() + "\n    )" for b in bodies)
    return ("  let selfcheck : () -> Bool = fun _ ->\n    "
            + conj)

def module_of(name, src, fixes=(), extra_members=""):
    for old, new in fixes:
        assert old in src, (name, old[:40])
        src = src.replace(old, new)
    defs, tests = split_tests(src)
    members = defs_to_members(defs)
    if not members.rstrip().endswith(";"):
        members = members + ";"
    if extra_members:
        members += "\n" + extra_members.rstrip()
        if not members.endswith(";"):
            members += ";"
    check = tests_to_selfcheck(tests) if tests.strip() else \
        "  let selfcheck : () -> Bool = fun _ -> true"
    body = members + "\n\n" + check
    # indent everything two spaces under the module braces
    indented = "\n".join(
        ("  " + l if l.strip() else l) for l in body.split("\n"))
    return f"module {name} = {{\n{indented}\n}} in\n"

# ---------------------------------------------------------------- Text
TEXT_MODULE = """module Text = {
  # Digits and integer rendering (no string_of_int builtin on dev) #
  let digit : Int -> String = fun d ->
    case d
    | 0 => "0" | 1 => "1" | 2 => "2" | 3 => "3" | 4 => "4"
    | 5 => "5" | 6 => "6" | 7 => "7" | 8 => "8" | _ => "9"
    end;

  let go_render : Int -> String = fun k ->
    if k < 10
    then digit(k)
    else go_render(k / 10) ++ digit(int_mod(k, 10));

  let int_to_text : Int -> String = fun n ->
    if n < 0 then "-" ++ go_render(0 - n) else go_render(n);

  let selfcheck : () -> Bool = fun _ ->
    (int_to_text(0) == "0")
    && (int_to_text(7) == "7")
    && (int_to_text(45) == "45")
    && (int_to_text(125) == "125")
    && (int_to_text(0 - 8) == "-8")
} in
"""

def study_modules():
    mods = []
    mods.append(("Text", TEXT_MODULE))

    # 26: dew ledger — bug: Spill halves jars instead of dew
    mods.append(("DewLedger", module_of(
        "DewLedger", code_of("26-task-dew-ledger.hzt"),
        fixes=[("(dew = m.dew, jars = m.jars / 2)",
                "(dew = m.dew / 2, jars = m.jars)")])))

    # 27: grove_name — implement the hole
    src27 = code_of("27-task-grove-name.hzt").replace(
        "let grove_name(path: String): String =\n  ¿\n\n",
        "let grove_name(path: String): String =\n"
        "  let parts = string_split(\"/\", path) in\n"
        "  let named = filter(parts, fun s -> string_length(s) > 0) in\n"
        "  case named\n"
        "  | [] => \"\"\n"
        "  | first :: _ => first\n"
        "  end\n")
    mods.append(("GroveNames", module_of("GroveNames", src27)))

    # 28: watering timer — bug: > should be >=; string_of_int -> Text
    src28 = code_of("28-task-watering-timer.hzt").replace(
        "string_of_int", "Text.int_to_text").replace(
        "if minutes > 60", "if minutes >= 60")
    mods.append(("WateringTimer", module_of("WateringTimer", src28)))

    # 29: running_sum — implement the hole
    src29 = code_of("29-task-running-sum.hzt").replace(
        "let running_sum(nums: [Int]): [Int] =\n  ¿\n\n",
        "let running_sum(nums: [Int]): [Int] =\n"
        "  let stepped =\n"
        "    fold_left(nums, fun (acc, n) ->\n"
        "      case acc\n"
        "      | (sums, total) => ((total + n) :: sums, total + n)\n"
        "      end\n"
        "    , ([], 0)) in\n"
        "  case stepped\n"
        "  | (sums, _) => reverse(sums)\n"
        "  end\n")
    mods.append(("RunningSums", module_of("RunningSums", src29)))

    # 31: clean_entry — implement the hole
    src31 = code_of("31-task-log-cleaner.hzt").replace(
        "let clean_entry(entry: String): String =\n  ¿\n\n",
        "let clean_entry(entry: String): String =\n"
        "  let words = string_split(\" \", string_trim(entry)) in\n"
        "  let kept = filter(words, fun w -> string_length(w) > 0) in\n"
        "  let body =\n"
        "    case kept\n"
        "    | [] => []\n"
        "    | _ :: tail => tail\n"
        "    end in\n"
        "  let joined = string_join(\" \", body) in\n"
        "  let tight = string_replace(\" -- \", joined, \"--\") in\n"
        "  string_replace(\"--\", tight, \": \")\n")
    mods.append(("LogCleaner", module_of("LogCleaner", src31)))

    # 30: crop plotter — bug: setCell tests j == row (and hides it via _col)
    mods.append(("CropPlotter", module_of(
        "CropPlotter", code_of("30-task-planting-bug.hzt"),
        fixes=[
            ("let setCell(grove: Grove, row: Row, _col: Col, plant: Plant): Grove =",
             "let setCell(grove: Grove, row: Row, col: Col, plant: Plant): Grove ="),
            ("      if j == row\n", "      if j == col\n"),
        ])))

    # 32: harvest ledger — bug: streak compares quality to itself
    mods.append(("HarvestLedger", module_of(
        "HarvestLedger", code_of("32-task-harvest-streak.hzt"),
        fixes=[("let continues = !isFirst && h.quality == newLast",
                "let continues = !isFirst && h.quality == ledger.lastQuality")])))

    # 34: field plotter — bug: Growing does not advance
    mods.append(("FieldPlotter", module_of(
        "FieldPlotter", code_of("34-task-growth-plotter.hzt"),
        fixes=[("| Growing => Growing", "| Growing => Mature")])))
    return mods

def meta_and_tests(mod_names):
    reports = ",\n      ".join(
        f'(label = "{n}", passed = {n}.selfcheck(()))' for n in mod_names)
    meta = f"""module MetaRunner = {{
  # The linking layer: an MVU program whose actions run the sub-apps #
  type Report = (label = String, passed = Bool);
  type Model = (ran = Int, ok = Int);
  type Action =
    + RunAll
    + Reset;

  let init : Model = (ran = 0, ok = 0);

  let all_reports : () -> [Report] = fun _ ->
    [
      {reports}
    ];

  let update : (Model, Action) -> Model = fun (m, a) ->
    case a
    | RunAll =>
        let rs = all_reports(()) in
        let good = filter(rs, fun r -> r.passed) in
        (ran = m.ran + length(rs), ok = m.ok + length(good))
    | Reset => init
    end;

  let run_all : () -> Model = fun _ -> update(init, RunAll);

  let selfcheck : () -> Bool = fun _ ->
    let after = run_all(()) in
    after.ran == {len(mod_names)} && after.ok == {len(mod_names)}
}} in
"""
    tests = "\n".join(
        f"test {n}.selfcheck(()) end;" for n in mod_names)
    tail = f"""{tests}
test MetaRunner.selfcheck(()) end;

let final = MetaRunner.run_all(()) in
final.ok == {len(mod_names)}
"""
    return meta, tail

def compose(mods, out):
    names = [n for n, _ in mods]
    meta, tail = meta_and_tests(names)
    txt = ("# Mega corpus: study task programs as modules + a meta runner. #\n"
           "# Generated by build_mega.py - see that file for provenance. #\n\n"
           + "\n".join(t for _, t in mods) + "\n" + meta + "\n" + tail)
    path = os.path.join(HERE, out)
    open(path, "w").write(txt)
    print(out, len(txt.splitlines()), "lines")

def to_mod_item(t):
    """Turn an `... in`-terminated top-level binding into a `;` module
    item (the content is identical; only the chain terminator moves)."""
    return re.sub(r"\}\s*in\s*$", "};", t.rstrip()) + "\n"

def compose_mod_root(mods, out):
    """Same corpus with the TOP LEVEL as a module body (root sort Mod,
    plans/mod-root.md): `module X = {...};` items instead of `... in`
    chains; the runner tail becomes a `let;` item + trailing member
    expression. No leading/trailing bare `;` (the FastParse mod-root
    brace-wrap rejects spliced separators)."""
    names = [n for n, _ in mods]
    meta, tail = meta_and_tests(names)
    tail = tail.replace(
        "let final = MetaRunner.run_all(()) in\n",
        "let final = MetaRunner.run_all(());\n")
    txt = ("# Mega corpus (mod-rooted): top level is a module body. #\n"
           "# Generated by build_mega.py - see that file for provenance. #\n\n"
           + "\n".join(to_mod_item(t) for _, t in mods)
           + "\n" + to_mod_item(meta) + "\n" + tail)
    path = os.path.join(HERE, out)
    open(path, "w").write(txt)
    print(out, len(txt.splitlines()), "lines")

if __name__ == "__main__":
    sys.path.insert(0, HERE)
    import packs

    study = study_modules()
    compose(study, "mega-1k.hz")
    compose(study + packs.PACK_A + packs.family_modules(5), "mega-2k.hz")
    compose(study + packs.PACK_A + packs.family_modules(18), "mega-4k.hz")
    compose_mod_root(study, "mega-mod-1k.hz")
    compose_mod_root(study + packs.PACK_A + packs.family_modules(5),
                     "mega-mod-2k.hz")
    compose_mod_root(study + packs.PACK_A + packs.family_modules(18),
                     "mega-mod-4k.hz")
