"""Writes benchmark.json: theorems the prover proves, each as a program
whose target theorem's proof is `??`, with the earlier theorems (the
lemmas it may cite) proven before it, and a reference program whose
proof checks. Hard items have no reference proof."""
import json, re, sys, os

HERE = os.path.dirname(os.path.abspath(__file__))
FOCUS = sys.argv[1]  # focus-examples.hazel
STLC = os.path.join(HERE, "../../docs/stlc-progress-example.hazel")

def theorems(src):
    """(preamble, [(name, statement_text_through_proof_kw, proof)]) for a
    program of `theorem n = s proof p in` blocks, each block starting on
    its own line."""
    lines = src.split("\n")
    start = next(i for i, l in enumerate(lines) if l.startswith("theorem "))
    pre = "\n".join(l for l in lines[:start] if not l.startswith("#"))
    rest = "\n".join(lines[start:])
    blocks = re.split(r"\n(?=theorem )", rest)
    out = []
    for b in blocks:
        b = re.sub(r"\n#[^\n]*#", "", b)
        m = re.match(r"theorem (\w+) = (.*?) proof(.*)", b, re.S)
        name, stmt, body = m.group(1), m.group(2), m.group(3)
        body = re.sub(r"\s*in\s*(0\s*)?$", "", body.rstrip())
        out.append((name, stmt.strip(), body.strip()))
    return pre, out

def program(pre, thms, target, proof):
    parts = [pre]
    for name, stmt, body in thms:
        if name == target:
            parts.append(f"theorem {name} = {stmt} proof {proof} in\n0")
            return "\n".join(parts)
        parts.append(f"theorem {name} = {stmt} proof {body} in")
    raise KeyError(target)

items = []
def add(name, tier, pre, thms, reference=True, extra=None):
    """`extra`: (statement, reference proof) for a theorem not in the source."""
    if extra:
        thms = thms + [(name, extra[0], extra[1])]
    body = dict((n, b) for n, _, b in thms)[name]
    items.append({
        "name": name, "tier": tier,
        "program": program(pre, thms, name, "??"),
        "reference": program(pre, thms, name, body) if reference else None,
    })

focus_pre, focus = theorems(open(FOCUS).read())
app_assoc_proof = ("induction xs | [] => from [] @ ys do eval end; from [] @ (ys @ zs) do eval end; axiom refl_eq end"
    " | x :: t => from (x :: t) @ ys do axiom app_cons end end; from (x :: (t @ ys)) @ zs do axiom app_cons end end;"
    " from (t @ ys) @ zs do axiom ih end end; from (x :: t) @ (ys @ zs) do axiom app_cons end end; axiom refl_eq end end")
focus = [(n, s, app_assoc_proof if n == "app_assoc" else b) for n, s, b in focus]

for n in ["ordinals", "algebra", "under_fun", "rev_nil", "total_cons", "fold_cons"]:
    add(n, "1-one-step", focus_pre, focus)
add("totals_agree", "2-lemma", focus_pre, focus)
upto_invol = focus[:focus.index(next(t for t in focus if t[0] == "invol")) + 1]
add("rev_rev_cons", "2-lemma", focus_pre, upto_invol, extra=(
    "forall x: Int -> forall t: [Int] -> rev(rev(x :: t)) == x :: t",
    "from rev(rev(x :: t)) do axiom invol end end; axiom refl_eq end"))
upto_total_cons = focus[:focus.index(next(t for t in focus if t[0] == "total_cons")) + 1]
add("total_two", "2-lemma", focus_pre, upto_total_cons, extra=(
    "forall a: Int -> forall b: Int -> total(a :: b :: []) == a + b",
    "from total(a :: b :: []) do axiom total_cons end end; from total(b :: []) do axiom total_cons end end;"
    " from total([]) do axiom total_nil end end; from b + 0 do rewrite b end end; axiom refl_eq end"))
upto_app_cons = focus[:focus.index(next(t for t in focus if t[0] == "app_cons")) + 1]
add("rev_single", "2-lemma", focus_pre, upto_app_cons, extra=(
    "forall x: Int -> rev(x :: []) == [x]",
    "from rev(x :: []) do axiom rev_cons end end; from rev([]) do axiom rev_nil end end;"
    " from [] @ [x] do eval end; axiom refl_eq end"))

for n in ["snoc", "invol", "app_assoc"]:
    add(n, "3-induction", focus_pre, focus)
add("app_nil_r", "3-induction", focus_pre, upto_app_cons, extra=(
    "forall xs: [Int] -> xs @ [] == xs",
    "induction xs | [] => from [] @ [] do eval end; axiom refl_eq end"
    " | x :: t => from (x :: t) @ [] do axiom app_cons end end; from t @ [] do axiom ih end end; axiom refl_eq end end"))

len_pre = ("let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n"
           "let add1 = fun (acc, _) -> acc + 1 in")
len_thms = [
    ("len_nil", "len([]) == 0", "from len([]) do eval end; axiom refl_eq end"),
    ("len_cons", "forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t)", "from len(h :: t) do eval end; axiom refl_eq end"),
    ("app_cons", "forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m)", "from (y :: l) @ m do eval end; axiom refl_eq end"),
    ("fold_nil", "forall a: Int -> fold_left([], add1, a) == a", "from fold_left([], add1, a) do eval end; axiom refl_eq end"),
    ("shuffle", "forall a: Int -> forall b: Int -> a + (1 + b) == (a + 1) + b", "from a + (1 + b) do rewrite (a + 1) + b end end; axiom refl_eq end"),
    ("fold_cons", "forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1)",
     "from fold_left(h :: t, add1, a) do eval; from $e(a, h) do eval end; eval end; from fold_left(t, add1, a + 1) do eval end; axiom refl_eq end"),
    ("len_app", "forall xs: [Int] -> forall ys: [Int] -> len(xs @ ys) == len(xs) + len(ys)",
     "induction xs | [] => from [] @ ys do eval end; from len([]) do axiom len_nil end end; from 0 + len(ys) do rewrite len(ys) end end; axiom refl_eq end"
     " | x :: t => from (x :: t) @ ys do axiom app_cons end end; from len(x :: (t @ ys)) do axiom len_cons end end;"
     " from len(t @ ys) do axiom ih end end; from len(x :: t) do axiom len_cons end end;"
     " from 1 + (len(t) + len(ys)) do rewrite (1 + len(t)) + len(ys) end end; axiom refl_eq end end"),
    ("lenfold", "forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc)",
     "generalize acc => induction xs | [] => forall acc => from len([]) do axiom len_nil end end; from fold_left([], add1, acc) do axiom fold_nil end end;"
     " from acc + 0 do rewrite acc end end; axiom refl_eq end"
     " | x :: xs' => forall acc => from len(x :: xs') do axiom len_cons end end; from fold_left(x :: xs', add1, acc) do axiom fold_cons end end;"
     " from acc + (1 + len(xs')) do axiom shuffle end end; from fold_left(xs', add1, acc + 1) do axiomrev ih end end; axiom refl_eq end end"),
]
add("len_app", "3-induction", len_pre, len_thms[:7])
add("lenfold", "4-generalize", len_pre, len_thms)
add("total_acc", "4-generalize", focus_pre, focus)

fa_pre = ("let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n"
          "let fa = fun (xs, acc) -> case xs | [] => acc | _ :: t => fa((t, acc + 1)) end in")
fa_thms = [len_thms[0], len_thms[1], len_thms[4],
    ("fa_nil", "forall a: Int -> fa(([], a)) == a", "from fa(([], a)) do eval end; axiom refl_eq end"),
    ("fa_cons", "forall h: Int -> forall t: [Int] -> forall a: Int -> fa((h :: t, a)) == fa((t, a + 1))",
     "from fa((h :: t, a)) do eval end; axiom refl_eq end"),
    ("lenfa", "forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fa((xs, acc))",
     "generalize acc => induction xs | [] => forall acc => from len([]) do axiom len_nil end end; from fa(([], acc)) do axiom fa_nil end end;"
     " from acc + 0 do rewrite acc end end; axiom refl_eq end"
     " | x :: xs' => forall acc => from len(x :: xs') do axiom len_cons end end; from fa((x :: xs', acc)) do axiom fa_cons end end;"
     " from acc + (1 + len(xs')) do axiom shuffle end end; from fa((xs', acc + 1)) do axiomrev ih end end; axiom refl_eq end end"),
]
add("lenfa", "4-generalize", fa_pre, fa_thms)

stlc = open(STLC).read()
stlc_lines = stlc.split("\n")
defs = "\n".join(l for l in stlc_lines if not l.startswith("theorem") and not l.startswith("|") and l.strip() and not l.startswith(" "))
defs = defs.replace("end in 0", "").rstrip()
stlc_thms = [l for l in stlc_lines if l.startswith("theorem step_")]
for i, name in enumerate(["step_ap_val1", "step_lam_ap2", "step_beta"]):
    before = stlc_thms[:i]
    target = stlc_thms[i].replace("; ? in", "; ?? in")
    prog = "\n".join([defs] + before + [target, "0"])
    items.append({"name": name, "tier": "5-hard", "program": prog, "reference": None})

json.dump(items, open(os.path.join(HERE, "benchmark.json"), "w"), indent=1)
print(len(items), "items")
for it in items: print(it["tier"], it["name"])
