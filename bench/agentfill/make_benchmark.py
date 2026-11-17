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
app_assoc_proof = ("induction xs | [] => from [] @ ys do step end; from [] @ (ys @ zs) do step end; rewrite refl end"
    " | x :: t => from (x :: t) @ ys do rewrite app_cons end end; from (x :: (t @ ys)) @ zs do rewrite app_cons end end;"
    " from (t @ ys) @ zs do rewrite ih_t end end; from (x :: t) @ (ys @ zs) do rewrite app_cons end end; rewrite refl end end")
focus = [(n, s, app_assoc_proof if n == "app_assoc" else b) for n, s, b in focus]

for n in ["ordinals", "algebra", "under_fun", "rev_nil", "total_cons", "fold_cons"]:
    add(n, "1-one-step", focus_pre, focus)
add("totals_agree", "2-lemma", focus_pre, focus)
upto_invol = focus[:focus.index(next(t for t in focus if t[0] == "invol")) + 1]
add("rev_rev_cons", "2-lemma", focus_pre, upto_invol, extra=(
    "forall x: Int -> forall t: [Int] -> rev(rev(x :: t)) == x :: t",
    "from rev(rev(x :: t)) do rewrite invol end end; rewrite refl end"))
upto_total_cons = focus[:focus.index(next(t for t in focus if t[0] == "total_cons")) + 1]
add("total_two", "2-lemma", focus_pre, upto_total_cons, extra=(
    "forall a: Int -> forall b: Int -> total(a :: b :: []) == a + b",
    "from total(a :: b :: []) do rewrite total_cons end end; from total(b :: []) do rewrite total_cons end end;"
    " from total([]) do rewrite total_nil end end; from b + 0 do algebra b end end; rewrite refl end"))
upto_app_cons = focus[:focus.index(next(t for t in focus if t[0] == "app_cons")) + 1]
add("rev_single", "2-lemma", focus_pre, upto_app_cons, extra=(
    "forall x: Int -> rev(x :: []) == [x]",
    "from rev(x :: []) do rewrite rev_cons end end; from rev([]) do rewrite rev_nil end end;"
    " from [] @ [x] do step end; rewrite refl end"))

for n in ["snoc", "invol", "app_assoc"]:
    add(n, "3-induction", focus_pre, focus)
add("app_nil_r", "3-induction", focus_pre, upto_app_cons, extra=(
    "forall xs: [Int] -> xs @ [] == xs",
    "induction xs | [] => from [] @ [] do step end; rewrite refl end"
    " | x :: t => from (x :: t) @ [] do rewrite app_cons end end; from t @ [] do rewrite ih_t end end; rewrite refl end end"))

len_pre = ("let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n"
           "let add1 = fun (acc, _) -> acc + 1 in")
len_thms = [
    ("len_nil", "len([]) == 0", "from len([]) do step end; rewrite refl end"),
    ("len_cons", "forall h: Int -> forall t: [Int] -> len(h :: t) == 1 + len(t)", "from len(h :: t) do step end; rewrite refl end"),
    ("app_cons", "forall y: Int -> forall l: [Int] -> forall m: [Int] -> (y :: l) @ m == y :: (l @ m)", "from (y :: l) @ m do step end; rewrite refl end"),
    ("fold_nil", "forall a: Int -> fold_left([], add1, a) == a", "from fold_left([], add1, a) do step end; rewrite refl end"),
    ("shuffle", "forall a: Int -> forall b: Int -> a + (1 + b) == (a + 1) + b", "from a + (1 + b) do algebra (a + 1) + b end end; rewrite refl end"),
    ("fold_cons", "forall h: Int -> forall t: [Int] -> forall a: Int -> fold_left(h :: t, add1, a) == fold_left(t, add1, a + 1)",
     "from fold_left(h :: t, add1, a) do step; from $e(a, h) do step end; step end; from fold_left(t, add1, a + 1) do step end; rewrite refl end"),
    ("len_app", "forall xs: [Int] -> forall ys: [Int] -> len(xs @ ys) == len(xs) + len(ys)",
     "induction xs | [] => from [] @ ys do step end; from len([]) do rewrite len_nil end end; from 0 + len(ys) do algebra len(ys) end end; rewrite refl end"
     " | x :: t => from (x :: t) @ ys do rewrite app_cons end end; from len(x :: (t @ ys)) do rewrite len_cons end end;"
     " from len(t @ ys) do rewrite ih_t end end; from len(x :: t) do rewrite len_cons end end;"
     " from 1 + (len(t) + len(ys)) do algebra (1 + len(t)) + len(ys) end end; rewrite refl end end"),
    ("lenfold", "forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fold_left(xs, add1, acc)",
     "generalize acc => induction xs | [] => forall acc => from len([]) do rewrite len_nil end end; from fold_left([], add1, acc) do rewrite fold_nil end end;"
     " from acc + 0 do algebra acc end end; rewrite refl end"
     " | x :: xs' => forall acc => from len(x :: xs') do rewrite len_cons end end; from fold_left(x :: xs', add1, acc) do rewrite fold_cons end end;"
     " from acc + (1 + len(xs')) do rewrite shuffle end end; from fold_left(xs', add1, acc + 1) do rewrite <- ih_xs' end end; rewrite refl end end"),
]
add("len_app", "3-induction", len_pre, len_thms[:7])
add("lenfold", "4-generalize", len_pre, len_thms)
add("total_acc", "4-generalize", focus_pre, focus)

fa_pre = ("let len = fun xs -> case xs | [] => 0 | _ :: t => 1 + len(t) end in\n"
          "let fa = fun (xs, acc) -> case xs | [] => acc | _ :: t => fa((t, acc + 1)) end in")
fa_thms = [len_thms[0], len_thms[1], len_thms[4],
    ("fa_nil", "forall a: Int -> fa(([], a)) == a", "from fa(([], a)) do step end; rewrite refl end"),
    ("fa_cons", "forall h: Int -> forall t: [Int] -> forall a: Int -> fa((h :: t, a)) == fa((t, a + 1))",
     "from fa((h :: t, a)) do step end; rewrite refl end"),
    ("lenfa", "forall xs: [Int] -> forall acc: Int -> acc + len(xs) == fa((xs, acc))",
     "generalize acc => induction xs | [] => forall acc => from len([]) do rewrite len_nil end end; from fa(([], acc)) do rewrite fa_nil end end;"
     " from acc + 0 do algebra acc end end; rewrite refl end"
     " | x :: xs' => forall acc => from len(x :: xs') do rewrite len_cons end end; from fa((x :: xs', acc)) do rewrite fa_cons end end;"
     " from acc + (1 + len(xs')) do rewrite shuffle end end; from fa((xs', acc + 1)) do rewrite <- ih_xs' end end; rewrite refl end end"),
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
