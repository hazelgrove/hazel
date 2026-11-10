"""Summarizes run.js results: one row per theorem, then each failing
theorem's rounds (the model's reply and the checker's problems)."""
import json, sys

def rounds(d, name):
    ex = [e for e in d["exchanges"] if e["item"] == name]
    out = []
    for i, e in enumerate(ex):
        nxt = ex[i + 1]["messages"][-1]["content"] if i + 1 < len(ex) else None
        out.append({"reply": e.get("reply", e.get("error")), "problems": nxt})
    return out

def main(paths, detail):
    for p in paths:
        d = json.load(open(p))
        print(f"## {d['model']} / {d['intervention']}: {d['calls']} calls, ${d['cost']:.3f}")
        ok = 0
        for r in d["results"]:
            n = len([e for e in d["exchanges"] if e["item"] == r["name"]])
            good = r["result"] == "proven"
            ok += good
            print(f"{r['tier']:14} {r['name']:14} {r['result']:12} rounds={n}  {r['statuses'][-1][:110]}")
        print(f"proven {ok}/{len(d['results'])}\n")
        if detail:
            for r in d["results"]:
                if r["result"] == "proven":
                    continue
                print(f"### {r['name']}")
                for i, rd in enumerate(rounds(d, r["name"])):
                    print(f"-- reply {i+1}:\n{rd['reply']}")
                    if rd["problems"]:
                        print(f"-- problems:\n{rd['problems']}")
                print()

if __name__ == "__main__":
    detail = "--detail" in sys.argv
    main([a for a in sys.argv[1:] if a != "--detail"], detail)
