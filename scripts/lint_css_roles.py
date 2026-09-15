#!/usr/bin/env python3
"""Lint the CSS colour layering.

Three layers, and the direction of reference between them is the invariant:

    variables.css   palette (theme-settable input; user themes write these) --+
                                                                             |
    roles.css       semantic roles, each a BARE alias for one palette var  <--+
                                                                             |
    *.css           component stylesheets consume ROLES, never the palette  <-+

The bare-alias rule is what makes the migration checkable: role -> palette is
a total function, so substituting every var(--role) in a component stylesheet
back to its palette name must reproduce the pre-migration file byte for byte.
Anything richer (oklch(), color-mix(), a literal) would break that, so it is
rejected here rather than in review.

Run via `make lint-css`. Exits non-zero on a violation.
"""
import os, re, sys, collections

ROOT = 'src/web/www'
STYLE = os.path.join(ROOT, 'style')
VARIABLES = os.path.join(STYLE, 'variables.css')
ROLES = os.path.join(STYLE, 'roles.css')
INPUT_LAYER = {VARIABLES, ROLES}

PALETTE = set("""NONE SAND STONE BLACK BR1 BR2 BR3 BR4 T1 T2 T3 T4 Y0 Y1 Y2 Y3
                 R0 R1 R2 TYP PAT TPAT LABEL highlight-a highlight-b highlight-c
                 G0 G1 G2 GB0 GB1""".split())

# Pre-existing dangling references, inherited not introduced. Fixing one is a
# VISUAL change (an invalid var() makes the whole declaration drop), so it
# needs design judgement and does not belong in a mechanical migration. This
# list is a ratchet: it may shrink, never grow.
KNOWN_DANGLING = {
    'BLUE', 'G5', 'GREEN', 'R4', 'RED', 'TXT2', 'UI-Background', 'YELLOW',
    'font-mono', 'light-text-color', 'main-text-color', 'mono-font',
    'row-height-px', 'shard-label', 'text-disabled', 'text-primary', 'ui-text',
}

strip = lambda s: re.sub(r'/\*.*?\*/', '', s, flags=re.S)

def css_files():
    out = []
    for dp, _, ns in os.walk(STYLE):
        out += [os.path.join(dp, n) for n in ns if n.endswith('.css')]
    out.append(os.path.join(ROOT, 'style.css'))
    return sorted(set(out))

def main():
    files = css_files()
    problems = []

    # 1. roles.css must contain nothing but bare aliases.
    roles_src = strip(open(ROLES).read())
    roles = {}
    for m in re.finditer(r'--([\w-]+)\s*:\s*([^;]+);', roles_src):
        name, value = m.group(1), m.group(2).strip()
        bare = re.fullmatch(r'var\(--([\w-]+)\)', value)
        if not bare:
            problems.append(f"roles.css: --{name} is not a bare alias: {value}")
        else:
            roles[name] = bare.group(1)
            if bare.group(1) not in PALETTE:
                problems.append(
                    f"roles.css: --{name} aliases --{bare.group(1)}, "
                    "which is not a palette name")

    # 2. Component stylesheets must not consume palette names directly.
    defined, used = set(), collections.defaultdict(set)
    for f in files:
        src = strip(open(f).read())
        for m in re.finditer(r'(--[\w-]+)\s*:', src):
            defined.add(m.group(1)[2:])
        for m in re.finditer(r'var\(\s*--([\w-]+)', src):
            used[m.group(1)].add(f)
            if m.group(1) in PALETTE and f not in INPUT_LAYER:
                problems.append(
                    f"{os.path.relpath(f, ROOT)}: consumes palette --{m.group(1)} "
                    "directly; use a role from roles.css")

    # 3. No NEW dangling references.
    dangling = {n for n in used if n not in defined}
    for n in sorted(dangling - KNOWN_DANGLING):
        where = sorted(os.path.basename(x) for x in used[n])
        problems.append(f"dangling var(--{n}) in {where}: defined nowhere")
    stale = KNOWN_DANGLING - dangling
    if stale:
        problems.append(
            f"KNOWN_DANGLING is stale, these now resolve: {sorted(stale)} "
            "-- remove them from the list in scripts/lint_css_roles.py")

    # 4. No role refers to another role (the alias layer must be one hop).
    for name, target in roles.items():
        if target in roles:
            problems.append(f"roles.css: --{name} -> --{target} is a role, not a palette var")

    if problems:
        print(f"CSS role lint: {len(problems)} problem(s)\n")
        for p in problems:
            print("  " + p)
        return 1
    print(f"CSS role lint: OK "
          f"({len(roles)} roles, {len(files)} files, "
          f"{len(dangling)} known-dangling)")
    return 0

if __name__ == '__main__':
    sys.exit(main())
