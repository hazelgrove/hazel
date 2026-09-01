#!/usr/bin/env python3
"""Lint the CSS colour layering.

Two layers now, and the direction of reference between them is the invariant:

    the Colors slide   decides every colour and, through the fan-out table in
                       ColorConfiguration.re, writes it onto `:root` at startup

    variables.css      the same names again as DEFAULTS, in a generated block,
                       for the frame before the theme lands -- plus the
                       colours the slide has no field for yet

    *.css              component stylesheets consume the slide's ROLE names,
                       never the palette

There used to be a third layer, roles.css, a hand-written alias from role to
palette. It is gone: the theme writes those names itself, so the alias was a
second definition of a colour the slide had already decided, and it capped
what a themer could reach -- its 77 roles resolved to only 31 palette colours.

The rules below are what is left to enforce mechanically. Component CSS reads
roles, not the palette, so a themer changing `--frame-1` moves the things that
mean "faint", not everything that happens to be that colour. And no component
stylesheet declares a colour the theme owns, because two `:root` blocks
setting the same name is a race decided by @import order -- which is exactly
how 23 defaults drifted into the projector stylesheets.

Run via `make lint-css`. Exits non-zero on a violation.
"""
import io, os, re, sys, collections

ROOT = 'src/web/www'
STYLE = os.path.join(ROOT, 'style')
VARIABLES = os.path.join(STYLE, 'variables.css')
CONFIG = 'src/web/util/ColorConfiguration.re'

BEGIN = 'BEGIN GENERATED DEFAULTS'
END = 'END GENERATED DEFAULTS'

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


def palette():
    """The palette layer, read from the projection rather than restated. The
    names used to be spelled twice here, and a rename would have passed the
    lint while every reference pointed at a variable nothing defined."""
    src = io.open(CONFIG, encoding='utf-8').read()
    m = re.search(r'let palette: list\(string\) = \[(.*?)\];', src, re.S)
    if not m:
        sys.exit(f'lint_css_roles: cannot find the palette list in {CONFIG}')
    return set(re.findall(r'"([^"]+)"', m.group(1)))


def theme_owned():
    """Every name the theme writes, taken from the generated block. A test
    keeps that block equal to the projection, so this cannot drift."""
    src = io.open(VARIABLES, encoding='utf-8').read()
    try:
        block = src[src.index(BEGIN):src.index(END)]
    except ValueError:
        sys.exit(f'lint_css_roles: no generated block in {VARIABLES}')
    return set(re.findall(r'--([\w-]+)\s*:', block))


def css_files():
    out = []
    for dp, _, ns in os.walk(STYLE):
        out += [os.path.join(dp, n) for n in ns if n.endswith('.css')]
    out.append(os.path.join(ROOT, 'style.css'))
    return sorted(set(out))


def root_declarations(src):
    """Names declared directly on `:root`. A scoped override is a deliberate
    local decision and none of this lint's business; a `:root` one competes
    with the defaults file."""
    names, depth, sel = [], 0, ''
    for line in src.split('\n'):
        if depth == 0:
            m = re.match(r'\s*([^{]*)\{\s*$', line)
            if m:
                sel = m.group(1).strip()
        depth += line.count('{') - line.count('}')
        d = re.match(r'\s*--([\w-]+)\s*:', line)
        if d and sel == ':root':
            names.append(d.group(1))
    return names


def main():
    files = css_files()
    PALETTE, OWNED = palette(), theme_owned()
    problems = []

    defined, used = set(), collections.defaultdict(set)
    for f in files:
        raw = io.open(f, encoding='utf-8').read()
        src = strip(raw)
        for m in re.finditer(r'(--[\w-]+)\s*:', src):
            defined.add(m.group(1)[2:])

        # 1. Component stylesheets consume roles, not the palette.
        for m in re.finditer(r'var\(\s*--([\w-]+)', src):
            used[m.group(1)].add(f)
            if m.group(1) in PALETTE and f != VARIABLES:
                problems.append(
                    f'{os.path.relpath(f, ROOT)}: consumes palette '
                    f'--{m.group(1)} directly; use a role the slide writes')

        # 2. Only variables.css declares a theme-owned colour on :root.
        if f != VARIABLES:
            for n in root_declarations(strip(raw)):
                if n in OWNED:
                    problems.append(
                        f'{os.path.relpath(f, ROOT)}: declares theme-owned '
                        f'--{n} on :root; the default belongs in variables.css')

    # 3. No NEW dangling references.
    dangling = {n for n in used if n not in defined}
    for n in sorted(dangling - KNOWN_DANGLING):
        where = sorted(os.path.basename(x) for x in used[n])
        problems.append(f'dangling var(--{n}) in {where}: defined nowhere')
    stale = KNOWN_DANGLING - dangling
    if stale:
        problems.append(
            f'KNOWN_DANGLING is stale, these now resolve: {sorted(stale)} '
            '-- remove them from the list in scripts/lint_css_roles.py')

    if problems:
        print(f'CSS role lint: {len(problems)} problem(s)\n')
        for p in problems:
            print('  ' + p)
        return 1
    print(f'CSS role lint: OK ({len(OWNED)} theme-owned names, '
          f'{len(PALETTE)} palette, {len(files)} files, '
          f'{len(dangling)} known-dangling)')
    return 0


if __name__ == '__main__':
    sys.exit(main())
