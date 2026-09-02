#!/usr/bin/env python3
"""Lint the CSS colour layering.

Two layers now, and the direction of reference between them is the invariant:

    the Colors slide   decides every colour and, through the fan-out table in
                       ColorConfiguration.re, writes it onto `:root` at startup

    theme-generated.css  the same names again as DEFAULTS, wholly generated,
                         for the frame before the theme lands

    variables.css        the things the theme does not own: type, timing and
                         the z-index ladder

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
GENERATED = os.path.join(STYLE, 'theme-generated.css')
CONFIG = 'src/language/builtins/BuiltinsColorScheme.re'

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
    lint while every reference pointed at a variable nothing defined.

    It is `seeds @ derived` on the OCaml side -- what a scheme states, plus
    what the slide derives from that -- so both lists are read and unioned."""
    src = io.open(CONFIG, encoding='utf-8').read()
    names = set()
    for which in ('seeds', 'derived'):
        m = re.search(r'let %s: list\(string\) = \[(.*?)\];' % which, src, re.S)
        if not m:
            sys.exit(f'lint_css_roles: cannot find the {which} list in {CONFIG}')
        names |= set(re.findall(r'"([^"]+)"', m.group(1)))
    return names


def theme_owned():
    """Every name the theme writes, read from the generated stylesheet. A test
    keeps that file equal to the projection, so this cannot drift."""
    if not os.path.exists(GENERATED):
        sys.exit(f'lint_css_roles: {GENERATED} is missing; '
                 'run `make update-css-defaults`')
    src = io.open(GENERATED, encoding='utf-8').read()
    return set(re.findall(r'--([\w-]+)\s*:', src))


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
            if m.group(1) in PALETTE and f not in (VARIABLES, GENERATED):
                problems.append(
                    f'{os.path.relpath(f, ROOT)}: consumes palette '
                    f'--{m.group(1)} directly; use a role the slide writes')

        # 2. Only variables.css declares a theme-owned colour on :root.
        if f != GENERATED:
            for n in root_declarations(strip(raw)):
                if n in OWNED:
                    problems.append(
                        f'{os.path.relpath(f, ROOT)}: declares theme-owned '
                        f'--{n} on :root; that default is generated, so it '
                        'belongs in theme-generated.css')

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
