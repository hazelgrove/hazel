#!/usr/bin/env python3
"""Lint the CSS color layering.

Two layers now, and the direction of reference between them is the invariant:

    the Colors slide   decides every color and, through the fan-out table in
                       ColorConfiguration.re, writes it onto `:root` at startup

    theme-generated.css  the same names again as DEFAULTS, wholly generated,
                         for the frame before the theme lands

    variables.css        the things the theme does not own: type, timing and
                         the z-index ladder

    *.css              component stylesheets consume the slide's ROLE names,
                       never the palette -- which is now structural: a palette
                       color is published only under the semantic names the
                       fan-out gives it, so there is no --ink to consume

There used to be a third layer, roles.css, a hand-written alias from role to
palette. It is gone: the theme writes those names itself, so the alias was a
second definition of a color the slide had already decided, and it capped
what a themer could reach -- its 77 roles resolved to only 31 palette colors.

The rules below are what is left to enforce mechanically. Component CSS reads
role names, because a role names a PURPOSE and so is the unit a themer can
move on its own; a palette entry is a bundle, fanned out to several properties
that often share nothing but their color. And no component stylesheet
declares a color the theme owns, because two `:root` blocks setting the same
name is a race decided by @import order -- which is exactly how 23 defaults
drifted into the projector stylesheets. Nor does one state a color outright:
a literal is a color no scheme can move, so it is right in the scheme it was
picked for and wrong in the other three. Fading a theme color is allowed;
changing its lightness, chroma or hue is a derivation, and belongs in the
slide.

Run via `make lint-css`. Exits non-zero on a violation.
"""
import io, os, re, sys, collections

ROOT = 'src/web/www'
STYLE = os.path.join(ROOT, 'style')
VARIABLES = os.path.join(STYLE, 'variables.css')
GENERATED = os.path.join(STYLE, 'theme-generated.css')
CONFIG = 'src/language/builtins/BuiltinsColorScheme.re'
SRC = 'src'

# Pre-existing dangling references, inherited not introduced. Fixing one is a
# VISUAL change (an invalid var() makes the whole declaration drop), so it
# needs design judgement and does not belong in a mechanical migration. This
# list is a ratchet: it may shrink, never grow.
KNOWN_DANGLING = {
    'G5', 'R4', 'TXT2', 'UI-Background', 'font-mono', 'light-text-color', 'main-text-color', 'mono-font',
    'shard-label', 'text-disabled', 'text-primary', 'ui-text',
}

strip = lambda s: re.sub(r'/\*.*?\*/', '', s, flags=re.S)

# Comments blanked but their newlines kept, so offsets still map to lines.
blank_comments = lambda s: re.sub(
    r'/\*.*?\*/', lambda m: re.sub(r'[^\n]', ' ', m.group(0)), s, flags=re.S)

NAMED_COLORS = set('''
aliceblue antiquewhite aqua aquamarine azure beige bisque black blanchedalmond
blue blueviolet brown burlywood cadetblue chartreuse chocolate coral
cornflowerblue cornsilk crimson cyan darkblue darkcyan darkgoldenrod darkgray
darkgreen darkgrey darkkhaki darkmagenta darkolivegreen darkorange darkorchid
darkred darksalmon darkseagreen darkslateblue darkslategray darkslategrey
darkturquoise darkviolet deeppink deepskyblue dimgray dimgrey dodgerblue
firebrick floralwhite forestgreen fuchsia gainsboro ghostwhite gold goldenrod
gray green greenyellow grey honeydew hotpink indianred indigo ivory khaki
lavender lavenderblush lawngreen lemonchiffon lightblue lightcoral lightcyan
lightgoldenrodyellow lightgray lightgreen lightgrey lightpink lightsalmon
lightseagreen lightskyblue lightslategray lightslategrey lightsteelblue
lightyellow lime limegreen linen magenta maroon mediumaquamarine mediumblue
mediumorchid mediumpurple mediumseagreen mediumslateblue mediumspringgreen
mediumturquoise mediumvioletred midnightblue mintcream mistyrose moccasin
navajowhite navy oldlace olive olivedrab orange orangered orchid palegoldenrod
palegreen paleturquoise palevioletred papayawhip peachpuff peru pink plum
powderblue purple rebeccapurple red rosybrown royalblue saddlebrown salmon
sandybrown seagreen seashell sienna silver skyblue slateblue slategray
slategrey snow springgreen steelblue tan teal thistle tomato turquoise violet
wheat white whitesmoke yellow yellowgreen
'''.split())

COLOR_FUNCTION = re.compile(
    r'\b(rgba?|hsla?|hwb|lab|lch|oklab|oklch|color|color-mix)\(', re.I)
HEX = re.compile(r'#[0-9a-f]{3,8}\b', re.I)
# A theme color made partly transparent is still the theme's color: the
# slide decides it, CSS only chooses how much of it shows.
THEME_ALPHA = re.compile(
    r'color-mix\(\s*in\s+[\w-]+\s*,\s*var\(--[\w-]+\)\s*[\d.]+%\s*,'
    r'\s*transparent\s*\)'
    r'|oklch\(\s*from\s+var\(--[\w-]+\)\s+l\s+c\s+h\s*/\s*[\d.]+%?\s*\)',
    re.I)
DECLARATION = re.compile(r'([\w-]+)\s*:\s*([^;{}]+)')


def color_literals(src):
    """(line, declaration) for every color a declaration states outright. Only
    declaration values are read, so an id selector like `#add` is not a hex
    color; url()s and strings are skipped, since a file name is not one
    either."""
    src = blank_comments(src)
    out = []
    for d in DECLARATION.finditer(src):
        value = re.sub(r'url\([^)]*\)|"[^"]*"|\'[^\']*\'',
                       lambda m: ' ' * len(m.group(0)), d.group(2))
        value = THEME_ALPHA.sub(lambda m: ' ' * len(m.group(0)), value)
        base = d.start(2)
        hits = [(m.start(), m.group(0)) for m in HEX.finditer(value)]
        hits += [(m.start(), m.group(0))
                 for m in COLOR_FUNCTION.finditer(value)]
        hits += [(m.start(), m.group(0))
                 for m in re.finditer(r'(?<![\w-])[a-z]+(?![\w-])', value, re.I)
                 if m.group(0).lower() in NAMED_COLORS]
        if hits:
            line = src.count('\n', 0, base + min(at for at, _ in hits)) + 1
            out.append((line, ' '.join(d.group(0).split())))
    return out


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


def runtime_declared():
    """Names set on an element from OCaml rather than written in a stylesheet.
    They are declared, just not where rule 3 can see it, so reading the call
    sites is what keeps a measured value like --main-scroll-width from looking
    dangling. Only literal names count: the theme's own are passed as `"--" ++
    var` and reach this lint through theme-generated.css instead."""
    names = set()
    for dp, _, ns in os.walk(SRC):
        for n in ns:
            if not n.endswith(('.re', '.rei')):
                continue
            src = strip(io.open(os.path.join(dp, n), encoding='utf-8').read())
            names |= set(re.findall(
                r'set_css_(?:custom_property|variable)\(\s*"--([\w-]+)"', src))
    return names


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

        # 2. Only variables.css declares a theme-owned color on :root.
        if f != GENERATED:
            for n in root_declarations(strip(raw)):
                if n in OWNED:
                    problems.append(
                        f'{os.path.relpath(f, ROOT)}: declares theme-owned '
                        f'--{n} on :root; that default is generated, so it '
                        'belongs in theme-generated.css')

        # 4. Component stylesheets state no colors: every color is the
        # slide's, so a themer can reach it in all four schemes.
        if f not in (VARIABLES, GENERATED):
            for line, decl in color_literals(raw):
                problems.append(
                    f'{os.path.relpath(f, ROOT)}:{line}: states a color '
                    f'({decl}); give it a role in the Colors slide')

    # 3. No NEW dangling references.
    dangling = {n for n in used if n not in defined | runtime_declared()}
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
