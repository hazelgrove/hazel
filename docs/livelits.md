# Livelits in Hazel

Hazel implements the live literals (livelits) of Omar, Moon, Blinn, Voysey,
Collins and Chugh, *Filling Typed Holes with Live GUIs*, PLDI 2021
([paper](https://hazel.org/papers/livelits-pldi2021.pdf)). Section, figure and
line numbers below are that paper's. This document is the current design and
the plan for what is missing; the running record of the work, with
measurements, is the description of PR #2597.

A livelit is a GUI that fills an expression hole. A client writes `^name`,
presses space, and gets a widget whose state, the **model**, is part of the
program text. What the use means is the livelit's **expansion**.

## Status

| | |
|---|---|
| One signature, `expand` a sum of `Functional` and `Macro` | done |
| Livelit errors are type errors (a definition is checked against the signature) | done |
| Commands: `init`, `update` and `view` as command trees, with `do` syntax | done |
| Splices: `new_splice`, `set_splice`, `eval_splice`, `editor`, `result_view` | done |
| `SpliceRef` not type-indexed | done |
| A livelit's footprint may depend on its model (`shape`) | done |
| Parameters: values (Sec. 2.4.1) and types, through an abbreviation | done, with closed arguments; see [Parameters](#parameters) |
| `Exp` inhabited by quoted Hazel code | **done**: `quote e end`, `unquote e end`, `Ident` and `Lambda`; a Macro use is its expansion applied to its splices. See [Quotation](#quotation-the-plan) |

Working examples, shipped as the Documentation → Livelits slides
(`hazel-programs/docs/livelits/`):

- **Color (Figure 3)**: the paper's `$color`, keyed to Fig. 3 line by line.
  Its four splices name variables that sliders set, and its Macro `expand` is
  Fig. 3 l.56: a use means the quoted function applied to its four splices.
- **Hygiene**, a folder of slides (Livelits / Hygiene), one per part of the
  binding discipline, each beside what naive textual expansion would do: a splice holding the client's `x` next to an
  expansion that binds its own `x` (11, not 20); a client shadowing a
  builtin the expansion uses (3, not 0); and generated binders, captured
  with author-named `Lambda` (`[2, 2]`) and not with `Abs` (`[1, 2]`).
- **Parameters**: one `^slider` whose bounds are value parameters, and two
  abbreviations of it, `^percent` over 0-100 and `^die` over 1-6.
- **Either, Two Versions**, a folder: one livelit whose expansion type
  varies by use, written with `Expansion = ?` and with a type parameter.
- **Result View**: a sheet of three cells after the paper's `$dataframe`
  (Fig. 1c). Each cell shows its result with `result_view`, and only the
  formula bar, for the selected cell, is an `editor`.
- **Dynamic Row or Column**: a row or column of cells that grows and shrinks.
  `init` makes three cells with `new_splice`, **+** and **x** add and drop
  cells, and the view sums them with `eval_splice`: the number of splices is
  not fixed by the livelit. So its Macro `expand` *builds* its expansion, a
  function of as many parameters as there are cells, with one `Abs` per
  binder so that the system names each one, and a use means the list of the
  cells' values.

Every example is hygienic except one: Hygiene / 3. Generated Binders, which
names its binders with `Lambda` and `Ident` to show the capture that `Abs`
rules out.

## Defining a livelit

A livelit is a module bound to a `^name`, checked against one signature:

```
type Livelit = {
  type Model;  type Action;  type Expansion;
  let init   : UpdateCmd(Model);                        (Sec. 3.2.1)
  let update : Model -> Action -> UpdateCmd(Model);     (Sec. 3.2.4)
  let view   : Model -> ViewCmd(Html.T);                (Sec. 3.2.3)
  let expand : + Functional( Model -> Expansion )
               + Macro( Model -> (Exp, List(SpliceRef)) )   (Sec. 3.2.5)
}
```

- The three type members are the livelit's interface: `Model` types each
  use's argument, `Action` types what the view emits, and `Expansion` is what
  clients type against.
- A missing member is `ModuleMissingMembers`; a mistyped one is an ordinary
  type error at that member.
- Optionally, `let shape = Inline(w) | Block(w, h) | Tab(w, h)` sets the
  widget's footprint in character cells, or `let shape = fun m : Model -> ...`
  makes it follow the model.
- There is no `context` clause (Fig. 3 l.6). `init` and `update` run in the
  builtin environment, so a definition must be closed: helpers are members.

### Functional and Macro

`expand` commits to one of two arms.

- **`Functional(f)`**: the use means `f(model)`, a value. Every working
  livelit today is Functional. This arm is ours; the paper's `expand` is the
  Macro arm alone.
- **`Macro(g)`**: `g(model)` returns code and the splices that code takes,
  as in Fig. 3, and a use means that code applied to them. See
  [Quotation](#quotation-the-plan).

## Commands

`UpdateCmd(t)` and `ViewCmd(t)` are builtin trees of commands, which the
editor performs:

- both have `Pure` (answer a value) and `Bind`;
- `UpdateCmd` has `NewSplice` and `SetSplice`;
- `ViewCmd` has `EvalSplice`, `Editor` and `ResultView`.

`do p <- c in body` is real syntax, in the tile parser and in Menhir. The
checker works out both command types, so authors never write `@<...>` for a
bind. A do-block is in one kind of command, so `eval_splice` inside `update`
is a type error (Sec. 3.2.4: the model must not depend *directly* on the
client's code).

`init` runs when a use is created (typing `^name` then space), `update` at each
event, and `view` whenever the use is drawn. `update`'s answer is written into
the program text as the new model.

## Splices

A splice is a piece of the client's own code inside a livelit's GUI, in the
client's scope (Sec. 2.4). `new_splice` is the only way to make one.

- **The program text is where splices live.** Each ref in a use's model is
  written back as the splice itself, in parens, at the ref's position:
  Color's use reads `^color((r = (red), g = (green), b = (blue), a = (alpha)))`.
  Nothing is kept beside the text, so undo, save and reload keep every splice,
  and reloading rebuilds each parenthesized field or list element.
- **A ref carries the value its code had in that run**, because the use's
  model argument is evaluated in the use's own scope. `eval_splice` reads it
  and answers `Some(Val(v))` or `Some(Indet)`.
- **`result_view` draws what `eval_splice` reads.** It mirrors `editor`
  (Sec. 3.2.3, "Result Rendering"): `result_view(r, FixedWidth(n))`
  answers `Some(html)`, an opaque node that Hazel draws as it draws any
  value, for the view to place; `None` exactly when `eval_splice` would
  answer `Indet`, since a name with no value in this run would otherwise
  be drawn as its own unevaluated code. The view decides what `None`
  looks like.
- `set_splice` writes new code at the ref's position. Deletion is implicit:
  a splice no ref reaches is not written back.
- `new_splice(IntT, Some(IntLit(0)))` gives a splice initial code, and
  `set_splice(r, IntLit(n))` replaces it. Either takes a lift of any base
  type, `IntLit`, `FloatLit`, `StringLit` or `BoolLit`, or quoted code:
  `set_splice(r, quote string_of_int(n) end)` writes the quotation's body,
  its antiquotes filled, under fresh ids, so it shares none with the
  definition it came from. Written into a splice, that code is the
  client's: it is resolved in the client's scope, as all splice code is,
  so a quotation naming a builtin the client shadows means the client's
  binding there, where in a Macro expansion it would not. Code built with
  `Abs` is refused (it decodes only in a Macro expand), and so is a string
  holding a double quote, since Hazel's literals do not unescape and so
  cannot spell one: written into the text, it would corrupt it.
- **Value parameters**, the paper's `$slider 0 100`. The definition
  `fun p -> { ... }` is checked against `? -> Livelit`, so every member may
  use `p`, and the parameter's type is whatever its pattern says. Member
  types cannot mention a value, so an abbreviation has the definition's
  `Model`, `Action` and `Expansion`. `let ^b = ^a(args) in` makes `^b`
  `^a`'s definition applied to `args`: its `init`, `update`, `view` and
  `expand` all see them.
- **Type parameters**, which the paper does not have. The definition
  `typfun A -> { ... }` is checked against `forall A. Livelit`, with `A`
  abstract, so member types may mention it (`type Expansion = A`).
  `let ^b = ^a@<T> in` puts `T` in place of `A` in each member type.
  `typfun A -> fun p -> { ... }` is accepted, a value parameter inside a
  type parameter, but no test or slide instantiates one yet.
- **A parameterized livelit is not used directly.** `^slider(50)` is
  `LivelitNeedsArguments`, and a use of a type-parameterized one is
  `LivelitNeedsTypeArgument`, the paper's "missing livelit parameter".
  Either use synthesizes the unknown type.
- **Arguments are closed.** They are analyzed in the builtin context, as a
  quotation's body is, because `init` and `update` run there: an argument
  naming a client binding is an unbound variable where it is written.

Not yet: partial application (the paper's `let $uslider = $slider 0`),
since a definition takes one parameter, possibly a tuple, not a curried
sequence; a direct use with parameters, `$slider 0 100`, which would need a
second application slot beside the model's; and arguments that depend on
the client's scope.

The code: `UserLivelit.detect` reads `Fun` and `TypFun` around the module;
`UserLivelit.apply_args` and `UserLivelit.instantiate` build an
abbreviation's `LivelitCtx.raw_livelit`; `Statics.re` marks a use and
recognizes an abbreviation at its `let` (`livelit_abbrev_site`). Tests:
`test/Test_Parameters.re`, `test/Test_Either.re`.

## Checking a use

A use synthesizes the declared `Expansion`. Where a mismatch with it is
reported depends on `expand`'s kind. The **Livelits / Expansion Type Errors**
slides show three small examples of each.

- **Functional.** `expand` is an ordinary function, `Model -> Expansion`,
  checked where it is written. Its errors are ordinary type errors in the
  definition ("Expecting type String but got inconsistent type Int", on the
  body), and no use is marked.
- **Macro.** `expand` returns code, and a use means that code applied to the
  use's splices. The code must take each listed splice, at the type its code
  has, to `Expansion` (Fig. 5, premise 5). The splices are the client's, so
  this can only be checked at each use: Sec. 3.2.5's "validated at each
  livelit invocation site". A failure is `BadMacroExpansion`, on the use. It
  says which part failed, in terms the definition states, not the arrow type
  the check is phrased with, which appears nowhere in the program:
  - the result: "Applied to its splice, the expansion's code has type Int,
    but the livelit declares Expansion = String";
  - a parameter: "The expansion's code takes String for splice 1, whose code
    has type Int". Another use whose splice holds a string is fine, which is
    why the check is per use;
  - the shape: "The expansion's code has type Int, but must be a function of
    its splice, to Expansion = Int";
  - otherwise, when the types fit but the code has an error of its own: "The
    expansion's code has a type error". An error inside the quotation is also
    marked where it is written.

  The code's type is taken as the author wrote it (synthesized). Analysis
  against the arrow would fill each parameter in with its splice's type, and
  hide an annotated parameter that cannot take its splice.

`BadLivelitExpansion` ("Livelit expands to type Int, but declares Expansion =
String") remains for the non-Macro use-site check. Consistency, not equality,
is the test, as elsewhere in Hazel.

## Quotation: the plan

### What the paper requires

Fig. 3 l.55-57:

```
let expand : Model -> (Exp, List(SpliceRef)) =
  fun model -> (`fun r g b a -> (r, g, b, a)`,
                [model.r, model.g, model.b, model.a])
```

1. **The author writes the function over the splices.** `` `fun r g b a ->
   ...` `` is a quotation of a closed function with one parameter per listed
   splice. The system does not form this function; it checks that it is one
   (Sec. 3.2.5: "a function (here curried) that takes an argument for each
   listed SpliceRef ... So here, the parameterized expansion for $color must
   be a function of type Int -> Int -> Int -> Int -> Color").
2. **The list says which splice feeds which parameter.** It holds refs, not
   code: `expand` never sees the client's code (the splices are treated
   "parametrically").
3. **The system does the rest, at each use** (Fig. 5, Sec. 4.2):
   - it evaluates `expand(model)` to get the encoded expansion and the list
     (premise 3);
   - it decodes the encoding into an ordinary expression (premise 4);
   - it checks that expression against `τ1 -> ... -> τn -> Expansion`, where
     `τi` is each listed splice's declared type (premise 5), and marks the use
     with an error if not;
   - it expands each splice's code in the client's own context (premise 6);
   - the use **means the decoded function applied to the splice code**:
     `e_pexpansion(e_1)...(e_n)` (the rule's conclusion).
4. **Two binding properties fall out.** The quoted function must be closed, so
   it cannot depend on what happens to be in scope where the livelit is used
   (context independence). And the splices arrive as arguments, so a binder
   inside the expansion cannot capture a client variable, since application
   substitutes without capture (Sec. 2.4.3, Sec. 4.2).

So `Exp` is a staging device: `expand` produces a value of type `Exp`, and only
the livelit mechanism takes one apart, by decoding it into program code. No
Hazel program needs an `eval : Exp -> a`.

### How this maps onto this branch

- **Where the application happens.** `UserLivelit.mk_expand_dot` builds each
  use's elaboration: today a `case` on `^name.expand` whose `Macro` arm is an
  ascribed hole. With quotation, the Macro arm becomes the decoded function
  applied, in list order, to the code of each listed splice.
- **Where the splice code comes from.** It is already in the use's text (the
  parenthesized splices above), and `UserLivelit.expose_splice_refs` already
  pairs each ref with its code on every statics pass. So each listed
  `SpliceRef` resolves to the code at its position in the use's model.
- **When `expand` runs.** The paper runs `expand(model)` during expansion,
  before the program is checked. Two ways to do that here:
  - *at edit time*, like `init` and `update`: the editor evaluates
    `expand(model)` when a use's model or the definition changes, decodes the
    quotation, and the use elaborates to the application. The splice types
    are checked statically at the use and errors are ordinary static errors,
    as in the paper. This is the recommendation.
  - *at run time*: the Macro arm elaborates to a builtin that evaluates
    `g(model)`, decodes the quotation, evaluates it, and applies it to the
    values the refs carry. Smaller, but a type mismatch would only surface as
    a failed cast when the program runs.
- **Splice types.** Premise 5 needs each splice's declared type. It is
  kept in the text, as an ascription outside the splice: `new_splice(IntT,
  Some(IntLit(0)))` is written `(0 : Int)`, and the splice editor shows
  only `0`. Hazel's own ascription then checks the client's code against
  `Int` where it is written, and the code synthesizes `Int`, which is what
  premise 5 sees. A commit keeps each splice's type (read back by id from
  the text), and `set_splice` keeps the type of the splice it rewrites.
  Text with no ascription, like every use saved before this, still loads,
  typed by its code as before.

### `Exp`: quotation for code, lifting for values

- **Introduction.** `quote e end` quotes the expression `e`, written in
  ordinary Hazel syntax. Color's l.56 becomes
  `Macro(fun m -> (quote fun r -> fun g -> fun b -> fun a -> (r, g, b, a) end, [m.r, m.g, m.b, m.a]))`.
  (Hazel's `fun r g b a -> ...` is not a curried function: the parameters
  become a multi-hole, so the curried form is written out.)
- **No elimination form for authors.** Decoding belongs to the livelit
  mechanism. `Exp` can be abstract: authors build `Exp` values and hand them
  to `expand`, `new_splice` and `set_splice`, but do not inspect them.
  (Hazel's `Result` already carries values rather than `Exp`: `Val(v) |
  Indet`.)
- **Computed values need lifting, not quotation.** Fig. 3 l.49-52,
  `set_splice(model.r, IntLit(c.r))`, puts a number *computed by update* into
  the client's code. `quote c.r end` would quote the expression `c.r` itself, not its
  value. So lifting constructors stay, one per base type: `IntLit`,
  `FloatLit`, `StringLit` and `BoolLit`, each usable in `set_splice`,
  `new_splice`, or an antiquote (`quote unquote StringLit(s) end ++ "!" end`).
  Color needs no more than a quotation in `expand`: its quoted function is
  closed.
- **Building code: antiquotation, and names.** `unquote e end`, inside a
  quotation, is an antiquote: `e` is an `Exp` computed where the quotation is
  written, and its code is spliced in. With `Abs` (next), this lets
  `expand` build what no fixed quotation can: the row's
  `fun x0 -> fun x1 -> ... -> x0 :: x1 :: ... :: []`, for however many cells
  it has, which is how the paper says a livelit with a varying number of
  splices works (Sec. 3.2.5, its dataframe). `Ident(n)`, the variable named
  `n`, and `Lambda((n, c))`, `fun n -> c`, spell binders by name. They are
  unhygienic by design: keeping the names apart is the author's job, which
  is the Lisp `gensym` discipline. No example uses them except the
  counterexample slide.
- **Hygiene: `Abs(fun v -> c)`.** A binder the *system* names: decoding
  applies the function to a fresh variable whose name no program can write
  (`%v0`, `%v1`, ...), so no generated binder can capture another
  fragment's variable. The final decoding of a Macro use runs it, since it
  needs the evaluator; `%fill_quote` cannot, so it leaves an antiquote
  holding an `Abs` in place for that decoding. With the paper's two
  properties -- a splice is passed as an argument and cannot be captured,
  and the expansion is closed and cannot see the client's names -- this
  is the binding discipline in full. The **Livelits / Hygiene** folder
  shows each part on a slide of its own, beside what naive textual
  expansion would do.

### The `quote e end` syntax

Chosen after a spike on `'( e )` and a survey of the syntax in the
repository's 97 `.hz` files, counting code only:

- **Keyword forms are how Hazel introduces forms with special meaning**:
  `let`, `fun`, `case`, `do`, `typfun`, `type`, `module`, `test`. The two
  existing sigils mean other things: `^^name(e)` (411 uses) invokes a
  projector, a view that does *not* change what `e` means, which a
  quotation does; and `^name` (39) is a livelit.
- **`test e end` is an exact template**: it too wraps one expression and
  changes how it is treated, and `quote` follows it in `Form.re`
  (`mk_op_c(L, ["quote", "end"], Exp, [Exp])`) and in Menhir
  (`QUOTE; e = exp; END`).
- **`quote` was free**: it appeared nowhere as a name in any `.hz` file.
  `code` would not have been (67 uses as an identifier).
- **The paper's backtick is taken**: `` `...` `` is a quoted label (539 uses).
- **What the `'( e )` spike found** (branch `spike/quote-syntax`): it works
  in the tiles, but only by special-casing the token, because `'` is a name
  character and `(` never merges with a neighbour; typing passes through a
  lone `'`, a transient invalid token; and it sits beside the 44 primed names
  (`x'`, `row'`). Its slow parse was not the tick's fault: FastParse falls
  back on any form Menhir does not know, and Menhir already rejects primes.

`quote` is a reserved word, as `do` became.

### What `quote` does today

`Quote(e)` is a core expression form (`Grammar.re`), in the tiles, Menhir and
printing, so text containing one loads on FastParse's linear path.

- **Statics.** It synthesizes `Exp`. Its body is analyzed in the **builtin
  context**, not the lexical one, so a local variable named inside is an
  ordinary free-variable error there: the closedness Fig. 5 requires,
  reported early. The body's own type is not checked; that depends on the
  splices, and happens at each use (premise 5). A quotation contributes no
  uses of outer variables and no probe targets.
- **Dynamics.** It is a value, and its body is never evaluated or entered by
  substitution. It passes a cast to `Exp`. A constructor pattern does not
  match it, so `case q | IntLit(n) => ... | _ => ...` takes the last arm;
  `==` on two quotations is indeterminate.
- **Antiquotes.** A quotation containing them elaborates to
  `%fill_quote((quote <body with placeholders> end, [e_0, ...]))`, which
  fills each placeholder with the code its `Exp` denotes when the quotation
  is evaluated; the evaluator never sees an `unquote`. Each antiquote's `e` is
  analyzed in the quotation's lexical scope, against `Exp`, so its variables
  are uses there; the body stays in the builtin context. An `unquote` outside
  every quotation is an error (`BadOperator` for now).
- **A Macro use**: `expand(model)` is run while the use is checked, with each
  ref's code blanked; its answer is decoded (`BuiltinsADT.code_of_exp_value`:
  a quotation's body, or code spelled with `Lambda`, `Ident`, `IntLit`),
  checked against the splices' types and `Expansion`, and applied to the
  splices' values, read out of the model once bound (`%splice_value`).

`test/Test_Quote.re` covers each of these.

### Hygiene, by example

From the **Hygiene / 1. Capture Avoidance** slide. The program binds `x`; the
livelit's splice, edited in the widget, holds the client's code `x`, meaning
that `x`; and the expansion binds and uses an `x` of its own:

```
let x = 1 in

let ^add_ten = {
  type Model = (s = SpliceRef);
  type Action = Int;
  type Expansion = Int;
  let init =
    do s <- new_splice((IntT, Some(IntLit(0)))) in
    Pure((s = s));
  let update = fun m : Model -> fun _a : Action -> Pure(m);
  let view = fun m : Model ->
    do e <- editor((m.s, FixedWidth(6))) in
    Pure(Html.div([], [Html.text("s = "), e, Html.text(" + 10")]));
  let expand =
    Macro(fun m : Model ->
      (quote fun s -> let x = 10 in s + x end, [m.s]));
  let shape = Inline(16)
} in

^^livelit(^add_ten((s = (x))))
```

- **What it means: 11.** The use is the quoted function applied to the
  splice (Fig. 5): `(fun s -> let x = 10 in s + x)(x)`, with the splice's
  `x` evaluated where the client wrote it, so the argument is 1, and
  `1 + 10 = 11`. The expansion's `x` is internal to it.
- **What naive textual expansion would give: 20.** Pasting the client's code
  in for `s` gives `let x = 10 in x + x`: the expansion's `x` has captured
  the client's.
- **Why it cannot happen here.** A splice reaches the expansion only as an
  argument, and application substitutes without capture (Sec. 2.4.3). The
  converse holds too: the quotation is analyzed in the builtin context, so
  its body cannot name the client's `x` -- a free `x` in it would be an
  error where the quotation is written.

The folder's other slides: a client binding that shadows a builtin the
expansion uses (it means 3; naive, 0), and a generated two-splice
expansion whose binders a helper names, captured with `Lambda` (`[2, 2]`)
and not with `Abs` (`[1, 2]`).

### Maintenance: quotation as Hazel grows

Hazel will keep gaining expression forms. The question for its developers
is what a new form costs the quotation system, and the answer is designed
to be: nothing of its own, and a test says so when that stops being true.

**A quotation holds ordinary syntax.** `quote e end` is `Quote(e)` with `e`
the parsed term, whatever form it is. Nothing in quotation enumerates the
forms it can hold:

- *Statics* analyzes the body with the ordinary checker, in the builtin
  context (the `Quote` case of `Statics.re`). A new form is checked there
  exactly as it is anywhere else.
- *Dynamics* treat a quotation as a value whose body is never entered:
  evaluation stops at it (`Transition.re` counts it a value), and both
  substitutions pass over it (`Substitution.re`, and `DHExp.ty_subst` for
  types, each have a `Quote(_)` arm returning the quotation whole).
- *Antiquotes* are found, and later filled, by `Exp.map_term`: the
  `Quote` case of `Statics.re` collects them, `%fill_quote` fills them
  (`BuiltinsADT.fill_quote`), and decoding resolves any left over
  (`BuiltinsADT.code_of_exp_value`).

**So the one place a new form must be known is `Exp.map_term`**
(`TermBase.re`), and it must be known there for every other pass Hazel
has, not for quotation's sake. `map_term` is an exhaustive switch with no
default arm, so in the dev profile a form missing from it does not compile.

**What a new form in two weeks costs, case by case:**

- *An ordinary expression form*, whose sub-expressions `map_term` visits:
  nothing. It can be quoted, antiquoted inside, decoded and applied to
  splices the day it parses.
- *A form holding an expression `map_term` does not visit* (inside a
  payload it treats as opaque): an antiquote there would be silently left
  unfilled. This is the one real hazard, and the coverage test below
  catches it, because it places an antiquote in each form's child
  position.
- *A new binding form*: hygiene does not depend on knowing the binders.
  A splice reaches an expansion only as an argument, so capture avoidance
  is ordinary substitution, which a new binder must support to evaluate at
  all. The names the system generates, `%v0`, `%v1`, ... for `Abs` and
  `%unquote_i` for antiquote placeholders, start with `%`, which does not
  lex, so no binder any program writes can capture one, and none can
  capture a program's.

**Authors never name AST nodes.** The `Exp` constructors, `IntLit`,
`Ident`, `Lambda` and `Abs`, are not a mirror of the AST, and they do not
grow with it. Each exists for what a fixed quotation cannot write:

- `IntLit(n)` lifts a number *computed by* `update` into code
  (Fig. 3 l.49-52), since `quote c.r end` would quote the expression `c.r`.
- `Abs(fun v -> c)` makes a binder the system names, for code whose shape
  depends on the model.
- `Ident` and `Lambda` spell binders by name, unhygienically, and are kept
  for the counterexample slide.

Everything else is concrete syntax inside `quote ... end`, with
`unquote e end` where computed code goes. Dynamic Row or Column builds a
function of one parameter per cell, and the list of those parameters,
without naming a single node:

```
let list_of = fun vars : [Exp] ->
  case vars
  | [] => quote [] end
  | v :: rest => quote unquote v end :: unquote list_of(rest) end end
  end;
let gen = fun vars : [Exp] -> fun i : Int ->
  if i == n then list_of(vars)
  else Abs(fun v -> gen(vars @ [v])(i + 1))
```

`::` and `[]` here, like `fun` in the code `Abs` decodes to, are written or
produced as syntax. A new form is used the same way: write it inside a
quotation.

What *does* grow with Hazel is lifting, one constructor per base type:
`IntLit`, `FloatLit`, `StringLit` and `BoolLit` today, so a new base type
wants a lift of its own. That grows with Hazel's *types*, not its forms,
and is two short cases, one in each decoder (`BuiltinsADT.code_of_exp_value`
and `SpliceStore.code_of_exp`), plus a constructor in `exp_typ`. Values of
compound types (tuples, lists) have no lift; their code can be built with
antiquotes over the lifts of their parts.

**The coverage test, `test/Test_QuoteCoverage.re`**, makes the claim
checkable, in two layers:

- *Compile time.* It has an exhaustive switch over `Exp.cls` with no default
  arm, giving every form a `Sample` of closed concrete syntax or an
  `Exempt` with a reason (made by elaboration or evaluation, or malformed
  input). `Exp.cls_of_term` is itself exhaustive, so a new constructor in
  `Grammar.re` forces a new `Exp.cls` tag, which forces an entry here.
- *Run time.* It iterates `Exp.all_of_cls`, which `ppx_enumerate` derives,
  so no tag can be skipped, and for each sample checks that `quote <sample>
  end` parses to a quotation whose body is that form, evaluates to the
  same quotation unchanged, and decodes back to the body. Where the sample
  marks a child position, it checks that an antiquote there is filled
  exactly as the code written in place.

Both layers are exercised, not only green: making `map_term` skip a list
literal's elements fails the run for `ListLit` and `ListConcat`, and
deleting one arm of the switch fails the build (warning 8, which CI's
strict-warning gate, `make ci-check`, makes an error). CI's Full tests run
the group with the rest.

Exempt forms are the ones no author writes: holes made by evaluation
(`DynamicErrorHole`, `Closure`), elaboration's products (`BuiltinFun`, and
the Nat and SInt literals and operators, which are written with the Int
syntax), malformed input (`Invalid`, `MultiHole`), `Unquote` (the
mechanism itself, covered by the `Quote` group), `Parens` (never a class)
and `Projector`, which is transparent to semantics, so a quotation of
`^^fold(e)` holds `e`. Forms that occur only inside another, a tuple's
labeled item or a partial application's `_`, have samples that contain
them.

## Design decisions

Decisions taken on the way here, kept because the alternatives looked
reasonable and may be proposed again. (Folded in from `livelits-full.md`, the
plan this branch was written against.)

- **`init` runs; it is not rewritten as syntax.** A livelit's `init` used to
  be pasted into the program as text when a use was created, before any
  statics or evaluation existed, so `new_splice` could not run there. The
  cheaper option was to read `new_splice(Int, Some(0))` in `init`'s text as a
  name for the parens a splice is marked by, keeping `init` as syntax. That
  buys a declared type but not a list of splices whose length is decided
  when the use is made. So the editor performs `init` as an `UpdateCmd` when
  a use is created (`Triggers.expand_livelit` into
  `UpdateCmdRunner.init_model`), which is also the faithful reading of Fig. 3.
- **`set_splice` makes ownership a convention.** Before it, `update` could not
  write a spliced field at all, so "a livelit may read the client's code but
  not rewrite it" held by construction. With `set_splice` (Fig. 3 l.49-52,
  Color's *teal* button) a livelit can overwrite the client's code, and
  respecting it is up to the author.
- **`eval_splice` answers `Val | Indet`, not a bare value.** Handing the view a
  value directly (as the earlier refs-in-the-model design did) left no way to
  say a splice has no value: delete a splice's code and the widget got a hole
  without asking. With `Indet` the widget decides what to show, as the row
  slide does when it counts empty cells.
- **`Functional` avoids capture rather than solving it.** Its `expand`
  computes a value from splices already evaluated in the client's scope, so
  no expansion code ever surrounds client code and nothing can be captured.
  The Macro arm is where capture avoidance is actually needed, and where the
  paper's argument-passing provides it.
- **Names.** This branch briefly spelled the splice operations
  `splice_new`, `splice_set`, `splice_eval`; they are the paper's
  `new_splice`, `set_splice`, `eval_splice` now. The older spelling survives
  in some commit messages.

## Built-in livelits

A built-in livelit implements `LivelitCtx.BuiltinLivelit` in OCaml and is
converted to a raw livelit by `raw_of_builtin`:

```reasonml
module type BuiltinLivelit = {
  let name: string;
  type model_t;
  type expansion_t;
  type action_t;

  let hazel_model_t: TermBase.Typ.t;
  let model_to_hazel: model_t => model_exp;
  let model_from_hazel: model_exp => option(model_t);
  let model_default: model_t;

  let hazel_expansion_t: TermBase.Typ.t;
  let expand: model_t => expansion_t;
  let expand_to_hazel: expansion_t => expansion_exp;

  let hazel_action_t: TermBase.Typ.t;
  let action_to_hazel: action_t => action_exp;
  let action_from_hazel: action_exp => option(action_t);

  let update: (action_t, model_t) => model_t;
  let view:
    (model_t, action_t => Ui_effect.t(unit)) => Virtual_dom.Vdom.Node.t;
  let shape: ProjectorShape.t;
};
```

Register it in the `livelits` list at the end of `src/language/Livelit.re`,
which today holds `Js`:

```reasonml
let livelits: list(raw_livelit) =
  [(module Js), (module YourNewLivelit)] |> List.map(raw_of_builtin);
```

Style livelits in `src/web/www/style/projectors/proj-livelit.css`.

## Where the code is

| | |
|---|---|
| the signature, `Functional` / `Macro`, `Exp`, commands | `src/language/builtins/BuiltinsADT.re` |
| checking a definition, splice refs, a use's elaboration | `src/language/statics/UserLivelit.re` |
| performing commands | `src/haz3lcore/projectors/UpdateCmdRunner.re`, `ViewCmdRunner.re` |
| the splice store in the text | `src/haz3lcore/projectors/SpliceStore.re` |
| the widget | `src/haz3lcore/projectors/implementations/LivelitProj.re` |
| example programs | `hazel-programs/docs/livelits/` |
