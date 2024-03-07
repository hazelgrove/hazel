WIP

The Hazel implementation is split into three parts: `hazelcore`, `hazelweb`, and `pretty`.

# `hazelcore`
`hazelcore` implements the semantics of Hazel, independent of any particular user interface.

## Coding Standards
Code in `hazelcore`should be pure OCaml.

  - no side effects (except memoization)
  - no js dependencies

## Module Organization

Users edit external expressions, of type `UHExp.t`, via edit actions. External
expressions are elaborated to internal expressions, of type `DHExp.t`, for
evaluation. The external and internal languages share a type system. Types are
of type `Typ.t`.

  - external language syntax:
    - (mention convention in terms of `UH` vs `Z` prefixes)
    - central modules `UHExp`, `UHPat` `UHTyp`
      - shared forms `Seq`, `Skel`, `OpSeq`
        - (should `OpSeq` be prefixed by `UH`?)
      - `Var`, `InjSide`,
      - `ErrStatus`, `VarErrStatus`
      - `MetaVar` (hole numbers are metavars)
    - syntax + cursor `ZExp`, `ZPat`, `ZTyp`
      - shared forms `ZSeq` `ZOpSeq`
      - `CursorPosition`
        - `CharIndex`
        - `DelimIndex`
        - `OpIndex`
        - `Side`
      - `CursorPath_common`, `CursorPath_Exp`, `CursorPath_Pat`, `CursorPath_Typ`
  - typechecking:
    - `Statics_common`, `Statics_Exp`, `Statics_Pat`
    - `HTyp`
    - `Contexts`, `VarCtx`, `VarMap`
  - edit action semantics:
    - `Action_common`, `Action_Exp`, `Action_Pat`, `Action_Typ`
    - `ExpandingKeyword`
    - `IDGen`
    - `TextShape`
  - dynamics
    - internal syntax: `DHExp`, `DHPat`
      - external expressions are for editing
      - need to elaborate external expressions to internal in order to insert casts
        and closure information
      - see POPL 2019, external expressions use variable e, internal use variable d
    - elaboration from external syntax to internal syntax:
      - `Dynamics`
      - `MetaVarMap`
      - `Delta`
      - `MetaVarInst`
    - evaluation
      - `Dynamics`
      - `Environment`
      - `HoleInstance`
      - `HoleInstanceInfo`
      - `InstancePath` (used by context inspector)
  - editor services:
    - `CursorInfo_common`, `CursorInfo_Exp`, `CursorInfo_Pat`, `CursorInfo_Typ`
    - `UsageAnalysis`
    - `CodeHistory`
  - livelits
    - `Monads`
- `pretty` -- pretty printing library
  -
- `hazelweb` contains the code pertaining to web interface for Hazel.

We use
  [`js_of_ocaml`](https://ocsigen.org/js_of_ocaml)
  to compile from OCaml to JavaScript. The main HTML file is contained in the
  `www` subdirectory; upon load, the compiled `hazel.js` program starts at
  the `Main` module, which inserts content in the specified element.

  The `Main` module implements the Hazel UI by following a model-view-update
  architecture using Jane Street's [`Incr_dom`](https://github.com/janestreet/incr_dom) library. For a gentle overview of model-view-update, read through the section
  titled [The Elm Architecture](https://guide.elm-lang.org/architecture/) in
  the Elm documentation.

  - `www`:
    the assets (e.g. `fonts`, `imgs`, `style.css`, `index.html`) loaded
    along with the compiled `hazel.js` program
  - `Main`: kicks off `Incr_dom`
  - `Hazel`: defines `Incr_dom` component
  - `Logger`: janky logging system for logging user edit actions
  - `layout`: client directory of `pretty` library
    - `doc/layout` for external expressions
      - `UHAnnot` annotations on `doc/layout`
        - (we use "term" to range over expressions, patterns, and types)
        - `TermShape`: various term shapes that are handled differently visually
        - `TermSort` exp vs pat vs typ
      - `UHDoc_common`, `UHDoc_Exp`, `UHDoc_Pat`, `UHDoc_Typ` all possible choices of layout
      - `UHLayout` final layout chosen by `pretty`
    - `doc/layout` for internal expressions
      - `DHAnnot` annotations on `doc/layout`
      - `DHDoc_common`, `DHDoc_Exp`, `DHDoc_Pat`, `DHDoc_Typ` all possible choices of layout
      - `DHLayout` final layout chosen by `pretty`
    - `doc/layout` for `HTyp`
      - `HTypAnnot`, `HTypDoc`, `HTypLayout`
  - `Update`: top level update for `Incr_dom` model-view-update
  - `State`: an additional module in the `Incr_dom` API that supports
    storing external async process state
  - `Palettes`: WIP, will be renamed to `Livelits` once `livelits` branch is merged
  - model
    - `Model`: top level model for `Incr_dom` model-view-update
    - `Program`:
      entry point to any Hazel program, provides functions for
      acquiring semantic info as well as layout concerns, goes
      beyond `Statics.edit_state` in that it contains information like
      current editor width, whether its focused, etc...
    - `Result`: result of evaluating a Hazel program
    - `UndoHistory`: undo logic
    - `UserSelectedInstances`:
      In Hazel, users can select and inspect closures produced in the
      result of a program. Keeps track of which closure is selected.
    - `CursorMap`:
      In Hazel, we roll our own caret positioning logic in `CursorMap`.
      - TODO: move this into documentation for `CursorMap`
        Maps (row, col) positions to program tree paths. Provides
        functions for looking a program tree position near or at
        some (row, col) position. Generated once a layout is chosen.
    - `Card`, `Cardstack`, `Cardstacks`:
      - currently Hazel has a single `Program` per `Card`, `Card`s
      are organized into `Cardstack`s
      - there may be different `Cardstack`s for different purposes
        (e.g., Hazel tutorial, RC study, see `hazelweb/cardstacks/builtins`),
        the currently selected cardstack is determined by the state of
        `Cardstacks`
      - each `Card` and `Cardstack` comes with accompying `CardInfo` and
        `CardstackInfo`
      - `ZCard` is the selected card in a `Cardstack`, it differs from
        `Card` in that it contains a full `Program` as opposed to
        a `Statics.edit_state`
  -


  - TODO add annotated screenshot of Hazel UI