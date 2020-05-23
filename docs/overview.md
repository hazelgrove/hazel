WIP

The Hazel implementation is split into three parts: `hazelcore`, `hazelweb`, and `pretty`.
- `hazelcore`
  - coding standards
    - no side effects (except memoization)
    - no js dependencies
  - semantics of Hazel
    - external syntax:
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
        - `CursorPath`
    - typechecking:
      - `Statics`
      - `HTyp`
      - `Contexts`, `VarCtx`, `VarMap`
    - edit action semantics:
      - `Action`
      - `ExpandingKeyword`
      - `MetaVarGen`
      - `TextShape`
    - dynamics
      - internal syntax: `DHExp`, `DHPat`
        - external expressions are for editing
        - need to expand external expressions to internal in order to insert casts
          and closure information
        - see POPL 2019, external expressions use variable e, internal use variable d
      - expansion from external syntax to internal syntax:
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
      - `CursorInfo`
      - `UsageAnalysis`
      - `CodeHistory`
    - livelits
      - `Monads`
- `hazelweb` contains the code pertaining to web interface for Hazel. The web
  interface follows a model-view-update architecture using Jane Street's
  [`Incr_dom`](https://github.com/janestreet/incr_dom) library. We use
  [`js_of_ocaml`](https://ocsigen.org/js_of_ocaml)
  to compile from OCaml to JavaScript.
  - `Main`: kicks off `Incr_dom`
  - `Hazel`: defines `Incr_dom` component
  - `Logger`: janky logging system for logging user edit actions
  - layout: client directory of `pretty` library
    - doc/layout for external expressions
      - `UHAnnot` annotations on doc/layout
        - (we use "term" to range over expressions, patterns, and types)
        - `TermShape`: various term shapes that are handled differently visually
        - `TermSort` exp vs pat vs typ
      - `UHDoc` all possible choices of layout
      - `UHLayout` final layout chosen by `pretty`
    - doc/layout for internal expressions
      - `DHAnnot` annotations on doc/layout
      - `DHDoc` all possible choices of layout
      - `DHLayout` final layout chosen by `pretty`
    - doc/layout for `HTyp`
      - `HTypAnnot`, `HTypDoc`, `HTypLayout`
  - model
    - cardstacks:
      -

  - TODO add annotated screenshot of Hazel UI