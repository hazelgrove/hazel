/* Controls how ExpToSegment inserts automatic newlines during pretty-printing.

      This affects the layout of generated code segments, particularly for
      block structures (let, type, if, case, etc.) and compound literals
      (tuples, lists).

      The three modes form a hierarchy of increasing "expansion":
      - Inline: everything on one line
      - Block: multi-line for statements, but tuples/lists stay compact
      - ExpandElements: multi-line everywhere, including within tuples/lists
   */

[@deriving (show({with_path: false}), sexp, yojson)]
type t =
  | Inline
  /* No automatic newlines anywhere. Everything renders on a single line.
     Used for small inline UI displays like CursorInspector, ContextInspector,
     and type annotations where compact output is needed. */
  | Block
  /* Adds newlines after block structures: let bindings, type aliases,
     use statements, if/then/else branches, case arms, and sequences.
     Tuples and list literals remain on a single line.
     This is the standard "pretty" output mode used for most code rendering:
     CLI output, projectors, stepper, eval results, etc. */
  | ExpandElements;
/* Like Block, but ALSO adds newlines after each comma in tuples and
   list literals. Used for external projector bridges where expanded
   layout of compound data is desired. */
