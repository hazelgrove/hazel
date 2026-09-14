open Alcotest;
open Language;
module Printer = Haz3lcore.Printer;
module ExpToSegment = Haz3lcore.ExpToSegment;

/* The Adapton types the Fumola panel carries, checked against json the
   runtime actually produced.

   The panel cannot ask a program what `Edge` means -- it is not inside one --
   so FumolaAdapton declares these types in OCaml and FumolaValue pushes them
   down through the value. That only works if the declarations match the
   shapes the runtime emits, and nothing else would catch a mismatch: a wrong
   type does not fail, it degrades. A constructor whose sum does not declare
   it comes back unannotated, a record whose labels do not line up comes back
   as a bare tuple, and a symbol comes back as its text. All of those render
   -- just wrongly, and only a reader who knew the shape would notice.

   These two samples are verbatim from `prim "adaptonPeekHistory" ()` on the
   instance the `Node info` slide runs. */

let node_row_json = {|{"tag":"Record","value":{"metaTime":{"tag":"Int","value":"1"},"node":{"tag":"Variant","value":{"name":"nonThunk","value":{"tag":"Int","value":"41"}}},"nodeId":{"tag":"Tuple","value":[{"tag":"Variant","value":{"name":"Symbol","value":{"tag":"Symbol","value":{"tag":"Num","value":"1"}}}},{"tag":"Variant","value":{"name":"Now","value":null}},{"tag":"Int","value":"1"}]}}}|};

let edge_row_json = {|{"tag":"Record","value":{"edge":{"tag":"Record","value":{"action":{"tag":"Variant","value":{"name":"put","value":{"tag":"Int","value":"41"}}},"align":{"tag":"Variant","value":{"name":"aligned","value":null}},"metaTimes":{"tag":"Tuple","value":[{"tag":"Int","value":"1"},{"tag":"Int","value":"1"}]},"source":{"tag":"Tuple","value":[{"tag":"Variant","value":{"name":"Here","value":null}},{"tag":"Variant","value":{"name":"Now","value":null}},{"tag":"Int","value":"0"}]},"target":{"tag":"Tuple","value":[{"tag":"Variant","value":{"name":"Symbol","value":{"tag":"Symbol","value":{"tag":"Num","value":"1"}}}},{"tag":"Variant","value":{"name":"Now","value":null}},{"tag":"Int","value":"1"}]}}},"edgeId":{"tag":"Variant","value":{"name":"edgeId","value":{"tag":"Int","value":"1001"}}},"metaTime":{"tag":"Int","value":"1"}}}|};

/* No pointer appears in either sample, so nothing is followed and the eval
   hook is never called. */
let no_eval = (_: string): Yojson.Safe.t => `Null;

let translate = (~ana, source: string): TermBase.Exp.t =>
  switch (
    FumolaValue.exp_of_json(
      ~instance_id=0,
      ~eval=no_eval,
      ~ana,
      ~tools=FumolaAdapton.tools,
      Yojson.Safe.from_string(source),
    )
  ) {
  | Ok(exp) => exp
  | Error(message) => failwith("translation failed: " ++ message)
  };

/* Printed rather than compared structurally: what the panel shows IS this
   text, so the test says what a reader will see. */
let printed = (exp: TermBase.Exp.t): string =>
  ExpToSegment.exp_to_segment(
    ~settings=ExpToSegment.Settings.of_core(~inline=true, CoreSettings.on),
    exp,
  )
  |> Printer.of_segment(~holes="?", ~refractors=[]);

let contains = (needle: string, haystack: string): bool => {
  let n = String.length(needle);
  let rec go = i =>
    i + n > String.length(haystack)
      ? false : String.sub(haystack, i, n) == needle || go(i + 1);
  go(0);
};

let has = (label, needle, text) =>
  check(bool, label ++ ": " ++ needle, true, contains(needle, text));

let node_row_translates = () => {
  let text =
    printed(translate(~ana=FumolaAdapton.node_row(), node_row_json));
  /* The labels survive, so it is a record and not a bare tuple. */
  has("node row", "metaTime", text);
  has("node row", "nodeId", text);
  /* nonThunk is recased and resolved against Node, so it is a constructor of
     that sum rather than a free name. */
  has("node row", "NonThunk", text);
  /* The space carries the symbol's structure, not its text: this is the
     difference from BuiltinsADT.Space, and what lets a node be matched with
     the symbol in an event. */
  has("node row", "Symbol", text);
  has("node row", "Num", text);
  has("node row", "Now", text);
};

let edge_row_translates = () => {
  let text =
    printed(translate(~ana=FumolaAdapton.edge_row(), edge_row_json));
  has("edge row", "edgeId", text);
  has("edge row", "EdgeId", text);
  /* The three fields of the row, including the metaTime the slide's peekInfo
     types never see. */
  has("edge row", "metaTime", text);
  /* The edge's own five. */
  has("edge row", "source", text);
  has("edge row", "target", text);
  has("edge row", "action", text);
  has("edge row", "metaTimes", text);
  has("edge row", "align", text);
  /* Both recased and resolved: Put against Action, Aligned against Align. */
  has("edge row", "Put", text);
  has("edge row", "Aligned", text);
  has("edge row", "Here", text);
};

let tests = (
  "FumolaAdapton",
  [
    test_case("a node row translates", `Quick, node_row_translates),
    test_case("an edge row translates", `Quick, edge_row_translates),
  ],
);
