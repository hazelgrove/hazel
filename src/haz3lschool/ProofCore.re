open Util;
module Derivation = Haz3lcore.Derivation;

[@deriving (show({with_path: false}), sexp, yojson)]
type model('code) = {
  prelude: 'code,
  trees: list(tree('code)),
}
and tree('code) = Tree.p(deduction_abbr('code))
and deduction_abbr('code) =
  | Just(deduction('code))
  | Abbr(index)
and index = int
and deduction('code) = {
  jdmt: 'code,
  rule: Derivation.Rule.t,
};

let map_jdmt = f =>
  fun
  | Just(d) => Just({...d, jdmt: f(d.jdmt)})
  | Abbr(i) => Abbr(i);

let get_jdmt_opt =
  fun
  | Just(d) => Some(d.jdmt)
  | Abbr(_) => None;

let get_jdmt =
  fun
  | Just(d) => d.jdmt
  | Abbr(_) => failwith("ProofCore.get_jdmt: Abbr");

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | Trees(int, Tree.pos);

module ModelUtil = {
  // Utility functions
  let map = (f: 'a => 'b, m: model('a)): model('b) => {
    prelude: f(m.prelude),
    trees: m.trees |> List.map(Tree.map(map_jdmt(f))),
  };

  let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) => {
    prelude: f(Prelude, m.prelude),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi(pos => map_jdmt(f(Trees(i, pos))))),
  };

  let nth = (m: model('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Trees(i, pos) =>
      m.trees |> List.nth(_, i) |> Tree.nth(_, pos) |> get_jdmt
    };

  let map_nth = (f: 'a => 'a, m: model('a), pos: pos): model('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Trees(i, pos) => {
        ...m,
        trees:
          m.trees |> ListUtil.map_nth(i, Tree.map_nth(map_jdmt(f), _, pos)),
      }
    };

  let flatten = (m: model('a)): list('a) =>
    [m.prelude]
    @ (
      m.trees
      |> List.map(Tree.flatten)
      |> List.concat
      |> List.filter_map(get_jdmt_opt)
    );

  // Config Functions/Parameters
  let switch_editor = (pos: pos, instructor_mode: bool): bool => {
    ignore(pos);
    ignore(instructor_mode);
    true;
  };

  let readonly_in = (pos: pos, instructor_mode: bool): bool => {
    ignore(pos);
    ignore(instructor_mode);
    false;
  };

  let visible_in = (pos: pos, instructor_mode): bool => {
    ignore(pos);
    ignore(instructor_mode);
    true;
  };

  let switch_derivation_rule =
      (~pos: pos, ~m: model('a), ~rule: Derivation.Rule.t): model('a) =>
    switch (pos) {
    | Prelude => m // Prelude is not editable
    | Trees(i, pos) =>
      let jdmt = nth(m, Trees(i, pos));
      let tree =
        Tree.put_nth(Just({jdmt, rule}), List.nth(m.trees, i), pos);
      {...m, trees: ListUtil.put_nth(i, tree, m.trees)};
    };

  let derivation_init_wrapper = (init: unit => 'a): deduction_abbr('a) => {
    Just({jdmt: init(), rule: Derivation.Rule.Assumption});
  };

  let add_premise =
      (~pos: pos, ~m: model('a), ~index: int, ~init): model('a) => {
    switch (pos) {
    | Prelude => m // Prelude is not editable
    | Trees(i, pos) =>
      let init = derivation_init_wrapper(init);
      let tree = Tree.insert(init, index, List.nth(m.trees, i), pos);
      {...m, trees: ListUtil.put_nth(i, tree, m.trees)};
    };
  };

  let del_premise = (~pos: pos, ~m: model('a), ~index: int): model('a) => {
    switch (pos) {
    | Prelude => m // Prelude is not editable
    | Trees(i, pos) =>
      let (_, tree) = Tree.remove(index, List.nth(m.trees, i), pos);
      {...m, trees: ListUtil.put_nth(i, tree, m.trees)};
    };
  };

  let init = (init: pos => 'a): model('a) => {
    prelude: Prelude |> init,
    trees: [
      Tree.init(
        derivation_init_wrapper(init(Trees(0, Value)) |> Fun.const)
        |> Fun.const,
      ),
    ],
  };

  let fill = (m: model('a), init: pos => 'b): model('b) => {
    prelude: init(Prelude),
    trees:
      m.trees
      |> List.mapi(i =>
           Tree.mapi((pos, _) =>
             derivation_init_wrapper(init(Trees(i, pos)) |> Fun.const)
           )
         ),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type stitched('a) = {
  prelude: 'a, // prelude
  trees: list(Tree.p(option('a))) // prelude + derivation
};

module StitchUtil = {
  let stitch = (stitch2: ('a, 'a) => 'a, m: model('a)): stitched('a) => {
    let prelude_term = m.prelude; //|> wrap_filter(FilterAction.Eval);
    {
      prelude: prelude_term,
      trees:
        m.trees
        |> List.map(
             Tree.map(
               fun
               | Just(d) => Some(stitch2(prelude_term, d.jdmt))
               | Abbr(_) => None,
             ),
           ),
    };
  };
  let key = (pos: pos): string =>
    switch (pos) {
    | Prelude => "prelude"
    | Trees(i, pos) =>
      let rec aux = (acc, pos: Tree.pos) =>
        switch (pos) {
        | Value => acc
        | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
        };
      "derivation_" ++ string_of_int(i) ++ "_" ++ aux("", pos);
    };

  let map = (f: 'a => 'b, s: stitched('a)): stitched('b) => {
    prelude: f(s.prelude),
    trees: s.trees |> List.map(Tree.map(Option.map(f))),
  };

  let mapi = (f: (pos, 'a) => 'b, m: stitched('a)): stitched('b) => {
    prelude: m.prelude |> f(Prelude),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi(pos => Option.map(f(Trees(i, pos))))),
  };

  let nth = (m: stitched('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Trees(i, pos) =>
      m.trees |> List.nth(_, i) |> Tree.nth(_, pos) |> Option.get
    // |> Option.value(~default=failwith("ProofCore.StitchUtil.nth: None"))
    };

  let map_nth = (f: 'a => 'a, m: stitched('a), pos: pos): stitched('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: f(m.prelude)}
    | Trees(i, pos) => {
        ...m,
        trees:
          m.trees
          |> ListUtil.map_nth(i, Tree.map_nth(Option.map(f), _, pos)),
      }
    };

  let flatten = (m: stitched('a)): list('a) =>
    [m.prelude]
    @ (
      m.trees
      |> List.map(Tree.flatten)
      |> List.concat
      |> List.filter_map(Fun.id)
    );

  let init = (init: pos => 'a): stitched('a) => {
    prelude: init(Prelude),
    trees: [Tree.init(Some(init(Trees(0, Value))) |> Fun.const)],
  };

  let fill = (m: model('a), init: pos => 'b): stitched('b) => {
    prelude: init(Prelude),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi((pos, _) => Some(init(Trees(i, pos))))),
  };
};
