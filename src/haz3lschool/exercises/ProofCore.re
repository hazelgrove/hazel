open Util;
module Derivation = Haz3lcore.Derivation;

[@deriving (show({with_path: false}), sexp, yojson)]
type derivation('code) = {
  jdmt: 'code,
  rule: Derivation.Rule.t,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type derivation_tree('code) = Tree.p(derivation('code));

[@deriving (show({with_path: false}), sexp, yojson)]
type model('code) = {
  prelude: 'code,
  tree: derivation_tree('code),
};

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | Tree(Tree.pos);
module ModelUtil = {
  // Utility functions
  let map = (f: 'a => 'b, m: model('a)): model('b) => {
    prelude: m.prelude |> f,
    tree: m.tree |> Tree.map(({jdmt, rule}) => {jdmt: jdmt |> f, rule}),
  };

  let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) => {
    prelude: f(Prelude, m.prelude),
    tree:
      m.tree
      |> Tree.mapi((pos, {jdmt, rule}) => {
           {jdmt: f(Tree(pos), jdmt), rule}
         }),
  };

  let nth = (m: model('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Tree(pos) => Tree.nth(m.tree, pos).jdmt
    };

  let map_nth = (f: 'a => 'a, m: model('a), pos: pos): model('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Tree(pos) => {
        ...m,
        tree:
          Tree.map_nth(
            ({jdmt, rule}) => {jdmt: jdmt |> f, rule},
            m.tree,
            pos,
          ),
      }
    };

  let flatten = (m: model('a)): list('a) =>
    [m.prelude] @ (m.tree |> Tree.map(m => m.jdmt) |> Tree.flatten);

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
      (~pos: Tree.pos, ~m: model('a), ~rule: Derivation.Rule.t): model('a) => {
    let tree = m.tree;
    let {jdmt, _} = Tree.nth(tree, pos);
    let tree = Tree.put_nth({jdmt, rule}, tree, pos);
    {...m, tree};
  };

  let derivation_init_wrapper = (init: unit => 'a): derivation('a) => {
    jdmt: init(),
    rule: Derivation.Rule.Assumption,
  };

  let add_premise =
      (~pos: Tree.pos, ~m: model('a), ~index: int, ~init): model('a) => {
    let init = derivation_init_wrapper(init);
    {...m, tree: Tree.insert(init, index, m.tree, pos)};
  };

  let del_premise = (~pos: Tree.pos, ~m: model('a), ~index: int): model('a) => {
    ...m,
    tree: Tree.remove(index, m.tree, pos) |> snd,
  };

  let init = (init: pos => 'a): model('a) => {
    prelude: Prelude |> init,
    tree:
      Tree.init(
        derivation_init_wrapper(init(Tree(Value)) |> Fun.const) |> Fun.const,
      ),
  };

  let fill = (m: model('a), init: pos => 'b): model('b) => {
    prelude: Prelude |> init,
    tree:
      Tree.mapi(
        (pos, _) => derivation_init_wrapper(init(Tree(pos)) |> Fun.const),
        m.tree,
      ),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type stitched('a) = {
  prelude: 'a, // prelude
  tree: Tree.p('a) // prelude + derivation
};

module StitchUtil = {
  let stitch = (stitch2: ('a, 'a) => 'a, model: model('a)): stitched('a) => {
    let prelude_term = model.prelude; //|> wrap_filter(FilterAction.Eval);
    {
      prelude: prelude_term,
      tree:
        model.tree
        |> Util.Tree.map(({jdmt, _}) => stitch2(prelude_term, jdmt)),
    };
  };
  let key = (pos: pos): string =>
    switch (pos) {
    | Prelude => "prelude"
    | Tree(pos) =>
      let rec aux = (acc, pos: Tree.pos) =>
        switch (pos) {
        | Value => acc
        | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
        };
      aux("derivation", pos);
    };

  let map = (f: 'a => 'b, m: stitched('a)): stitched('b) => {
    prelude: m.prelude |> f,
    tree: m.tree |> Tree.map(f),
  };

  let mapi = (f: (pos, 'a) => 'b, m: stitched('a)): stitched('b) => {
    prelude: m.prelude |> f(Prelude),
    tree: m.tree |> Tree.mapi(pos => Tree(pos) |> f),
  };

  let nth = (m: stitched('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Tree(pos) => Tree.nth(m.tree, pos)
    };

  let map_nth = (f: 'a => 'a, m: stitched('a), pos: pos): stitched('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Tree(pos) => {...m, tree: Tree.map_nth(f, m.tree, pos)}
    };

  let flatten = (m: stitched('a)): list('a) =>
    [m.prelude] @ (m.tree |> Tree.flatten);

  let init = (init: pos => 'a): stitched('a) => {
    prelude: Prelude |> init,
    tree: Tree.init(Tree(Value) |> init |> Fun.const),
  };

  let fill = (m: model('a), init: pos => 'b): stitched('b) => {
    prelude: Prelude |> init,
    tree: Tree.mapi((pos, _) => Tree(pos) |> init, m.tree),
  };
};
