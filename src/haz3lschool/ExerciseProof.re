// open Sexplib.Std;
open Haz3lcore;
module Tree = Util.Tree;

module Model = {
  // Exercise Model Structure
  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation('code) = {
    jdmt: 'code,
    rule: Derivation.Rule.t,
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type derivation_tree('code) = Tree.p(derivation('code));

  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('code) = {
    prelude: 'code,
    derivation_tree: derivation_tree('code),
  };

  [@deriving (show({with_path: false}), sexp, yojson)]
  type pos =
    | Prelude
    | Derive(Tree.pos);

  // Utility functions
  let map = (f: 'a => 'b, m: p('a)): p('b) => {
    prelude: m.prelude |> f,
    derivation_tree:
      m.derivation_tree
      |> Tree.map(({jdmt, rule}) => {jdmt: jdmt |> f, rule}),
  };

  let mapi = (f, m: p('a)): p('b) => {
    prelude: f(Prelude, m.prelude),
    derivation_tree:
      m.derivation_tree
      |> Tree.mapi((pos, {jdmt, rule}) => {
           {jdmt: f(Derive(pos), jdmt), rule}
         }),
  };

  let nth = (m: p('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Derive(pos) => Tree.nth(m.derivation_tree, pos).jdmt
    };

  let map_nth = (f, m: p('a), pos: pos): p('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Derive(pos) => {
        ...m,
        derivation_tree:
          Tree.map_nth(
            ({jdmt, rule}) => {jdmt: jdmt |> f, rule},
            m.derivation_tree,
            pos,
          ),
      }
    };

  let flatten = m =>
    [m.prelude] @ (m.derivation_tree |> Tree.map(m => m.jdmt) |> Tree.flatten);

  // Config Functions/Parameters
  let switch_editor = (~pos: pos, instructor_mode: bool): bool => {
    ignore(instructor_mode);
    ignore(pos);
    true;
  };

  let switch_derivation_rule =
      (~pos: Tree.pos, ~m: p('a), ~rule: Derivation.Rule.t): p('a) => {
    let tree = m.derivation_tree;
    let {jdmt, _} = Tree.nth(tree, pos);
    let tree = Tree.put_nth({jdmt, rule}, tree, pos);
    {...m, derivation_tree: tree};
  };

  let derivation_init_wrapper = (init: unit => 'a): derivation('a) => {
    jdmt: init(),
    rule: Derivation.Rule.Assumption,
  };

  let add_premise = (~pos: Tree.pos, ~m: p('a), ~index: int, ~init): p('a) => {
    let init = derivation_init_wrapper(init);
    {
      ...m,
      derivation_tree: Tree.insert(init, index, m.derivation_tree, pos),
    };
  };

  let del_premise = (~pos: Tree.pos, ~m: p('a), ~index: int): p('a) => {
    ...m,
    derivation_tree: Tree.remove(index, m.derivation_tree, pos) |> snd,
  };

  let readonly_in = (pos: pos, instructor_mode: bool): bool => {
    ignore(instructor_mode);
    ignore(pos);
    false;
  };

  let visible_in = (pos: pos, instructor_mode): bool => {
    ignore(instructor_mode);
    ignore(pos);
    true;
  };

  let init = (init: pos => 'a): p('a) => {
    prelude: Prelude |> init,
    derivation_tree:
      Tree.init(
        derivation_init_wrapper(init(Derive(Value)) |> Fun.const)
        |> Fun.const,
      ),
  };

  let fill = (m: p('a), init: pos => 'b): p('b) => {
    prelude: Prelude |> init,
    derivation_tree:
      Tree.mapi(
        (pos, _) =>
          derivation_init_wrapper(init(Derive(pos)) |> Fun.const),
        m.derivation_tree,
      ),
  };
};

module Stitch = {
  type p('a) = {
    prelude: 'a, // prelude
    derivation_tree: Tree.p('a) // prelude + derivation
  };

  let key = (pos: Model.pos): string =>
    switch (pos) {
    | Prelude => "prelude"
    | Derive(pos) =>
      let rec aux = (acc, pos: Tree.pos) =>
        switch (pos) {
        | Value => acc
        | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
        };
      aux("derivation", pos);
    };

  let map = (f: 'a => 'b, m: p('a)): p('b) => {
    prelude: m.prelude |> f,
    derivation_tree: m.derivation_tree |> Tree.map(f),
  };

  let mapi = (f: (Model.pos, 'a) => 'b, m: p('a)): p('b) => {
    prelude: m.prelude |> f(Prelude),
    derivation_tree: m.derivation_tree |> Tree.mapi(pos => Derive(pos) |> f),
  };

  let nth = (m: p('a), pos: Model.pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Derive(pos) => Tree.nth(m.derivation_tree, pos)
    };

  let map_nth = (f, m: p('a), pos: Model.pos): p('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Derive(pos) => {
        ...m,
        derivation_tree: Tree.map_nth(f, m.derivation_tree, pos),
      }
    };

  let flatten = m => [m.prelude] @ (m.derivation_tree |> Tree.flatten);

  let init = (init: Model.pos => 'a): p('a) => {
    prelude: Prelude |> init,
    derivation_tree: Tree.init(Derive(Value) |> init |> Fun.const),
  };

  let fill = (m: Model.p('a), init: Model.pos => 'b): p('b) => {
    prelude: Prelude |> init,
    derivation_tree:
      Tree.mapi((pos, _) => Derive(pos) |> init, m.derivation_tree),
  };
};
