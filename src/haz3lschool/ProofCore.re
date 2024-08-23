open Util;
module Derivation = Haz3lcore.Derivation;

[@deriving (show({with_path: false}), sexp, yojson)]
type tree('a) = Tree.p('a);

module Abbr = {
  [@deriving (show({with_path: false}), sexp, yojson)]
  type p('a) =
    | Just('a)
    | Abbr(option(index))
  and index = int;

  let get_just =
    fun
    | Just(a) => a
    | Abbr(_) => failwith("Abbr.get_just: Abbr");

  let get_just_opt =
    fun
    | Just(a) => Some(a)
    | Abbr(_) => None;

  let map_just = f =>
    fun
    | Just(a) => Just(f(a))
    | Abbr(i) => Abbr(i);

  let update_before_add = index =>
    fun
    | Abbr(Some(i)) when i >= index => Abbr(Some(i + 1))
    | _ as a => a;

  let update_after_del = index =>
    fun
    | Abbr(Some(i)) when i == index => Abbr(None)
    | Abbr(Some(i)) when i > index => Abbr(Some(i - 1))
    | _ as a => a;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type abbr('a) = Abbr.p('a);

[@deriving (show({with_path: false}), sexp, yojson)]
type abbr_trees('a) = list(tree(abbr('a)));

[@deriving (show({with_path: false}), sexp, yojson)]
type model('code) = {
  prelude: 'code,
  setup: 'code,
  trees: abbr_trees(deduction('code)),
}
and deduction('code) = {
  jdmt: 'code,
  rule: option(Derivation.Rule.t),
};

let map_jdmt = f => Abbr.map_just(d => {...d, jdmt: f(d.jdmt)});

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | Setup
  | Trees(int, Tree.pos);

module ModelUtil = {
  // Utility functions
  let map = (f: 'a => 'b, m: model('a)): model('b) => {
    prelude: f(m.prelude),
    setup: f(m.setup),
    trees: m.trees |> List.map(Tree.map(map_jdmt(f))),
  };

  let mapi = (f: (pos, 'a) => 'b, m: model('a)): model('b) => {
    prelude: f(Prelude, m.prelude),
    setup: f(Setup, m.setup),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi(pos => map_jdmt(f(Trees(i, pos))))),
  };

  let nth = (m: model('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Setup => m.setup
    | Trees(i, pos) =>
      m.trees
      |> List.nth(_, i)
      |> Tree.nth(_, pos)
      |> Abbr.get_just
      |> (d => d.jdmt)
    };

  let map_nth = (f: 'a => 'a, m: model('a), pos: pos): model('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: m.prelude |> f}
    | Setup => {...m, setup: m.setup |> f}
    | Trees(i, pos) => {
        ...m,
        trees:
          m.trees |> ListUtil.map_nth(i, Tree.map_nth(map_jdmt(f), _, pos)),
      }
    };

  let flatten = (m: model('a)): list('a) =>
    [m.prelude, m.setup]
    @ (
      m.trees
      |> List.map(Tree.flatten)
      |> List.concat
      |> List.filter_map(Abbr.get_just_opt)
      |> List.map(d => d.jdmt)
    );

  // Config Functions/Parameters
  let switch_editor = (pos: pos, instructor_mode: bool): bool => {
    ignore(pos);
    ignore(instructor_mode);
    true;
  };

  let readonly_in = (pos: pos, instructor_mode: bool, model: model('a)): bool => {
    switch (pos) {
    | Prelude => !instructor_mode
    | Trees(i, Value) when i + 1 == List.length(model.trees) =>
      !instructor_mode
    | _ => false
    };
  };

  let visible_in = (pos: pos, instructor_mode): bool => {
    ignore(pos);
    ignore(instructor_mode);
    true;
  };

  let derivation_init_wrapper = (init: unit => 'a): abbr(deduction('a)) => {
    Just({jdmt: init(), rule: None});
  };

  let init = (init: pos => 'a): model('a) => {
    prelude: Prelude |> init,
    setup: Setup |> init,
    trees: [
      Tree.empty(
        derivation_init_wrapper(init(Trees(0, Value)) |> Fun.const),
      ),
    ],
  };

  let fill = (m: model('a), init: pos => 'b): model('b) => {
    prelude: init(Prelude),
    setup: init(Setup),
    trees:
      m.trees
      |> List.mapi(i =>
           Tree.mapi((pos, _) =>
             derivation_init_wrapper(init(Trees(i, pos)) |> Fun.const)
           )
         ),
  };
};

// UI functionality

let get_trees_pos =
  fun
  | Trees(i, pos) => (i, pos)
  | _ as pos => failwith("ProofCore.get_trees_pos: " ++ show_pos(pos));

let add_premise = (m: model('a), ~pos, ~index, ~init: unit => 'a): model('a) => {
  let (i, pos) = get_trees_pos(pos);
  let premise = Abbr.Just({jdmt: init(), rule: None});
  let trees =
    m.trees
    |> List.nth(_, i)
    |> Tree.insert(premise, index, _, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {...m, trees};
};

let del_premise = (m: model('a), ~pos): model('a) => {
  let (i, pos) = get_trees_pos(pos);
  let (index, pos) = Tree.pos_split_last(pos);
  let trees =
    m.trees
    |> List.nth(_, i)
    |> Tree.remove(index, _, pos)
    |> snd
    |> ListUtil.put_nth(i, _, m.trees);
  {...m, trees};
};

let add_abbr = (m: model('a), ~index, ~init: unit => 'a): model('a) => {
  let abbr = Tree.empty(Abbr.Just({jdmt: init(), rule: None}));
  let trees =
    m.trees
    |> List.mapi(i =>
         i >= index ? Tree.map(Abbr.update_before_add(index)) : Fun.id
       )
    |> ListUtil.insert(abbr, _, index);
  {...m, trees};
};

let del_abbr = (m: model('a), ~index): model('a) => {
  let trees =
    m.trees
    |> ListUtil.remove(_, index)
    |> List.mapi(i =>
         i >= index ? Tree.map(Abbr.update_after_del(index)) : Fun.id
       );
  {...m, trees};
};

// TODO(zhiyao): might need to separate two
let del_premise = (m: model('a), ~pos): model('a) =>
  switch (get_trees_pos(pos)) {
  | (index, Value) => del_abbr(m, ~index)
  | _ => del_premise(m, ~pos)
  };

let switch_rule =
    (m: model('a), ~pos: pos, ~rule, ~init: unit => 'a): model('a) => {
  let (i, pos) = get_trees_pos(pos);
  let tree = List.nth(m.trees, i);
  let trees =
    tree
    |> Tree.nth(_, pos)
    |> Abbr.get_just_opt
    |> Option.map(d => d.jdmt)
    |> Option.value(~default=init())
    |> (jdmt => Abbr.Just({jdmt, rule}))
    |> Tree.put_nth(_, tree, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {...m, trees};
};

let switch_abbr = (m: model('a), ~pos: pos, ~index): model('a) => {
  let (i, pos) = get_trees_pos(pos);
  let tree = List.nth(m.trees, i);
  let trees =
    Abbr.Abbr(index)
    |> Tree.empty
    |> Tree.put_nth_node(_, tree, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {...m, trees};
};

let bind_none = l => [Option.none] @ (l |> List.map(Option.some));
let all_rules = Derivation.Rule.all |> bind_none;
let all_abbrs = pos =>
  pos |> get_trees_pos |> fst |> List.init(_, Fun.id) |> bind_none;

[@deriving (show({with_path: false}), sexp, yojson)]
type stitched('a) = {
  prelude: 'a, // prelude
  setup: 'a,
  trees: list(Tree.p(option('a))) // prelude + derivation
};

module StitchUtil = {
  let stitch = (stitch2: ('a, 'a) => 'a, m: model('a)): stitched('a) => {
    let prelude = m.prelude; //|> wrap_filter(FilterAction.Eval);
    let setup = stitch2(prelude, m.setup);
    {
      prelude,
      setup,
      trees:
        m.trees
        |> List.mapi(i =>
             Tree.mapi(pos =>
               fun
               | Abbr.Just(d)
                   when i + 1 == List.length(m.trees) && pos == Value =>
                 Some(stitch2(prelude, d.jdmt))
               | Just(d) => Some(stitch2(setup, d.jdmt))
               | Abbr(_) => None
             )
           ),
    };
  };

  let key = (pos: pos): string =>
    switch (pos) {
    | Prelude => "prelude"
    | Setup => "setup"
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
    setup: f(s.setup),
    trees: s.trees |> List.map(Tree.map(Option.map(f))),
  };

  let mapi = (f: (pos, 'a) => 'b, m: stitched('a)): stitched('b) => {
    prelude: m.prelude |> f(Prelude),
    setup: m.setup |> f(Setup),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi(pos => Option.map(f(Trees(i, pos))))),
  };

  let nth = (m: stitched('a), pos: pos): 'a =>
    switch (pos) {
    | Prelude => m.prelude
    | Setup => m.setup
    | Trees(i, pos) =>
      m.trees |> List.nth(_, i) |> Tree.nth(_, pos) |> Option.get
    // |> Option.value(~default=failwith("ProofCore.StitchUtil.nth: None"))
    };

  let map_nth = (f: 'a => 'a, m: stitched('a), pos: pos): stitched('a) =>
    switch (pos) {
    | Prelude => {...m, prelude: f(m.prelude)}
    | Setup => {...m, setup: f(m.setup)}
    | Trees(i, pos) => {
        ...m,
        trees:
          m.trees
          |> ListUtil.map_nth(i, Tree.map_nth(Option.map(f), _, pos)),
      }
    };

  let flatten = (m: stitched('a)): list('a) =>
    [m.prelude, m.setup]
    @ (
      m.trees
      |> List.map(Tree.flatten)
      |> List.concat
      |> List.filter_map(Fun.id)
    );

  let init = (init: pos => 'a): stitched('a) => {
    prelude: init(Prelude),
    setup: init(Setup),
    trees: [Tree.empty(Some(init(Trees(0, Value))))],
  };

  let fill = (m: model('a), init: pos => 'b): stitched('b) => {
    prelude: init(Prelude),
    setup: init(Setup),
    trees:
      m.trees
      |> List.mapi(i => Tree.mapi((pos, _) => Some(init(Trees(i, pos))))),
  };
};
