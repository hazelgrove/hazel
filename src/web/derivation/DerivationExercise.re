open Util_web;
open Haz3lcore;

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
type p('code) = {
  id: Id.t,
  title: string,
  module_name: string,
  prompt: string,
  max_points: int,
  prelude: 'code,
  setup: 'code,
  rule_set: Language.RuleImage.rule_set,
  trees: abbr_trees(deduction('code)),
}
and deduction('code) = {
  jdmt: 'code,
  rule: option(Language.RuleImage.t),
};

let map_jdmt = f =>
  Abbr.map_just(d =>
    {
      ...d,
      jdmt: f(d.jdmt),
    }
  );

[@deriving (show({with_path: false}), sexp, yojson)]
type pos =
  | Prelude
  | Setup
  | Trees(int, Tree.pos);

let root_of_pos = (pos: pos): Sort.t =>
  switch (pos) {
  | Prelude => Exp
  | Setup => Exp
  | Trees(_, _) => Drv(Exp)
  };

// UI functionality

let zipper_of_code = (code, ~root) => {
  switch (Parser.to_zipper(code, ~root)) {
  | None => failwith("Transition failed.")
  | Some(zipper) => zipper
  };
};

let init = (~root: Sort.t) =>
  "" |> zipper_of_code(~root) |> Editor.Model.mk(~root);

let get_trees_pos =
  fun
  | Trees(i, pos) => (i, pos)
  | _ as pos =>
    failwith("DerivationExercise.get_trees_pos: " ++ show_pos(pos));

/* A fresh empty deduction node (blank conclusion, no selected rule). */
let blank_deduction = (~pos: pos): abbr(deduction('a)) =>
  Abbr.Just({
    jdmt: init(~root=root_of_pos(pos)),
    rule: None,
  });

let add_premise = (m: p('a), ~pos, ~index): p('a) => {
  let (i, tree_pos) = get_trees_pos(pos);
  let trees =
    m.trees
    |> List.nth(_, i)
    |> Tree.insert(blank_deduction(~pos), index, _, tree_pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {
    ...m,
    trees,
  };
};

/* Remove a premise subtree at [pos]. (Internal helper — the exported
   [del_premise] below additionally handles the top-level-abbr case.) */
let del_premise_internal = (m: p('a), ~pos): p('a) => {
  let (i, pos) = get_trees_pos(pos);
  let (index, pos) = Tree.pos_split_last(pos);
  let trees =
    m.trees
    |> List.nth(_, i)
    |> Tree.remove(index, _, pos)
    |> snd
    |> ListUtil.put_nth(i, _, m.trees);
  {
    ...m,
    trees,
  };
};

let add_abbr = (m: p('a), ~index): p('a) => {
  let abbr = Tree.empty(blank_deduction(~pos=Trees(0, Value)));
  let trees =
    m.trees
    |> List.mapi(i =>
         i >= index ? Tree.map(Abbr.update_before_add(index)) : Fun.id
       )
    |> ListUtil.insert(abbr, _, index);
  {
    ...m,
    trees,
  };
};

let del_abbr = (m: p('a), ~index): p('a) => {
  let trees =
    m.trees
    |> ListUtil.remove(_, index)
    |> List.mapi(i =>
         i >= index ? Tree.map(Abbr.update_after_del(index)) : Fun.id
       );
  {
    ...m,
    trees,
  };
};

/* Remove the selected premise; if the selection is a top-level abbreviation
   tree (at [Value] position), remove the whole abbreviation. */
let del_premise = (m: p('a), ~pos): p('a) =>
  switch (get_trees_pos(pos)) {
  | (index, Value) => del_abbr(m, ~index)
  | _ => del_premise_internal(m, ~pos)
  };

let pop_premise = (m: p('a), ~pos): p('a) => {
  let (index, pos) = get_trees_pos(pos);
  let abbr = m.trees |> List.nth(_, index) |> Tree.nth_node(_, pos);
  let trees =
    m.trees
    |> List.mapi(i =>
         i >= index ? Tree.map(Abbr.update_before_add(index)) : Fun.id
       )
    |> ListUtil.map_nth(
         index,
         Tree.put_nth_node(Tree.empty(Abbr.Abbr(Some(index))), _, pos),
       )
    |> ListUtil.insert(abbr, _, index);
  {
    ...m,
    trees,
  };
};

let push_premise = (m: p('a), ~pos): p('a) => {
  let (index, pos) = get_trees_pos(pos);
  let addr_index =
    switch (m.trees |> List.nth(_, index) |> Tree.nth(_, pos)) {
    | Abbr.Abbr(Some(i)) => i
    | _ => failwith("DerivationExercise.push_premise: not an abbreviation")
    };
  let abbr = m.trees |> List.nth(_, addr_index);
  let trees =
    m.trees |> ListUtil.map_nth(index, Tree.put_nth_node(abbr, _, pos));
  {
    ...m,
    trees,
  };
};

let switch_rule = (m: p('a), ~pos: pos, ~rule): p('a) => {
  let root = root_of_pos(pos);
  let (i, pos) = get_trees_pos(pos);
  let tree = List.nth(m.trees, i);
  let trees =
    tree
    |> Tree.nth(_, pos)
    |> Abbr.get_just_opt
    |> Option.map(d => d.jdmt)
    |> Option.value(~default=init(~root))
    |> (
      jdmt =>
        Abbr.Just({
          jdmt,
          rule,
        })
    )
    |> Tree.put_nth(_, tree, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {
    ...m,
    trees,
  };
};

let switch_abbr = (m: p('a), ~pos: pos, ~index): p('a) => {
  let (i, pos) = get_trees_pos(pos);
  let tree = List.nth(m.trees, i);
  let trees =
    Abbr.Abbr(index)
    |> Tree.empty
    |> Tree.put_nth_node(_, tree, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {
    ...m,
    trees,
  };
};

/* Menu helpers: pre-pend [None] to a list of options so the UI can render
   a "no selection" choice before the real entries. */
let with_none = l => [None, ...List.map(Option.some, l)];
let all_abbrs = pos =>
  pos |> get_trees_pos |> fst |> List.init(_, Fun.id) |> with_none;

[@deriving (show({with_path: false}), sexp, yojson)]
type hint = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type spec = p(Zipper.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type transitionary_spec = p(string);

let farthest_pos = (pos: pos, editors: p('a)): pos =>
  switch (pos) {
  | Prelude => Prelude
  | Setup => Setup
  | Trees(i, pos) =>
    let i = min(i, List.length(editors.trees) - 1);
    let tree = List.nth(editors.trees, i);
    let farthest =
      Tree.farthest_cond(
        fun
        | Abbr.Abbr(_) => false
        | Just(_) => true,
        tree,
        pos,
      );
    Trees(i, farthest);
  };

let map = (p: p('a), f: 'a => 'b): p('b) => {
  {
    id: p.id,
    title: p.title,
    module_name: p.module_name,
    prompt: p.prompt,
    max_points: p.max_points,
    prelude: p.prelude |> f,
    setup: p.setup |> f,
    rule_set: p.rule_set,
    trees: p.trees |> List.map(Tree.map(map_jdmt(f))),
  };
};

let mapi = (p: p('a), f: (pos, 'a) => 'b): p('b) => {
  {
    id: p.id,
    title: p.title,
    module_name: p.module_name,
    prompt: p.prompt,
    max_points: p.max_points,
    prelude: p.prelude |> f(Prelude),
    setup: p.setup |> f(Setup),
    rule_set: p.rule_set,
    trees:
      p.trees
      |> List.mapi(i => Tree.mapi(pos => map_jdmt(f(Trees(i, pos))))),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {eds};

let main_editor_of_state = (~selection: pos, eds) =>
  switch (selection) {
  | Prelude => eds.prelude
  | Setup => eds.setup
  | Trees(i, pos) =>
    eds.trees
    |> List.nth(_, i)
    |> Tree.nth(_, pos)
    |> Abbr.get_just
    |> (d => d.jdmt)
  };

let put_main_editor = (~selection: pos, eds: p('a), editor: 'a): p('a) =>
  switch (selection) {
  | Prelude => {
      ...eds,
      prelude: editor,
    }
  | Setup => {
      ...eds,
      setup: editor,
    }
  | Trees(i, pos) =>
    let trees =
      eds.trees
      |> ListUtil.map_nth(i, Tree.map_nth(map_jdmt(_ => editor), _, pos));
    {
      ...eds,
      trees,
    };
  };

let update_title = (eds: p('a), title: string): p('a) => {
  ...eds,
  title,
};

let update_module_name = (eds: p('a), module_name: string): p('a) => {
  ...eds,
  module_name,
};

let update_prompt = (eds: p('a), prompt: string): p('a) => {
  ...eds,
  prompt,
};

let editors = (eds: p('a)) =>
  [eds.prelude, eds.setup]
  @ (
    eds.trees
    |> List.map(Tree.flatten)
    |> List.concat
    |> List.filter_map(Abbr.get_just_opt)
    |> List.map(d => d.jdmt)
  );

let editor_positions = eds =>
  [Prelude, Setup]
  @ (
    eds.trees
    |> List.mapi(i => Tree.mapi(pos => map_jdmt(_ => Trees(i, pos))))
    |> List.map(Tree.flatten)
    |> List.concat
    |> List.filter_map(Abbr.get_just_opt)
    |> List.map(d => d.jdmt)
  );

let positioned_editors = state =>
  List.combine(editor_positions(state), editors(state));

let transition: transitionary_spec => spec =
  mapi(_, pos => zipper_of_code(_, ~root=root_of_pos(pos)));

/* ---------- stitching ------------------------------------------------------
   A derivation exercise is rendered as three independently editable regions
   (prelude, setup, and the derivation trees), but statics/dynamics need to
   see them as a single program. [stitch_term] produces that combined view
   while remembering which editor each stitched term came from. */

module TermItem = {
  type t = {
    term: Language.Exp.t,
    editor: Editor.t,
  };
};

module StaticsItem = {
  type t = CachedStatics.t;
};

[@deriving (show({with_path: false}), sexp, yojson)]
type stitched('a) = {
  prelude: 'a, // prelude
  setup: 'a,
  trees: list(Tree.p(option('a))) // prelude + derivation
};

let map_stitched = (f: (pos, 'a) => 'b, s: stitched('a)): stitched('b) => {
  prelude: s.prelude |> f(Prelude),
  setup: s.setup |> f(Setup),
  trees:
    s.trees
    |> List.mapi(i => Tree.mapi(pos => Option.map(f(Trees(i, pos))))),
};

let get_stitched = (pos, s: stitched('a)): 'a =>
  switch (pos) {
  | Prelude => s.prelude
  | Setup => s.setup
  | Trees(i, pos) =>
    s.trees
    |> List.nth(_, i)
    |> Tree.nth(_, pos)
    |> OptUtil.value_exn(
         ~none=Invalid_argument("DerivationExercise.get_stitched"),
       )
  };

let put_stitched = (pos, s: stitched('a), x: 'a): stitched('a) =>
  switch (pos) {
  | Prelude => {
      ...s,
      prelude: x,
    }
  | Setup => {
      ...s,
      setup: x,
    }
  | Trees(i, pos) => {
      ...s,
      trees:
        s.trees
        |> ListUtil.map_nth(i, Tree.map_nth(Option.map(_ => x), _, pos)),
    }
  };

let wrap = (term, editor: Editor.t): TermItem.t => {
  term,
  editor,
};

let term_of = (editor: Editor.t): Language.Exp.t =>
  MakeTerm.from_zip_for_sem(editor.state.zipper, ~root=editor.root).term;

let stitch_term = (eds: p('a)): stitched(TermItem.t) => {
  let prelude_term = eds.prelude |> term_of;
  let setup_term = EditorUtil.append_exp(prelude_term, eds.setup |> term_of);
  {
    prelude: wrap(prelude_term, eds.prelude),
    setup: wrap(setup_term, eds.setup),
    trees:
      eds.trees
      |> List.map(
           Tree.map(
             fun
             | Abbr.Just(d) =>
               Some(
                 wrap(
                   EditorUtil.append_exp(setup_term, d.jdmt |> term_of),
                   d.jdmt,
                 ),
               )
             | Abbr(_) => None,
           ),
         ),
  };
};
let stitch_term = Core.Memo.general(stitch_term);

let prelude_key = "prelude";
let setup_key = "setup";
let trees_key = (i, pos) => {
  let rec aux = (acc, pos: Tree.pos) =>
    switch (pos) {
    | Value => acc
    | Children(i, pos) => aux(acc ++ "_" ++ string_of_int(i), pos)
    };
  "derivation_" ++ string_of_int(i) ++ aux("", pos);
};

let key_for_statics = (pos: pos): string =>
  switch (pos) {
  | Prelude => prelude_key
  | Setup => setup_key
  | Trees(i, pos) => trees_key(i, pos)
  };

let pos_of_key = (key: string): pos =>
  switch () {
  | _ when key == prelude_key => Prelude
  | _ when key == setup_key => Setup
  | _ when String.starts_with(key, ~prefix="derivation_") =>
    let i = String.index(key, '_');
    let key = String.sub(key, i + 1, String.length(key) - i - 1);
    try({
      let i = String.index(key, '_');
      let n = String.sub(key, 0, i) |> int_of_string;
      let key = String.sub(key, i + 1, String.length(key) - i - 1);
      let rec aux = (key: string): Tree.pos =>
        try({
          let i = String.index(key, '_');
          let n = String.sub(key, 0, i) |> int_of_string;
          let key = String.sub(key, i + 1, String.length(key) - i - 1);
          Children(n, aux(key));
        }) {
        | Not_found => Children(key |> int_of_string, Value)
        };
      Trees(n, aux(key));
    }) {
    | Not_found => Trees(key |> int_of_string, Value)
    };
  | _ => failwith("invalid key")
  };

// // Module Export

let editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let serialization = Zipper.show(zipper);
  // let string_literal = "\"" ++ String.escaped(serialization) ++ "\"";
  Format.pp_print_string(fmt, serialization);
};

let export_module = ({eds, _}: state) => {
  let prefix = "let exercise: Exercise.t = Derivation ";
  let record = show_p(editor_pp, eds);
  let data = prefix ++ record ++ "\n";
  data;
};

/* Export a derivation doc slide as a `DerivationExercise.spec` value
   (the format consumed by `Init.documentation_drv_slides`). */
let export_doc_slide_module = (eds: eds) => {
  let prefix = "let exercise : DerivationExercise.spec = ";
  let record = show_p(editor_pp, eds);
  prefix ++ record ++ "\n";
};

let transitionary_editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let code = PersistentZipper.to_string(zipper);
  Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
};

let export_transitionary_module = (_module_name, {eds, _}: state) => {
  let prefix = "let exercise: Exercise.t = Derivation (DerivationExercise.transition(";
  let record = show_p(transitionary_editor_pp, eds);
  let data = prefix ++ record ++ "))\n";
  data;
};

let blank_spec = (~title, ~module_name): spec => {
  let ts: transitionary_spec = {
    id: Id.mk(),
    title,
    module_name,
    prompt: "TODO: prompt",
    max_points: 10,
    prelude: "",
    setup: "",
    rule_set: Language.RuleImage.PropositionalLogic,
    trees: [
      Tree.empty(
        Abbr.Just({
          jdmt: "",
          rule: None,
        }),
      ),
    ],
  };
  transition(ts);
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_state = p(PersistentZipper.t);
