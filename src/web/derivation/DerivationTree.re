open Util;
open Haz3lcore;

let output_header_grading = _module_name =>
  "module Exercise = GradePrelude.Exercise\n" ++ "let prompt = ()\n";

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
  title: string,
  version: int,
  module_name: string,
  prompt: string,
  prelude: 'code,
  setup: 'code,
  corpus: Language.RuleImage.corpus,
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

let init = (~root: Sort.t) => "" |> zipper_of_code(~root) |> Editor.Model.mk;

let get_trees_pos =
  fun
  | Trees(i, pos) => (i, pos)
  | _ as pos => failwith("ProofCore.get_trees_pos: " ++ show_pos(pos));

let add_premise = (m: p('a), ~pos, ~index): p('a) => {
  let root = root_of_pos(pos);
  let (i, pos) = get_trees_pos(pos);
  let premise =
    Abbr.Just({
      jdmt: init(~root),
      rule: None,
    });
  let trees =
    m.trees
    |> List.nth(_, i)
    |> Tree.insert(premise, index, _, pos)
    |> ListUtil.put_nth(i, _, m.trees);
  {
    ...m,
    trees,
  };
};

let del_premise = (m: p('a), ~pos): p('a) => {
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
  let abbr =
    Tree.empty(
      Abbr.Just({
        jdmt: init(~root=root_of_pos(Trees(0, Value))),
        rule: None,
      }),
    );
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

// Note(zhiyao): might need to separate two
let del_premise = (m: p('a), ~pos): p('a) =>
  switch (get_trees_pos(pos)) {
  | (index, Value) => del_abbr(m, ~index)
  | _ => del_premise(m, ~pos)
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
    | _ => failwith("ProofCore.push_premise: Not an abbreviation")
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

let bind_none = l => [Option.none] @ (l |> List.map(Option.some));
let all_rules = Language.Rule.all |> bind_none;
let all_abbrs = pos =>
  pos |> get_trees_pos |> fst |> List.init(_, Fun.id) |> bind_none;

[@deriving (show({with_path: false}), sexp, yojson)]
type hint = string;

[@deriving (show({with_path: false}), sexp, yojson)]
type key = (string, int);

let key_of = p => {
  (p.title, p.version);
};

let find_key_opt = (key, specs: list(p('code))) => {
  specs |> Util.ListUtil.findi_opt(spec => key_of(spec) == key);
};

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
    title: p.title,
    version: p.version,
    module_name: p.module_name,
    prompt: p.prompt,
    prelude: p.prelude |> f,
    setup: p.setup |> f,
    corpus: p.corpus,
    trees: p.trees |> List.map(Tree.map(map_jdmt(f))),
  };
};

let mapi = (p: p('a), f: (pos, 'a) => 'b): p('b) => {
  {
    title: p.title,
    version: p.version,
    module_name: p.module_name,
    prompt: p.prompt,
    prelude: p.prelude |> f(Prelude),
    setup: p.setup |> f(Setup),
    corpus: p.corpus,
    trees:
      p.trees
      |> List.mapi(i => Tree.mapi(pos => map_jdmt(f(Trees(i, pos))))),
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type eds = p(Editor.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type state = {eds};

let key_of_state = eds => key_of(eds);

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

let idx_of_pos = (pos, _: p('code)) =>
  switch (pos) {
  | Prelude => 0
  | Setup => 1
  | Trees(i, _) => 2 + i // NOTE(zhiyao): hard to calculate
  };

let pos_of_idx = (_: p('code), idx: int) =>
  switch (idx) {
  | 0 => Prelude
  | 1 => Setup
  | _ =>
    if (idx < 2) {
      failwith("element idx");
    } else {
      Trees(idx - 2, Value);
    }
  };

let derivation_init_wrapper = (): abbr(deduction('a)) => {
  Just({
    jdmt: init(~root=root_of_pos(Trees(0, Value))),
    rule: None,
  });
};

let transition: transitionary_spec => spec =
  mapi(_, pos => zipper_of_code(_, ~root=root_of_pos(pos)));

let eds_of_spec = (eds, ~settings as _: Language.CoreSettings.t) =>
  map(eds, Editor.Model.mk);

//
// Old version of above that did string-based parsing, may be useful
// for transitions between zipper data structure versions (TODO)
//

// # Stitching

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
         ~none=Invalid_argument("DerivationTree.get_stitched"),
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
  MakeTerm.from_zip_for_sem(editor.state.zipper).term;

let stitch3 = (ed1: Editor.t, ed2: Editor.t, ed3: Editor.t) =>
  EditorUtil.append_exp(
    EditorUtil.append_exp(term_of(ed1), term_of(ed2)),
    term_of(ed3),
  );

let stitch_term = (eds: p('a)): stitched(TermItem.t) => {
  let prelude_term = eds.prelude |> term_of;
  let setup_term = EditorUtil.append_exp(prelude_term, eds.setup |> term_of);
  {
    prelude: wrap(prelude_term, eds.prelude),
    setup: wrap(setup_term, eds.setup),
    trees:
      eds.trees
      |> List.mapi(i =>
           Tree.mapi(pos =>
             fun
             | Abbr.Just(d)
                 when i + 1 == List.length(eds.trees) && pos == Value =>
               Some(
                 wrap(
                   EditorUtil.append_exp(prelude_term, d.jdmt |> term_of),
                   d.jdmt,
                 ),
               )
             | Just(d) =>
               Some(
                 wrap(
                   EditorUtil.append_exp(setup_term, d.jdmt |> term_of),
                   d.jdmt,
                 ),
               )
             | Abbr(_) => None
           )
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
  let prefix = "let exercise: DerivationTree.spec = ";
  let record = show_p(editor_pp, eds);
  let data = prefix ++ record ++ "\n";
  data;
};

let transitionary_editor_pp = (fmt, editor: Editor.t) => {
  let zipper = editor.state.zipper;
  let code = PersistentZipper.to_string(zipper);
  Format.pp_print_string(fmt, "\"" ++ String.escaped(code) ++ "\"");
};

let export_transitionary_module = (module_name, {eds, _}: state) => {
  let prefix =
    "let prompt = "
    ++ module_name
    ++ "_prompt.prompt\n"
    ++ "let exercise: Exercise.spec = Exercise.transition(";
  let record = show_p(transitionary_editor_pp, eds);
  let data = prefix ++ record ++ ")\n";
  data;
};

let export_grading_module = (module_name, {eds, _}: state) => {
  let header = output_header_grading(module_name);
  let prefix = "let exercise: Exercise.spec = ";
  let record = show_p(editor_pp, eds);
  let data = header ++ prefix ++ record ++ "\n";
  data;
};

let blank_spec = (~title, ~module_name) => {
  let prelude = Zipper.next_blank();
  let setup = Zipper.next_blank();
  let trees = [
    Tree.empty(
      Abbr.Just({
        jdmt: Zipper.next_blank(),
        rule: None,
      }),
    ),
  ];
  {
    title,
    version: 1,
    module_name,
    prompt: "TODO: prompt",
    prelude,
    setup,
    corpus: Language.RuleImage.PropositionalLogic,
    trees,
  };
};

[@deriving (show({with_path: false}), sexp, yojson)]
type persistent_exercise_mode = p(PersistentZipper.t);
