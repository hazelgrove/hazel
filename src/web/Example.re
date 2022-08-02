open Core;
open Model;

let id_gen: ref(int) = ref(0);

let mk_id = (): int => {
  let uid = id_gen^;
  id_gen := id_gen^ + 1;
  uid;
};

let mk_tile: (Form.t, list(list(Piece.t))) => Piece.t =
  //TODO: asserts
  (form, children) =>
    Tile({
      id: mk_id(),
      label: form.label,
      mold: form.mold,
      shards: List.mapi((i, _) => i, form.label),
      children,
    });

let mk_ancestor: (Form.t, (list(Segment.t), list(Segment.t))) => Ancestor.t =
  //TODO: asserts
  (form, (l, _) as children) => {
    id: mk_id(),
    label: form.label,
    mold: form.mold,
    shards:
      List.mapi((i, _) => i, form.label)
      |> Util.ListUtil.split_n(List.length(l) + 1),
    children,
  };

let mk_monotile = form => mk_tile(form, []); //TODO: asserts
let int = n => mk_monotile(Form.mk_convex_mono(Exp, n));
let exp = v => mk_monotile(Form.mk_convex_mono(Exp, v));
let pat = v => mk_monotile(Form.mk_convex_mono(Pat, v));
let mk_parens_exp = mk_tile(Form.get("parens_exp"));
let mk_fun = mk_tile(Form.get("fun_"));
let mk_fun_ancestor = mk_ancestor(Form.get("fun_"));
let mk_parens_ancestor = mk_ancestor(Form.get("parens_exp"));
let mk_let_ancestor = mk_ancestor(Form.get("let_"));
let plus = mk_monotile(Form.get("plus"));

let l_sibling: Segment.t = [plus, Grout({id: mk_id(), shape: Convex})];
let r_sibling: Segment.t = [mk_parens_exp([[int("1"), plus, int("2")]])];

let content: Segment.t = [exp("foo"), Grout({id: mk_id(), shape: Concave})];

let ancestors: Ancestors.t = [
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("bar")]])], [])),
  (mk_parens_ancestor(([], [])), ([mk_fun([[pat("taz")]])], [])),
  (mk_let_ancestor(([[pat("foo")]], [])), ([], [int("2")])),
];

let backpack: Backpack.t = [{focus: Left, content: [exp("foo")]}];

let zipper: Zipper.t = {
  selection: {
    focus: Left,
    content,
  },
  backpack,
  relatives: {
    siblings: (l_sibling, r_sibling),
    ancestors,
  },
  caret: Outer,
  caret_col_target: 0,
};

let init = Model.mk(Simple(zipper));
