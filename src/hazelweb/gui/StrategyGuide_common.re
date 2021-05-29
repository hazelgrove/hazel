module Vdom = Virtual_dom.Vdom;
open Vdom;

/**
 * Gets the type of the expression at the cursor.
 * Return HTyp.t
 */
let get_type = (cursor_info: CursorInfo.t) => {
  let rec my_type = (typed: CursorInfo.typed) =>
    switch (typed) {
    | Analyzed(ty) => Some(ty)
    | AnaAnnotatedLambda(expected_ty, _) => Some(expected_ty)
    | AnaSubsumed(expected_ty, _) => Some(expected_ty)
    | Synthesized(ty) => Some(ty)
    | SynMatchingArrow(syn_ty, _) => Some(syn_ty)
    | SynBranchClause(join, typed, _) =>
      switch (join, typed) {
      | (JoinTy(ty), Synthesized(got_ty)) =>
        switch (HTyp.consistent(ty, got_ty), HTyp.eq(ty, got_ty)) {
        | (true, true) => Some(ty)
        | (true, false) => Some(ty)
        | _ => None
        }
      | (NoBranches, _) => my_type(typed)
      | _ => None
      }
    | PatAnalyzed(ty) => Some(ty)
    | PatAnaSubsumed(expected_ty, _) => Some(expected_ty)
    | PatSynthesized(ty) => Some(ty)
    | _ => None
    };
  my_type(cursor_info.typed);
};

let code_node = text =>
  Node.div([Attr.classes(["code-font"])], [Node.text(text)]);

let shortcut_node = text =>
  Node.div([Attr.classes(["code-font", "shortcut"])], [Node.text(text)]);

let example_lit_node = text =>
  Node.div([Attr.classes(["code-font", "example"])], [Node.text(text)]);

let option = nodes => Node.div([Attr.classes(["option"])], nodes);
let mini_option = nodes => Node.div([Attr.classes(["mini-option"])], nodes);
let fill_space = Node.span([Attr.classes(["filler"])], []);

let int_lit =
  option([
    Node.text("Enter an Integer Literal"),
    fill_space,
    Node.text("(e.g. "),
    example_lit_node("1"),
    Node.text(")"),
  ]);
let float_lit =
  option([
    Node.text("Enter a Floating Point Literal"),
    fill_space,
    Node.text("(e.g. "),
    example_lit_node("1.0"),
    Node.text(")"),
  ]);
let bool_lit =
  option([
    Node.text("Enter a Boolean Literal"),
    fill_space,
    Node.text("(e.g. "),
    example_lit_node("true"),
    Node.text(")"),
  ]);

let type_driven = body => Node.div([Attr.classes(["type-driven"])], body);
