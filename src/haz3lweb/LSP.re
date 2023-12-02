open Js_of_ocaml;
open Util.OptUtil.Syntax;
open Haz3lcore;
open Haz3lcore.TyDi;

Js.Unsafe.js_expr("require('process')");
let arg_str = Js.Unsafe.js_expr("process.argv[2]") |> Js.to_string;
print_endline("LSP: recieved string: " ++ arg_str);

/*TODO: refactor
  type thing_desired =
    (option((tok_cls, cur_string, existing_token_id))    //completion
      ,
      option(hole_id))   //new (filling rightwards hole)
    )
    //actually also need to factor in new infix ops (which id to use...)
  */

//TODO(andrew): this logic is complicated and makes assumptions
/* assume for now left-to-right entry, so the present shards are
   a prefix of the complete tile. this means that regardless of
   completeness, we can just use the left nibs */

let left_nib_dir = (t: Tile.t): Util.Direction.t =>
  fst(t.mold.nibs).shape == Convex ? Left : Right;
let right_nib_dir = (t: Tile.t): Util.Direction.t =>
  Tile.is_complete(t)
    ? snd(t.mold.nibs).shape == Convex ? Right : Left : Left;

let right_nib_str = (t: Tile.t): string =>
  right_nib_dir(t) == Left ? "<" : ">";

let left_nib_str = (t: Tile.t): string =>
  left_nib_dir(t) == Left ? "<" : ">";

let tile_str = (t: Tile.t): string => {
  let label_str = t.label |> String.concat(" ");
  left_nib_str(t) ++ label_str ++ right_nib_str(t);
};

//This makes similar assumptions to the above
let _rightmost_token = (t: Tile.t): string =>
  List.nth(t.label, List.hd(List.rev(t.shards)));

let hole_to_right = (z: Zipper.t): option(Id.t) =>
  /* If we're doing pure left to right entry, there should be nothing to
     the right except for maybe a convex hole inserted by the grouter.
     If there is a such a hole, its CI should be used to inform new
     token insertions (but NOT completions of the token to the left) */
  switch (z.relatives.siblings |> snd) {
  | [] =>
    print_endline("LSP: No rightwards piece");
    None;
  | [Grout({id, shape: Convex, _})] =>
    print_endline("LSP: Rightwards piece is Convex Grout");
    Some(id);
  | [_, ..._] as rhs =>
    print_endline("LSP: Rightwards segment is: " ++ Segment.show(rhs));
    failwith("LSP: EXN: Nonempty Rightwards segment not single Convex Grout");
  };

type lalala =
  | Monotile(Id.t, string)
  | Polytile(Id.t);

type substantial_thing_to_left =
  | LeftConvex(lalala)
  | LeftConcave(lalala);

type thing_to_left =
  | Nothing
  | Just(substantial_thing_to_left)
  | SpacesThen(substantial_thing_to_left);

let thing_to_left = (z: Zipper.t): thing_to_left =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (z.relatives.siblings |> fst |> List.rev) {
  | [] => Nothing
  | [lht, ..._] as seg =>
    if (Piece.is_secondary(lht)) {
      print_endline("LSP: Leftward piece is Secondary; trimming");
    };
    switch (Segment.trim_secondary(Left, seg)) {
    | [] =>
      failwith("LSP: EXN: Rightwards segment after trimming secondaries")
    | [lht, ..._] =>
      switch (lht) {
      | Tile({label: [], _}) => failwith("LSP: EXN: Tile with empty label")
      | Tile({label: [tok_to_left], id, _} as t) =>
        print_endline("LSP: Leftward piece is Monotile: " ++ tile_str(t));
        let g =
          switch (right_nib_dir(t)) {
          | Right => LeftConvex(Monotile(id, tok_to_left))
          | Left => LeftConcave(Monotile(id, tok_to_left))
          };
        Piece.is_secondary(lht) ? SpacesThen(g) : Just(g);
      | Tile(t) =>
        print_endline(
          "LSP: Leftward piece is "
          ++ (Tile.is_complete(t) ? "Complete" : "Incomplete")
          ++ " Polytile: "
          ++ tile_str(t),
        );
        let g =
          switch (right_nib_dir(t)) {
          | Right => LeftConvex(Polytile(t.id))
          | Left => LeftConcave(Polytile(t.id))
          };
        Piece.is_secondary(lht) ? SpacesThen(g) : Just(g);
      | Grout({id: _, shape, _}) =>
        failwith("LSP: EXN: Leftward Grout " ++ Grout.show_shape(shape))
      | Secondary(_) =>
        failwith("LSP: EXN: Secondary after trimming secondaries")
      }
    };
  };

type generation_options =
  | NewRightConvexToken(Id.t)
  | NewRightConcaveToken(Id.t) //id here is iffy
  | LeftConcaveCompletionOrNewRightConvex(Id.t, string, Id.t)
  | LeftConvexCompletionOrNewRightConcave(Id.t, string, Id.t); //snd id here is iffy

let generation_options = (z: Zipper.t) => {
  switch (thing_to_left(z), hole_to_right(z)) {
  | (Nothing, None) => failwith("LSP: EXN: Nothing to left or right")
  | (Just(LeftConcave(_)) | SpacesThen(LeftConcave(_)), None) =>
    failwith("LSP: EXN: Concave to left and nothing to right")
  | (Just(LeftConvex(_)) | SpacesThen(LeftConvex(_)), Some(_)) =>
    failwith("LSP: EXN: Convex to left and right")
  | (Nothing | Just(LeftConcave(Polytile(_))) | SpacesThen(_), Some(id)) =>
    NewRightConvexToken(id)
  | (Just(LeftConcave(Monotile(id, left_token))), Some(id_to_right)) =>
    LeftConcaveCompletionOrNewRightConvex(id, left_token, id_to_right)
  | (
      Just(LeftConvex(Polytile(id))) |
      SpacesThen(LeftConvex(Polytile(id) | Monotile(id, _))),
      None,
    ) =>
    //TODO: id here is weird
    NewRightConcaveToken(id)
  | (Just(LeftConvex(Monotile(id, left_token))), None) =>
    //TODO: id here is weird
    LeftConvexCompletionOrNewRightConcave(id, left_token, id)
  };
};

let lsp_z_to_ci =
    (~settings: CoreSettings.t, ~ctx: Ctx.t, id: Id.t, z: Zipper.t) => {
  let map =
    z
    |> MakeTerm.from_zip_for_sem
    |> fst
    |> Interface.Statics.mk_map_ctx(settings, ctx);
  Id.Map.find_opt(id, map);
};

let of_sugs: list(Suggestion.t) => string =
  s => s |> List.map((s: Suggestion.t) => s.content) |> String.concat(" ");

let error_str = ci =>
  switch (Info.error_of(ci)) {
  | Some(err) => Info.show_error(err)
  | None => "None"
  };

let nu_sugs =
    (~shape: Nib.Shape.t, ~completion: option(string), z: Zipper.t, id: Id.t)
    : option(list(string)) => {
  let settings = CoreSettings.on;
  let ctx_init = []; //Builtins.ctx_init;
  let* ci = lsp_z_to_ci(~settings, ~ctx=ctx_init, id, z);
  let sort = Info.sort_of(ci);
  let cls = Info.cls_of(ci);
  let ctx = Info.ctx_of(ci);
  let expected_ty = AssistantForms.Typ.expected(ci);
  print_endline("LSP: Cursor info from form: " ++ Term.Cls.show(cls));
  print_endline("LSP: Having sort: " ++ Sort.to_string(sort));
  print_endline("LSP: Having ctx: " ++ Ctx.to_string(ctx));
  print_endline("LSP: Having expected type: " ++ Typ.to_string(expected_ty));
  print_endline("LSP: Having error: " ++ error_str(ci));
  let backpack_tokens = TyDi.backpack_to_token_list(z.backpack);
  // TODO: filter backpack suggestion by shape
  let backpack_suggestion = suggest_backpack(z);
  print_endline(
    "LSP: Pre: Backpack stack: " ++ String.concat(" ", backpack_tokens),
  );
  print_endline(
    "LSP: Pre: Backpack suggestions: " ++ of_sugs(backpack_suggestion),
  );
  let base_mono_operand_suggestions = AssistantForms.suggest_operand(ci);
  let abstract_mono_operand_suggestions =
    AssistantForms.suggest_abstract_mono(ci);
  let leading_suggestions = AssistantForms.suggest_leading(ci);
  let variable_suggestions = AssistantCtx.suggest_variable(ci);
  let lookahead_variable_suggestions =
    AssistantCtx.suggest_lookahead_variable(ci);
  print_endline(
    "LSP: Pre: Base mono operand suggestions: "
    ++ of_sugs(base_mono_operand_suggestions),
  );
  print_endline(
    "LSP: Pre: Base mono operand suggestions: "
    ++ of_sugs(base_mono_operand_suggestions),
  );
  print_endline(
    "LSP: Pre: Leading suggestions: " ++ of_sugs(leading_suggestions),
  );
  print_endline(
    "LSP: Pre: Variable suggestions: " ++ of_sugs(variable_suggestions),
  );
  print_endline(
    "LSP: Pre: Lookahead variable suggestion: "
    ++ of_sugs(lookahead_variable_suggestions),
  );
  let suggestions_concave = AssistantForms.suggest_operator(ci);
  print_endline(
    "LSP: Pre: Operator suggestion: " ++ of_sugs(suggestions_concave),
  );
  let suggestions_convex =
    base_mono_operand_suggestions
    @ abstract_mono_operand_suggestions
    @ leading_suggestions
    @ variable_suggestions
    @ lookahead_variable_suggestions
    |> List.sort(Suggestion.compare);

  let base_suggestions =
    switch (shape) {
    | Convex =>
      suggestions_convex
      @ List.filter_map(
          ({strategy, _} as s: Suggestion.t) =>
            switch (strategy) {
            | Any(FromBackpack(shape)) when shape == Convex => Some(s)
            | _ => None
            },
          backpack_suggestion,
        )

    | Concave(_) =>
      suggestions_concave
      @ List.filter_map(
          ({strategy, _} as s: Suggestion.t) =>
            switch (strategy) {
            | Any(FromBackpack(shape)) when shape != Convex => Some(s)
            | _ => None
            },
          backpack_suggestion,
        )
    };
  switch (completion) {
  | None => Some(base_suggestions |> List.map(Suggestion.content_of))
  | Some(tok_to_left) =>
    suggestions_concave
    @ suggestions_convex
    @ backpack_suggestion
    |> List.filter(({content, _}: Suggestion.t) =>
         String.starts_with(~prefix=tok_to_left, content)
       )
    |> List.map(Suggestion.content_of)
    |> List.filter_map(x => TyDi.suffix_of(x, tok_to_left))
    |> Option.some
  };
  //TODO: only suggest backpack, operator if prev token is complete (non-errored)?
  /*let suggestions =
      (backpack_suggestion @ List.sort(Suggestion.compare, suggestions_concave))
      @ finish_current_token_suggestions;
    print_endline(
      "LSP: Suggested tokens: "
      ++ (suggestions |> List.map(Suggestion.content_of) |> String.concat(" ")),
    );
    Some(suggestions |> List.map(Suggestion.content_of));*/
};

let dispatch_generation = (s: string) => {
  let* z = Printer.zipper_of_string(s);
  print_endline("LSP: String parsed successfully");
  let seg_before = z.relatives.siblings |> fst |> List.rev;
  let seg_after = z.relatives.siblings |> snd;
  if (z.caret != Outer) {
    print_endline(
      "Segment before: "
      ++ Segment.show(seg_before)
      ++ "Segment after: "
      ++ Segment.show(seg_after),
    );
    failwith("LSP: EXN: Caret position is inner");
  };
  if (seg_before == [] && seg_after == []) {
    failwith("LSP: EXN: Empty segment");
  };
  switch (generation_options(z)) {
  | NewRightConvexToken(id) => nu_sugs(~shape=Convex, ~completion=None, z, id)
  | NewRightConcaveToken(id) =>
    nu_sugs(~shape=Concave(0), ~completion=None, z, id)
  | LeftConcaveCompletionOrNewRightConvex(id_left, string, id_right) =>
    let* a = nu_sugs(~shape=Convex, ~completion=None, z, id_right);
    let+ b =
      nu_sugs(~shape=Concave(0), ~completion=Some(string), z, id_left);
    a @ b;
  | LeftConvexCompletionOrNewRightConcave(id_left, string, id_right) =>
    let* a = nu_sugs(~shape=Concave(0), ~completion=None, z, id_right);
    let+ b = nu_sugs(~shape=Convex, ~completion=Some(string), z, id_left);
    a @ b;
  };
};

let toks_to_grammar = (toks: list(string)): string => {
  //TODO: just let copilot generate these prefix patterns lol, better checkem
  let _prefix = {|
intlit ::= [0-9]+
floatlit ::= [0-9]+ "." [0-9]+
stringlit ::= "\"" [^"]* "\""
patvar ::= [a-zA-Z_][a-zA-Z0-9_]*
typvar ::= [A-Z][a-zA-Z0-9_]*
whitespace ::= [ \t\n]+

root  ::=
    whitespace
  | |};
  //TODO: make whitespace conditional on something
  let toks =
    List.map(
      tok =>
        switch (tok) {
        | "<INTLIT>" => "intlit"
        | "<FLOATLIT>" => "floatlit"
        | "<STRINGLIT>" => "stringlit"
        | "<PATVAR>" => "patvar"
        | "<TYPVAR>" => "typvar"
        | tok => "\"" ++ tok ++ "\""
        },
      toks,
    );
  let toks = String.concat("\n  | ", toks);
  _prefix ++ toks;
};

let finals = (s: string) => {
  switch (dispatch_generation(s)) {
  | None => print_endline("LSP: No suggestions")
  | Some(sugs) =>
    print_endline("LSP: Suggestions: " ++ String.concat(" ", sugs));
    prerr_endline("LSP: Grammar:\n " ++ toks_to_grammar(sugs));
  };
};
finals(arg_str);
/**
TODO: also suggest start parens, square brackets, etc.
done, hacky (see assistant forms hacks)

TODO: figure out current expected shape by figuring out the
shape of whats to the left, and filter accordingly
eg only want to suggest operators if left is an operand
need to see if we already have a defined notion of shape for delimiters

 */;
//lsp(arg_str);
