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

type thing_desired =
  | New(Util.Direction.t)
  | Either(Util.Direction.t);

let desired_to_string = (d: thing_desired): string =>
  switch (d) {
  | New(Left) => "<NEW.."
  | New(Right) => ">NEW.."
  | Either(Left) => "<NEW.. or ..COMPLETE<"
  | Either(Right) => ">NEW.. or ..COMPLETE>"
  };

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
let rightmost_token = (t: Tile.t): string =>
  List.nth(t.label, List.hd(List.rev(t.shards)));

let lsp_token_to_left =
    (z: Zipper.t): option((Id.t, option(string), thing_desired)) =>
  /*
   Returning Left means we're looking for either a new thing
    that starts with a left-facing chevron, or a completion of the
    thing to the left, which has a left-side right-facing chevron.
    */
  switch (
    z.caret,
    z.relatives.siblings |> fst |> List.rev,
    z.relatives.siblings |> snd,
  ) {
  | (Inner(_), before, after) =>
    print_endline(
      "Segment before: "
      ++ Segment.show(before)
      ++ "Segment after: "
      ++ Segment.show(after),
    );
    failwith("LSP: EXN: Position is inner");
  | (Outer, [], []) => failwith("LSP: EXN: Empty segment")
  | (Outer, _, [Grout({id, shape: Convex, _})]) =>
    /* TODO: also want to get possible completions of the thing to the left
       (operator or prefix). But we want to retain this case, because for
       new things we want the id for the hole for the ci, but for completions
       we want the id for the thing being completed. So we need to return
       multiple things here, and also in the monotile case */
    print_endline("LSP: Rightwards piece is Convex Grout");
    Some((id, None, Either(Left)));
  | (Outer, _, [_, ..._] as rhs) =>
    print_endline("LSP: Rightwards segment is: " ++ Segment.show(rhs));
    failwith("LSP: EXN: Nonempty Rightwards segment not single Convex Grout");
  | (Outer, [lht, ..._] as seg, []) =>
    if (Piece.is_secondary(lht)) {
      print_endline("LSP: Leftward piece is Secondary; trimming");
    };
    let maybe_complete = !Piece.is_secondary(lht);
    switch (Segment.trim_secondary(Left, seg)) {
    | [] =>
      failwith("LSP: EXN: Rightwards segment after trimming secondaries")
    | [lht, ..._] =>
      switch (lht) {
      | Tile({label: [], _}) => failwith("LSP: EXN: Tile with empty label")
      | Tile({label: [tok_to_left], id, _} as t) =>
        print_endline("LSP: Leftward piece is Monotile: " ++ tile_str(t));
        let d = right_nib_dir(t) |> Util.Direction.toggle;
        let desired = maybe_complete ? Either(d) : New(d);
        Some((id, Some(tok_to_left), desired));
      | Tile(t) =>
        print_endline(
          "LSP: Leftward piece is "
          ++ (Tile.is_complete(t) ? "Complete" : "Incomplete")
          ++ " Polytile: "
          ++ tile_str(t),
        );
        let d = right_nib_dir(t) |> Util.Direction.toggle;
        let desired = New(d);
        Some((t.id, Some(rightmost_token(t)), desired));
      | Grout({id: _, shape, _}) =>
        failwith("LSP: EXN: Leftward Grout " ++ Grout.show_shape(shape))
      | Secondary(_) =>
        failwith("LSP: EXN: Secondary after trimming secondaries")
      }
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

let lsp = (s: string): option(list(string)) => {
  let settings = CoreSettings.on;
  let ctx_init = []; //Builtins.ctx_init;
  let* z = Printer.zipper_of_string(s);
  print_endline("LSP: String parsed successfully");
  let* (id, tok_to_left, desire) = lsp_token_to_left(z);
  switch (tok_to_left) {
  | None => print_endline("LSP: Since rightwards Convex Grout")
  | Some(tok) => print_endline("LSP: Token to left is: " ++ tok)
  };
  print_endline("LSP: Desired: " ++ desired_to_string(desire));
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
  let backpack_suggestion = suggest_backpack(z);
  print_endline(
    "LSP: Pre: Backpack stack: " ++ String.concat(" ", backpack_tokens),
  );
  print_endline(
    "LSP: Pre: Backpack suggestions: " ++ of_sugs(backpack_suggestion),
  );
  let operand_suggestions = AssistantForms.suggest_operand(ci);
  let leading_suggestions = AssistantForms.suggest_leading(ci);
  let variable_suggestions = AssistantCtx.suggest_variable(ci);
  let lookahead_variable_suggestions =
    AssistantCtx.suggest_lookahead_variable(ci);
  print_endline(
    "LSP: Pre: Operand suggestions: " ++ of_sugs(operand_suggestions),
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
  let operator_suggestions = AssistantForms.suggest_operator(ci);
  print_endline(
    "LSP: Pre: Operator suggestion: " ++ of_sugs(operator_suggestions),
  );
  let suggestions_pre =
    operand_suggestions
    @ leading_suggestions
    @ variable_suggestions
    @ lookahead_variable_suggestions
    |> List.sort(Suggestion.compare);
  let finish_current_token_suggestions =
    switch (tok_to_left) {
    | None => []
    | Some(tok_to_left) =>
      suggestions_pre
      |> List.filter(({content, _}: Suggestion.t) =>
           String.starts_with(~prefix=tok_to_left, content)
         )
    };
  //TODO: only suggest backpack, operator if prev token is complete (non-errored)?
  let suggestions =
    (
      backpack_suggestion @ List.sort(Suggestion.compare, operator_suggestions)
    )
    @ finish_current_token_suggestions;
  print_endline(
    "LSP: Suggested tokens: "
    ++ (suggestions |> List.map(Suggestion.content_of) |> String.concat(" ")),
  );
  Some(suggestions |> List.map(Suggestion.content_of));
};

/**
TODO: also suggest start parens, square brackets, etc.
done, hacky (see assistant forms hacks)

TODO: figure out current expected shape by figuring out the
shape of whats to the left, and filter accordingly
eg only want to suggest operators if left is an operand
need to see if we already have a defined notion of shape for delimiters

 */
lsp(arg_str);
