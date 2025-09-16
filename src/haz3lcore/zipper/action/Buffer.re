open Util;
open OptUtil.Syntax;

let buffer_clear = (z: Zipper.t): Zipper.t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => Zipper.clear_unparsed_buffer(z)
  | Buffer(Parsed) => z |> Zipper.destroy_selection |> Zipper.regrout(Left)
  | Normal => z
  };

let set_tydi_buffer = (ci: option(Language.Info.t), z: Zipper.t): Zipper.t =>
  switch (TyDi.set_buffer(~ci, z)) {
  | None => z
  | Some(z) => z
  };

let set_llm_buffer = (z: Zipper.t, response: string): Zipper.t =>
  switch (
    {
      //TODO: Check for incomplete syntax, report errors
      let+ res = Parser.to_zipper(response, ~root=Exp);
      Zipper.zip(res);
    }
  ) {
  | None => z
  | Some(content) => Zipper.set_buffer(z, ~content, ~mode=Parsed)
  };

let buffer_accept = (z: Zipper.t): option(Zipper.t) =>
  switch (z.selection.mode) {
  | Normal => None
  | Buffer(Parsed) => Some(Zipper.directional_unselect(Right, z))
  | Buffer(Unparsed) =>
    switch (TyDi.get_unparsed_buffer(z)) {
    | None => None
    | Some(completion)
        when Token.match(Token.regexp(".*\\)::$"), completion) =>
      /* Slightly hacky. There's currently only one genre of completion
       * that creates more than one hole on intial expansion: when on eg
       * 1 :: a|, we suggest "abs( )::" via lookahead. In such a case we
       * want the caret to end up to the left of the first hole, whereas
       * pasting would leave it to the left of the second. Thus we move
       * left to the previous hole. */
      let z = {
        open OptUtil.Syntax;
        let* z = Parser.to_zipper(~root=Exp, ~zipper_init=z, completion);
        let* z = Move.to_next_grout(Left, z);
        Move.local(ByToken, Left, z);
      };
      z;
    | Some(completion) =>
      Parser.to_zipper(~root=Exp, ~zipper_init=z, completion)
    }
  };

let go =
    (~ci: option(Language.Info.t), a: Action.buffer, z: Zipper.t)
    : Result.t(Zipper.t, Action.Failure.t) =>
  switch (a) {
  | Set(TyDi) => Ok(set_tydi_buffer(ci, z))
  | Set(LLM(response)) => Ok(set_llm_buffer(z, response))
  | Accept =>
    buffer_accept(z) |> Result.of_option(~error=Action.Failure.CantAccept)
  | Clear => Ok(buffer_clear(z))
  };
