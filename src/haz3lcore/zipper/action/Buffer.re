open Util;

let buffer_clear = (z: Zipper.t): Zipper.t =>
  switch (z.selection.mode) {
  | Buffer(Unparsed) => {
      ...z,
      selection: Selection.mk([]),
    }

  | Buffer(Parsed) => z |> Zipper.destroy_selection |> Zipper.regrout(Left)
  | Normal => z
  };

let set_tydi_buffer =
    (info_map: Language.Statics.Map.t, z: Zipper.t): Zipper.t =>
  switch (TyDi.set_buffer(~info_map, z)) {
  | None => z
  | Some(z) => z
  };

let set_llm_buffer = (z: Zipper.t, response: string): Zipper.t =>
  switch (
    {
      open OptUtil.Syntax;
      //TODO: Error feedback on below
      let* rz = Parser.to_zipper(response);
      switch (Zipper.local_backpack(rz)) {
      | [] =>
        Some(Zipper.set_buffer(z, ~content=Zipper.zip(rz), ~mode=Parsed))
      | _ => None
      };
    }
  ) {
  | None => z
  | Some(z) => z
  };

let buffer_accept = (z: Zipper.t): option(Zipper.t) =>
  switch (z.selection.mode) {
  | Normal => None
  | Buffer(Parsed) =>
    let z = Zipper.directional_unselect(Right, z);
    Some(z);
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
        let* z = Parser.to_zipper(~zipper_init=z, completion);
        let* z = Move.to_next_grout(Left, z);
        Move.primary(ByToken, Left, z);
      };
      z;
    | Some(completion) => Parser.to_zipper(~zipper_init=z, completion)
    }
  };

let go =
    (~info_map: Language.Statics.Map.t, a: Action.buffer, z: Zipper.t)
    : Result.t(Zipper.t, Action.Failure.t) =>
  switch (a) {
  | Set(TyDi) => Ok(set_tydi_buffer(info_map, z))
  | Set(LLM(response)) => Ok(set_llm_buffer(z, response))
  | Accept =>
    switch (buffer_accept(z)) {
    | None => Error(CantAccept)
    | Some(z) => Ok(z)
    }
  | Clear => Ok(buffer_clear(z))
  };
