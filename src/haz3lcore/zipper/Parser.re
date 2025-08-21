open Util.OptUtil.Syntax;

let default_projector_init = (_, _) => Some(Piece.mk_grout(Grout.Convex));

let to_zipper =
    (~projector_init, ~zipper_init=Zipper.init(), str: string)
    : option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(~projector_init, c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  let* z =
    str
    |> Util.StringUtil.to_list
    |> List.fold_left(insert, Some(zipper_init));
  /* HACK(andrew): Insert/Destruct below is a hack to deal
     with the fact that pasting something like "let a = b in"
     won't trigger the barfing of the "in"; to trigger this,
     we insert a linebreak, and then we immediately delete it */
  let* z = Insert.go(~projector_init, "\n", z);
  let+ z = Destruct.go(Left, z);
  Zipper.remold_regrout(Left, z);
};

let to_segment = (~projector_init, s: string): option(Segment.t) => {
  let+ z = to_zipper(~projector_init, s);
  Zipper.seg_without_buffer(z);
};

let to_term = (~projector_init, s: string): option(Language.Term.Exp.t) => {
  let+ z = to_zipper(~projector_init, s);
  switch (
    MakeTerm.from_zip_for_sem(
      ~of_projector=
        (~sort as _, ~id as _, _) =>
          failwith("Parser.to_term: Projectors unimplemented"),
      z,
    ).
      term
  ) {
  | Exp(term) => term
  | _ => failwith("Parser.to_term: Not expression")
  };
};
