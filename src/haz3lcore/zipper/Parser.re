open Util.OptUtil.Syntax;

let to_zipper =
    (~zipper_init=Zipper.init(), str: string): option(Zipper.t('p)) => {
  let insert = (z: option(Zipper.t('p)), c: string): option(Zipper.t('p)) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  str |> Util.StringUtil.to_list |> List.fold_left(insert, Some(zipper_init));
};

let to_segment = (s: string): option(Segment.t('p)) => {
  let+ z = to_zipper(s);
  Zipper.seg_without_buffer(z);
};

let to_term = (s: string): option(Language.Term.Exp.t) => {
  let+ z = to_zipper(s);
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
