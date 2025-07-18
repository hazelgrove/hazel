open Util.OptUtil.Syntax;

let to_zipper = (~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  str |> Util.StringUtil.to_list |> List.fold_left(insert, Some(zipper_init));
};

let to_segment = (s: string): option(Segment.t) => {
  let+ z = to_zipper(s);
  Zipper.seg_without_buffer(z);
};

let to_term = (s: string): option(Language.Term.Exp.t) => {
  let+ z = to_zipper(s);
  MakeTerm.from_zip_for_sem(z).term;
};
