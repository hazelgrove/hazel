open Util.OptUtil.Syntax;

let to_zipper =
    (~root, ~zipper_init=Zipper.init(~root), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    try(c == "\r" ? Some(z) : Insert.go(c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  let+ z = str |> Token.to_list |> List.fold_left(insert, Some(zipper_init));
  Zipper.remold_regrout(Left, z);
};

let to_segment = (s: string, ~root): option(Segment.t) => {
  let+ z = to_zipper(s, ~root);
  Zipper.unselect_and_zip(~erase_buffer=true, z);
};

let to_term = (s: string, ~root): option(Language.Term.Exp.t) => {
  let+ z = to_zipper(~root, s);
  MakeTerm.from_zip_for_sem(z).term;
};
