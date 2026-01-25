open Util.OptUtil.Syntax;

let to_zipper = (~zipper_init=Zipper.init(), str: string): option(Zipper.t) => {
  let insert = (z: option(Zipper.t), c: string): option(Zipper.t) => {
    let* z = z;
    /* Disable auto_indent so Parser faithfully reproduces input without adding spaces */
    try(c == "\r" ? Some(z) : Insert.go(~auto_indent=false, c, z)) {
    | exn =>
      print_endline("WARN: Parser.to_zipper: " ++ Printexc.to_string(exn));
      None;
    };
  };
  let+ z = str |> Token.to_list |> List.fold_left(insert, Some(zipper_init));
  Zipper.remold_regrout(Left, z);
};

let to_segment = (s: string): option(Segment.t) => {
  let+ z = to_zipper(s);
  Zipper.unselect_and_zip(~erase_buffer=true, z);
};

let to_term = (s: string): option(Language.Exp.t) => {
  let+ z = to_zipper(s);
  MakeTerm.from_zip_for_sem(z).term;
};
