open Haz3lcore;

let parse_program = (s: string) =>
  switch (
    {
      open Util.OptUtil.Syntax;
      let+ zip = Printer.zipper_of_string(s);
      MakeTerm.from_zip_for_sem(zip).term;
    }
  ) {
  | Some(Exp(e)) => e
  | Some(_)
  | None => failwith("Failed to parse expression: " ++ s)
  };
