open Util;

let parse_exp = (s: string) => {
  open OptUtil.Syntax;
  let+ zip = Haz3lcore.Printer.zipper_of_string(s, ~root=Exp);
  Haz3lcore.MakeTerm.from_zip_for_sem(zip).term;
};
