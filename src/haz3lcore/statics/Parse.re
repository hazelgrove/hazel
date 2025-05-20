open Util;

let parse_exp = (s: string) => {
  open OptUtil.Syntax;
  let+ zip = Printer.zipper_of_string(s, ~root=Sort.Exp);
  MakeTerm.from_zip_for_sem(zip).term;
};
