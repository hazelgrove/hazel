open Util;

let parse_exp = (~of_projector, s: string) => {
  open OptUtil.Syntax;
  let* zip = Printer.zipper_of_string(s);
  let any = MakeTerm.from_zip_for_sem(~of_projector, zip).term;
  let+ exp = Any.is_exp(any);
  exp;
};
