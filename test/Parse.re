open Util;

let parse_exp = (s: string) =>
  {
    open OptUtil.Syntax;
    let* zip = Haz3lcore.Parser.to_zipper(s);
    let term = Haz3lcore.MakeTerm.from_zip_for_sem(zip).term;
    let+ exp = Language.Any.is_exp(term);
    exp;
  }
  |> OptUtil.get_or_fail("Failed to parse expression: " ++ s);
