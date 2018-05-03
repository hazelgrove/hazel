open Semantics.Core;


/**
 * Raised by parsing functions if the passed serialization has incorrect syntax
 * (including invalid variable name) that prevents it from being parsed correctly.
 */
exception InvalidSyntax string;


/**
 * Raised by parsing or serialization functions if the passed or resultant UHExp
 * fails to type-check, which may be due to incorrect types but could also be due
 * to other problems that are caught during type-checking such as invalid variable
 * names.
 */
exception IllFormed UHExp.t;

let string_of_side side =>
  switch side {
  | UHExp.L => "L"
  | UHExp.R => "R"
  };

/* NOTE: If you change these, they must be changed in HZLex.mll as well! */
let lamSym = "\206\187";

let caseArrowSym = "\226\135\146";

let typeArrowSym = "\226\134\146";
