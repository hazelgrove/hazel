open Sexplib.Std;
open Ppx_yojson_conv_lib.Yojson_conv.Primitives;

/* Errors reported by the Blackboard checker.  Data only: rendering belongs
   to whoever shows them (tests, the cursor inspector). */

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type t =
  | Unbound(string)
  /* term used as a type; it has the given type instead of `type` */
  | NotAType(BbTerm.t, BbTerm.t)
  /* head of an application; it has the given non-arrow type */
  | NotAFunction(BbTerm.t, BbTerm.t)
  /* function, argument, expected domain, actual type of the argument */
  | ArgumentMismatch(BbTerm.t, BbTerm.t, BbTerm.t, BbTerm.t);

/* An error attributed to a named signature entry. */
[@deriving (show({with_path: false}), sexp, yojson, eq)]
type located = {
  entry: string,
  err: t,
};

let to_string = (e: t): string => {
  let s = BbTerm.to_string;
  switch (e) {
  | Unbound(x) => "unbound name " ++ x
  | NotAType(t, ty) =>
    s(t) ++ " is used as a type, but it has type " ++ s(ty)
  | NotAFunction(t, ty) =>
    s(t) ++ " is applied, but it has type " ++ s(ty) ++ ", not an arrow"
  | ArgumentMismatch(f, a, expected, actual) =>
    s(f)
    ++ " expects an argument of type "
    ++ s(expected)
    ++ " but "
    ++ s(a)
    ++ " has type "
    ++ s(actual)
  };
};

let located_to_string = ({entry, err}: located): string =>
  entry ++ ": " ++ to_string(err);
