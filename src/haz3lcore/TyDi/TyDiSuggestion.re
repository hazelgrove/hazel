open Util;

/* TyDi: Type-Directed Next-Token Suggestions

    IDEA: Expanded criteria for when to autoshow: Currently, we show only
    when there is at least one suggestion which prefix-matches but is not
    identical to the current nonzero prefix. We might consider relaxing
    the nonzero prefix part. We probably don't want to autoshow on correct
    tokens, but we could autoshow on errors if there are fixes, or on
    empties if there's only one option.

     IDEA: Add a keybinding to force reveal suggestion if not current shown.
     I've stubbed this out (Cmd+?) but needs an option to show suggestions
     even if on hole (ie prefix for completion is "")

     IDEA: If there are ~ no current suggestions, and the indicated term
     has a type error suggest following infixes which fix that type error,
     e.g. given "let a:Float = fst(1.0|" suggest comma
     e.g. given "let b:Bool = 1|" suggest <, >, <=, >=, ==, !=, etc.

     IDEA: UNBIDIRECTIONAL POSITIONS:
    1. In ap funpos: favor input ty consistent with arg
    2. In case scrut, favor the tys of extant patterns
    3. In list element, favor the tys of extant elements
    3. In pattern annotation type: favor patann expected type

    IDEA: If on infix op, suggest based on either operand type,
    especially the case where it would fix an operand type error

    IDEA: If on 2-multihole, suggest infix ops as above or Ap if applicable

   */

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_all =
  | FromMissingShards;

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_common =
  | NewForm(Language.Typ.t)
  | FromCtx(Language.Typ.t)
  | FromCtxAp(Language.Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_exp =
  | Common(strategy_common);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_pat =
  | Common(strategy_common)
  | FromCoCtx(Language.Typ.t);

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy_typ =
  | NewForm
  | FromCtx;

[@deriving (show({with_path: false}), sexp, yojson)]
type strategy =
  | Any(strategy_all)
  | Exp(strategy_exp)
  | Pat(strategy_pat)
  | Typ(strategy_typ);

/* A delimiter promised AFTER a lookahead suggestion's head token,
   e.g. the ")" and "::" of "foo( )::". hole_before: a hole sits
   between this delimiter and the previous one (or the head). */
[@deriving (show({with_path: false}), sexp, yojson)]
type tail_delim = {
  text: string,
  hole_before: bool,
};

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  content: string, /* full flat text: head ++ flat_of_tail(tail) */
  strategy,
  /* structured decomposition for lookahead suggestions; [] otherwise */
  tail: list(tail_delim),
};

/* the tail as it appears in the flat content (holes as spaces) */
let flat_of_tail = (tail: list(tail_delim)): string =>
  tail
  |> List.map(d => (d.hole_before ? " " : "") ++ d.text)
  |> String.concat("");

/* the head token: content minus the tail's flat rendering */
let head_of = ({content, tail, _}: t): string =>
  String.sub(
    content,
    0,
    String.length(content) - String.length(flat_of_tail(tail)),
  );

let compare = (s1: t, s2: t): int => {
  String.compare(s1.content, s2.content);
};
