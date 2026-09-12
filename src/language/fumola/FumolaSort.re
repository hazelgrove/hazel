/**
  The sorts of Fumola tiles.

  [Exp] is Fumola's expressions, which is nearly all of it: the one grammar
  covers expressions, declarations and blocks, and the distinctions are made
  by form, as DrvSort and BbSort do for their sub-languages.

  [Name] is the instance name in `fumola <instance> in … end`, and exists so
  that position accepts an identifier and nothing else. It is a sort rather
  than a Pat because it is not a binder: it names a Fumola VM instance, the
  handle the wasm API calls a FumolaInstanceId, and the name has to be text
  the programmer wrote so the instance survives an edit. Spelling it as a Pat
  would reuse more machinery at the cost of reading like a binding.
 */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Exp
  | Name;

let all = [Exp, Name];

let class_of =
  fun
  | Exp => "Fumola"
  | Name => "FumolaName";

let to_string =
  fun
  | Exp => "FumolaExp"
  | Name => "FumolaName";

let to_string_short =
  fun
  | Exp => "Fumola"
  | Name => "Instance";
