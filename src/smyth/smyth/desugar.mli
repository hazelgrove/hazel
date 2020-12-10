(** Helper module for constructing {!Lang.exp} values.

    See {!Sugar} for some inverse operations. *)

open Lang

val lett : typ -> string -> exp -> exp -> exp
(** [lett tau x binding body] constructs the equivalent of the following
    let-binding expression:

    {[
      let (x : tau) = binding in
      body
    ]} *)

val func_params : param list -> exp -> exp
(** [func_params \[x0, ..., xN\] body] constructs the equivalent of the
    following nested lambda expression:

    {[ \x0 -> ... \xN -> body ]} *)

val app : exp -> exp_arg list -> exp
(** [app head \[e0, ..., eN\] body] constructs the equivalent of the
    following application expression:

    {[ head e0 ... eN ]} *)

(* Precondition: input >= 0 *)
val nat : int -> exp
(** [nat n] constructs the expression [S (... S (Z ()) ...)], where the [S]
    constructor is nested [n] times (i.e., [S]{^ [n]}[(Z ())]).

    {b Precondition:} [n >= 0]. *)

val listt : exp list -> typ list -> exp
(** [listt \[e0, ..., eN\] taus] constructs the following polymorphic list
    expression:

    {[ Cons<taus> (e0, ... Cons<taus> (eN, Nil<taus>)) ]} *)

(** Packages up datatypes, function definitions, assertions, and the main
    expression of a program into a single type. *)
type program =
  { datatypes: datatype_ctx
  ; definitions: (string * (typ * exp)) list
  ; assertions: (exp * exp) list
  ; main_opt: exp option }
[@@deriving sexp]

val program : program -> exp * datatype_ctx
(** Desugars a {!program} value into the corresponding {!Lang.exp} value
    (along with its datatype context). *)
