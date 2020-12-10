open Sexplib.Std
open Lang

let annotate_rec_name : string -> exp -> exp =
 fun rec_name exp ->
  match exp with
  | EFix (_, param, body) -> EFix (Some rec_name, param, body)
  | _ -> exp

let lett : typ -> string -> exp -> exp -> exp =
 fun the_typ name binding body ->
  EApp
    ( false
    , EFix (None, PatParam (PVar name), body)
    , EAExp (ETypeAnnotation (annotate_rec_name name binding, the_typ)) )

let func_params : param list -> exp -> exp =
  List.fold_right (fun param body -> EFix (None, param, body))

let app : exp -> exp_arg list -> exp =
  List.fold_left (fun acc arg -> EApp (false, acc, arg))

(* Precondition: input >= 0 *)
let nat : int -> exp =
  let rec helper acc n =
    if n = 0 then acc else helper (ECtor ("S", [], acc)) (n - 1)
  in
  helper (ECtor ("Z", [], ETuple []))

let listt : exp list -> typ list -> exp =
 fun es ts ->
  List.fold_right
    (fun e acc -> ECtor ("Cons", ts, ETuple [e; acc]))
    es
    (ECtor ("Nil", ts, ETuple []))

type program =
  { datatypes: datatype_ctx
  ; definitions: (string * (typ * exp)) list
  ; assertions: (exp * exp) list
  ; main_opt: exp option }
[@@deriving sexp]

let program : program -> exp * datatype_ctx =
 fun {datatypes; definitions; assertions; main_opt} ->
  ( Post_parse.exp
      (List.fold_right
         (fun (name, (the_typ, the_exp)) -> lett the_typ name the_exp)
         definitions
         (List.fold_right
            (fun (e1, e2) -> lett (TTuple []) "_" (EAssert (e1, e2)))
            assertions
            (Option2.with_default (ETuple []) main_opt)))
  , List.rev datatypes )
