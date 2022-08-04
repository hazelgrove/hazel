open Tezt
open Tezt.Base
open Gadt

let register_test title tags f =
  Test.register ~__FILE__ ~title ~tags:([ "gadt"; "hashtbl" ] @ tags) f

let ensure b = if b then () else Test.fail "hashtbl behavior was incrorect"

module K = struct
  type 'a t =
    | A : int -> int t
    | B : float -> float t
    | C : char -> char t
    | D : int -> int t

  let equal : type a b. a t -> b t -> (a t, b t) Gadt.Eq.t option =
   fun x x' ->
    match (x, x') with
    | A n, A n' when Int.equal n n' -> Some Refl
    | B f, B f' when Float.equal f f' -> Some Refl
    | C c, C c' when Char.equal c c' -> Some Refl
    | D n, D n' when Int.equal n n' -> Some Refl
    | _, _ -> None
end

module Tbl = Hashtbl.Make (K)

let () =
  register_test "empty" [] (fun () ->
      let tbl = Tbl.create 10 in

      ensure (Tbl.length tbl == 0);

      ensure (Tbl.find_opt tbl (A 0) = None);
      ensure (Tbl.find_opt tbl (B 0.) = None);
      ensure (Tbl.find_opt tbl (C '0') = None);
      ensure (Tbl.find_opt tbl (D 0) = None);

      unit)

let () =
  register_test "add single" [] (fun () ->
      let tbl = Tbl.create 10 in

      Tbl.add tbl (A 0) 10;
      ensure (Tbl.find_opt tbl (A 0) = Some 10);
      ensure (Tbl.length tbl == 1);

      ensure (Tbl.find_opt tbl (A 1) = None);
      ensure (Tbl.find_opt tbl (B 0.) = None);
      ensure (Tbl.find_opt tbl (C '0') = None);
      ensure (Tbl.find_opt tbl (D 0) = None);

      unit)

let () =
  register_test "add overwrite" [] (fun () ->
      let tbl = Tbl.create 10 in

      Tbl.add tbl (A 0) 10;
      ensure (Tbl.find_opt tbl (A 0) = Some 10);
      ensure (Tbl.length tbl == 1);

      Tbl.add tbl (A 0) 20;
      ensure (Tbl.find_opt tbl (A 0) = Some 20);
      ensure (Tbl.length tbl == 2);

      ensure (Tbl.find_opt tbl (A 1) = None);
      ensure (Tbl.find_opt tbl (B 0.) = None);
      ensure (Tbl.find_opt tbl (C '0') = None);
      ensure (Tbl.find_opt tbl (D 0) = None);

      unit)

let () =
  register_test "replace" [] (fun () ->
      let tbl = Tbl.create 10 in

      Tbl.add tbl (A 0) 10;
      ensure (Tbl.find_opt tbl (A 0) = Some 10);
      ensure (Tbl.length tbl == 1);

      Tbl.replace tbl (A 0) 20;
      ensure (Tbl.find_opt tbl (A 0) = Some 20);
      ensure (Tbl.length tbl == 1);

      ensure (Tbl.find_opt tbl (A 1) = None);
      ensure (Tbl.find_opt tbl (B 0.) = None);
      ensure (Tbl.find_opt tbl (C '0') = None);
      ensure (Tbl.find_opt tbl (D 0) = None);

      unit)
