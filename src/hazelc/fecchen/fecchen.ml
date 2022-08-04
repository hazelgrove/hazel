module type S = sig
  type 'd query

  module Monad : Monad.S
  module Syntax = Monad.Syntax

  type 'd data
  type 'd fetch = 'd query -> ('d, 'd data) Monad.t
  type 'd pure = 'd -> ('d, 'd data) Monad.t
  type 'd io = 'd -> ('d, 'd data) Monad.t
  type 'd f = 'd fetch * 'd pure * 'd io
  type 'd rules = 'd f -> 'd query -> ('d, 'd data) Monad.t

  val run : 'd rules -> 'd query -> 'd
  val run_with_memo : 'd rules -> 'd query -> 'd
end

module Make (Query : Query.T) : S with type 'd query = 'd Query.t = struct
  type 'd query = 'd Query.t

  module Monad = Monad.Make (Query)
  module Syntax = Monad.Syntax
  open Monad
  open Monad.Syntax

  module H = struct
    let add_dep : 'd0 query -> 'd1 query -> ('d, unit) t =
     fun q q' ->
      update (fun ({ depgraph; _ } as s) ->
          Depgraph.add depgraph q q';
          { s with depgraph })

    let get_deps : 'd query -> ('d, Depgraph.ex_query list) t =
     fun q -> get >>| fun { depgraph; _ } -> Depgraph.deps depgraph q

    let store_memo : 'd query -> 'd -> Memo.kind -> ('d, unit) t =
     fun q d kind ->
      update (fun ({ memo; _ } as s) ->
          Memo.store memo q d kind;
          { s with memo })

    let retrieve_memo : 'd query -> ('d, ('d * Memo.kind) option) t =
     fun q -> get >>| fun { memo; _ } -> Memo.retrieve memo q
  end

  type 'd data = Computed of 'd | Memoized of 'd
  type 'd fetch = 'd query -> ('d, 'd data) Monad.t
  type 'd pure = 'd -> ('d, 'd data) Monad.t
  type 'd io = 'd -> ('d, 'd data) Monad.t
  type 'd f = 'd fetch * 'd pure * 'd io
  type 'd rules = 'd f -> 'd query -> ('d, 'd data) Monad.t

  let rec fetch : type d. d rules -> d fetch =
   fun rules q ->
    let pure : d pure = fun d -> Computed d |> return in
    let io : d io = fun d -> Computed d |> return in

    rules (fetch rules, pure, io) q

  let rec fetch_with_memo : type d. d rules -> d fetch =
   fun rules q ->
    let fetch_ : d fetch =
     fun q' ->
      (* Add dependency relation between the two queries. *)
      let* () = H.add_dep q q' in

      (* Fetch dependency. *)
      fetch_with_memo rules q'
    in

    (* Return a pure result that was just computed. *)
    let pure : d pure =
     fun d ->
      (* Memoize the result. *)
      let+ () = H.store_memo q d Pure in
      Computed d
    in

    (* Return an impure result that was just (possibly re-) computed. *)
    let io : d io =
     fun d ->
      let* old = H.retrieve_memo q in
      match old with
      (* If new result is the same as the memoized one, return as memoized. *)
      | Some (old, _) when old == d -> Memoized d |> return
      (* Otherwise, memoize and return as computed. *)
      | Some _ | None ->
          let+ () = H.store_memo q d Io in
          Computed d
    in

    let rules_ = rules (fetch_, pure, io) in
    H.retrieve_memo q >>= function
    | Some (d, Pure) ->
        (* If all dependencies returned as memoized, return memoized value. *)
        let* deps = H.get_deps q in
        let* reuse =
          deps
          |> List.fold_left
               (fun acc (Depgraph.Q q) ->
                 acc >>= function
                 | false -> false |> return
                 | true -> (
                     rules_ q >>| function
                     | Memoized _ -> true
                     | Computed _ -> false))
               (true |> return)
        in
        if reuse then Memoized d |> return else rules_ q
    | Some (_, Io) | None -> rules_ q

  let run_ m = run_state m (init ()) |> fun (Computed d | Memoized d) -> d
  let run rules q = q |> fetch rules |> run_
  let run_with_memo rules q = q |> fetch_with_memo rules |> run_
end
