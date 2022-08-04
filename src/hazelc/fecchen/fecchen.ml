module type S = sig
  type 'd query

  module Monad : Monad.S
  module Syntax = Monad.Syntax

  type 'd f = {
    fetch : 'd query -> ('d, 'd) Monad.t;
    pure : 'd -> ('d, 'd) Monad.t;
    io : 'd -> ('d, 'd) Monad.t;
  }

  type 'd rules = 'd f -> 'd query -> ('d, 'd) Monad.t

  val run : 'd rules -> 'd query -> 'd
  val run_with_memo : 'd rules -> 'd query -> 'd
end

module Make (Query : Query.T) : S with type 'd query = 'd Query.t = struct
  type 'd query = 'd Query.t

  module Monad = Monad.Make (Query)
  module Syntax = Monad.Syntax

  let store_memo q d =
    let open Monad in
    update (fun { memo } ->
        Memo.store memo q d;
        { memo })

  let retrieve_memo q =
    let open Monad in
    let open Monad.Syntax in
    get >>| fun { memo } -> Memo.retrieve memo q

  type 'd f = {
    fetch : 'd Query.t -> ('d, 'd) Monad.t;
    pure : 'd -> ('d, 'd) Monad.t;
    io : 'd -> ('d, 'd) Monad.t;
  }

  type 'd rules = 'd f -> 'd Query.t -> ('d, 'd) Monad.t

  let run rules q =
    let open Monad in
    let rec fetch rules q =
      let fetch' q' = fetch rules q' in
      let pure d = d |> return in
      let io d = d |> return in

      (* Invoke rules. *)
      rules { fetch = fetch'; pure; io } q
    in

    let m = fetch rules q in
    run_state m (init ())

  let run_with_memo rules q =
    let open Monad in
    let open Monad.Syntax in
    let rec fetch rules q =
      let fetch' q' =
        retrieve_memo q' >>= function
        | Some d -> d |> return
        | None -> fetch rules q'
      in

      let pure d =
        let+ () = store_memo q d in
        d
      in

      let io d =
        let+ () = store_memo q d in
        d
      in

      (* Invoke rules. *)
      rules { fetch = fetch'; pure; io } q
    in

    let open Monad in
    let m = fetch rules q in
    run_state m (init ())
end
