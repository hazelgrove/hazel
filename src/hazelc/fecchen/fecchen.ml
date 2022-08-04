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
  open Monad
  open Monad.Syntax

  let store_memo q d =
    update (fun ({ memo; _ } as s) ->
        Memo.store memo q d;
        { s with memo })

  let retrieve_memo q = get >>| fun { memo; _ } -> Memo.retrieve memo q

  type 'd f = {
    fetch : 'd Query.t -> ('d, 'd) Monad.t;
    pure : 'd -> ('d, 'd) Monad.t;
    io : 'd -> ('d, 'd) Monad.t;
  }

  type 'd rules = 'd f -> 'd Query.t -> ('d, 'd) Monad.t

  let rec fetch rules q =
    let pure d = d |> return in
    let io d = d |> return in

    let fetch_ q = rules { fetch = fetch rules; pure; io } q in
    fetch_ q

  let rec fetch_with_memo rules q =
    let pure d =
      let+ () = store_memo q d in
      d
    in

    let io d =
      let+ () = store_memo q d in
      d
    in

    let fetch_with_memo_ q =
      retrieve_memo q >>= function
      | Some d -> d |> return
      | None -> rules { fetch = fetch_with_memo rules; pure; io } q
    in

    fetch_with_memo_ q

  let run rules q =
    let m = fetch rules q in
    run_state m (init ())

  let run_with_memo rules q =
    let m = fetch_with_memo rules q in
    run_state m (init ())
end
