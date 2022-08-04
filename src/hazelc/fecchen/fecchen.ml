module type Q = sig
  type 'd t

  val equal : 'd0 t -> 'd1 t -> ('d0 t, 'd1 t) Gadt.Eq.t option
end

module type S = sig
  module Query : sig
    include Q

    type kind = Pure | Io [@@deriving eq, ord]
  end

  module Monad : sig
    type ('d, 'a) t

    val return : 'a -> ('d, 'a) t
    val bind : ('d, 'a) t -> ('a -> ('d, 'b) t) -> ('d, 'b) t
    val map : ('d, 'a) t -> ('a -> 'b) -> ('d, 'b) t

    module Syntax : sig
      val ( let* ) : ('d, 'a) t -> ('a -> ('d, 'b) t) -> ('d, 'b) t
      val ( let+ ) : ('d, 'a) t -> ('a -> 'b) -> ('d, 'b) t
      val ( >>= ) : ('d, 'a) t -> ('a -> ('d, 'b) t) -> ('d, 'b) t
      val ( >>| ) : ('d, 'a) t -> ('a -> 'b) -> ('d, 'b) t
    end
  end

  module Syntax = Monad.Syntax

  type 'd f = {
    fetch : 'd Query.t -> ('d, 'd) Monad.t;
    pure : 'd -> ('d, 'd) Monad.t;
    io : 'd -> ('d, 'd) Monad.t;
  }

  type 'd rules = 'd f -> 'd Query.t -> ('d, 'd) Monad.t

  val run : 'd rules -> 'd Query.t -> 'd
  val run_with_memo : 'd rules -> 'd Query.t -> 'd
end

module Make (Q : Q) : S with type 'd Query.t = 'd Q.t = struct
  module Query = struct
    type 'd t = 'd Q.t
    type kind = Pure | Io [@@deriving eq, ord]

    let equal = Q.equal
  end

  module Memo = Memo.Make (Query)

  module State = struct
    type 'd t = { memo : 'd Memo.t }

    let init () = { memo = Memo.create 50 }
  end

  module Monad = struct
    module Monad_ = struct
      type ('d, 'a) t = 'd State.t -> 'd State.t * 'a

      let return x s = (s, x)

      let bind xf f s =
        let s', x = xf s in
        f x s'

      let map x f = bind x (fun a -> return (f a))

      module Syntax = struct
        let ( let* ) = bind
        let ( let+ ) = map
        let ( >>= ) = bind
        let ( >>| ) = map
      end

      let get : ('d, 'd State.t) t = fun s -> (s, s)
      let put : 'd State.t -> ('d, unit) t = fun x _ -> (x, ())

      let update : ('d State.t -> 'd State.t) -> ('d, unit) t =
       fun f -> bind get (fun s -> put (f s))

      (* let update' : ('d State.t -> 'a * 'd State.t) -> ('d, 'a) t = *)
      (* fun f -> *)
      (* bind get (fun s -> *)
      (* let x, s = f s in *)
      (* bind (put s) (fun _ -> return x)) *)
    end

    include Monad_

    let store_memo q d =
      update (fun { memo } ->
          Memo.store memo q d;
          { memo })

    let retrieve_memo q =
      let open Syntax in
      get >>| fun { memo; _ } -> Memo.retrieve memo q
  end

  module Syntax = Monad.Syntax

  type 'd f = {
    fetch : 'd Query.t -> ('d, 'd) Monad.t;
    pure : 'd -> ('d, 'd) Monad.t;
    io : 'd -> ('d, 'd) Monad.t;
  }

  type 'd rules = 'd f -> 'd Query.t -> ('d, 'd) Monad.t

  let run rules q =
    let rec fetch rules q =
      let open Monad in
      let fetch' q' = fetch rules q' in
      let pure d = d |> return in
      let io d = d |> return in

      (* Invoke rules. *)
      rules { fetch = fetch'; pure; io } q
    in

    let _, d = fetch rules q (State.init ()) in
    d

  let run_with_memo rules q =
    let rec fetch rules q =
      let open Monad in
      let open Monad.Syntax in
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

    let _, d = fetch rules q (State.init ()) in
    d
end
