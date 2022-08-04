module type S = sig
  type 'd query

  module Memo : Memo.S with type 'd key = 'd query

  type 'd state = { memo : 'd Memo.t }

  val init : unit -> 'd state

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

  val get : ('d, 'd state) t
  val put : 'd state -> ('d, unit) t
  val update : ('d state -> 'd state) -> ('d, unit) t
  val update' : ('d state -> 'a * 'd state) -> ('d, 'a) t
  val run_state : ('d, 'a) t -> 'd state -> 'a
end

module Make (Query : Query.T) : S with type 'd query = 'd Query.t = struct
  type 'd query = 'd Query.t

  module Memo = Memo.Make (Query)

  type 'd state = { memo : 'd Memo.t }

  let init () = { memo = Memo.create 50 }

  type ('d, 'a) t = 'd state -> 'd state * 'a

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

  let get s = (s, s)
  let put x _ = (x, ())
  let update f = bind get (fun s -> put (f s))

  let update' f =
    bind get (fun s ->
        let x, s = f s in
        bind (put s) (fun _ -> return x))

  let run_state m s = m s |> snd
end
