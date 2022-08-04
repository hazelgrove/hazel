module type KEY = sig
  type 'a t

  val equal : 'a t -> 'b t -> ('a t, 'b t) Gadt.Eq.t option
end

module type S = sig
  type 'a key
  type 'v t

  val create : ?random:bool -> int -> 'v t
  val add : 'v t -> 'a key -> 'v -> unit
  val find_opt : 'v t -> 'a key -> 'v option
  val find : 'v t -> 'a key -> 'v
  val mem : 'v t -> 'a key -> bool
  val remove : 'v t -> 'a key -> unit
  val replace : 'v t -> 'a key -> 'v -> unit
  val length : 'v t -> int
end

module Make (K : KEY) : S with type 'a key = 'a K.t = struct
  type 'a key = 'a K.t
  type ex = K : 'a K.t -> ex
  type 'v t = (ex, ex * 'v) Hashtbl.t

  let create : ?random:bool -> int -> 'v t = Hashtbl.create

  let add : type a. 'v t -> a K.t -> 'v -> unit =
   fun tbl k v -> Hashtbl.add tbl (K k) (K k, v)

  let find_opt : type a. 'v t -> a K.t -> 'v option =
   fun tbl k ->
    match Hashtbl.find_opt tbl (K k) with
    | Some (K k', v) -> (
        match K.equal k k' with Some Refl -> Some v | None -> None)
    | None -> None

  let find : type a. 'v t -> a K.t -> 'v =
   fun tbl k ->
    match find_opt tbl k with Some v -> v | None -> raise Not_found

  let mem : type a. 'v t -> a K.t -> bool =
   fun tbl k -> find_opt tbl k |> Option.is_some

  let remove : type a. 'v t -> a K.t -> unit =
   fun tbl k -> Hashtbl.remove tbl (K k)

  let replace : type a. 'v t -> a K.t -> 'v -> unit =
   fun tbl k v -> Hashtbl.replace tbl (K k) (K k, v)

  let length : 'v t -> int = fun tbl -> Hashtbl.length tbl
end
