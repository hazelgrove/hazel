(**
   Dependent hash table, inspired by https://github.com/ollef/dependent-hashmap.
 *)

(** A GADT key type module. *)
module type KEY = sig
  type 'a t
  (** The type for the key. *)

  val equal : 'a t -> 'b t -> ('a t, 'b t) Gadt.Eq.t option
  (** [equal x x'] is [Some refl], where [refl] is an equality witness, or
      [None]. *)
end

(** Output of the [Make] functor. *)
module type S = sig
  type 'a key
  type 'v t

  val create : ?random:bool -> int -> 'v t
  (** See {!val:Hashtbl.create}. *)

  val add : 'v t -> 'a key -> 'v -> unit
  (** See {!val:Hashtbl.add}. *)

  val find_opt : 'v t -> 'a key -> 'v option
  (** See {!val:Hashtbl.find_opt}. *)

  val find : 'v t -> 'a key -> 'v
  (** See {!val:Hashtbl.find}. *)

  val mem : 'v t -> 'a key -> bool
  (** See {!val:Hashtbl.mem}. *)

  val remove : 'v t -> 'a key -> unit
  (** See {!val:Hashtbl.remove}. *)

  val replace : 'v t -> 'a key -> 'v -> unit
  (** See {!val:Hashtbl.replace}. *)

  val length : 'v t -> int
  (** See {!val:Hashtbl.length}. *)
end

(** Functor to make the implementation of the dependent hashtable for the given
    key type. *)
module Make (K : KEY) : S with type 'a key = 'a K.t
