(** A collection of timers used to cut off program execution early. *)

(** A countdown timer that, once started, continues to completion. *)
module Single : sig
  (** The available single timers. *)
  type t =
    | Total  (** The timer for the total synthesis time. *)
    | Eval  (** The timer for live evaluation and resumption. *)

  val start : t -> unit
  (** Starts the timer. *)

  val elapsed : t -> float
  (** Returns how much time has elapsed since starting the timer. *)

  val check : t -> bool
  (** Returns [true] if the timer is still running, and [false] if the
      elapsed time is greater than the cutoff. *)
end

(** A resettable countdown timer that accumulates time every time the
    {!accumulate} function is called. *)
module Multi : sig
  (** The available multi timers. *)
  type t = Guess  (** The timer for raw term enumeration. *)

  val reset : t -> unit
  (** Resets the timer. *)

  val accumulate : t -> (unit -> 'a) -> 'a
  (** Accumulates additional time on the timer. *)

  val check : t -> bool
  (** Returns [true] if the timer is still running, and [false] if the
      elapsed time is greater than the cutoff. *)
end

val itimer_timeout :
  string -> float -> ('a -> 'b) -> 'a -> 'b -> 'b * float * bool
(** A fragile hard-cutoff timer that uses Unix itimers. *)
