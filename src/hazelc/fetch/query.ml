module type T = sig
  type _ t

  val equal : 'd0 t -> 'd1 t -> ('d0 t, 'd1 t) Gadt.Eq.t option
end
