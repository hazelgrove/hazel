module ExprLabel0: Label.Gen.L;
module Map: (module type of Label.Map.Make(ExprLabel0));

include  (module type of ExprLabel0) with type t = ExprLabel0.t;
