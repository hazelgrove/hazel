let ctx: VarCtx.t = [
  ("int_of_float", Arrow(Float, Int)),
  ("float_of_int", Arrow(Int, Float)),
];

let lookup = x => VarMap.lookup(ctx, x);
