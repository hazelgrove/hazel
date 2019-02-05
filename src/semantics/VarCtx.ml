module VarCtx =
 struct
  type t = HTyp.t VarMap.t_
  include VarMap
 end
