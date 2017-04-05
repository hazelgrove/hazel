open Hazel_semantics

module Model = struct
  type t = ZExp.t * HTyp.t

  let empty = ((ZExp.CursorE HExp.EmptyHole), HTyp.Hole)

  (* react *)
  type rs = t React.signal (* reactive signal *)
  type rf = ?step:React.step -> t -> unit (* update function *)
  type rp = rs * rf (* pair of rs and rf *)
end


