open Graph.Imperative

module type S = sig
  type 'd query
  type ex_query = Q : _ query -> ex_query
  type t

  val create : ?size:int -> unit -> t
  val add : t -> _ query -> _ query -> unit
  val deps : t -> _ query -> ex_query list
end

module Make (Query : Query.T) : S with type 'd query = 'd Query.t = struct
  type 'd query = 'd Query.t
  type ex_query = Q : _ query -> ex_query

  module G = Digraph.Abstract (struct
    type t = ex_query
  end)

  type t = G.t

  let vertex q = G.V.create (Q q)
  let create = G.create

  let add graph q q' =
    let v = vertex q in
    let v' = vertex q' in
    match G.find_all_edges graph v v' with
    | [] | [ _ ] ->
        let edge = G.E.create v () v' in
        G.add_edge_e graph edge
    | _ -> failwith "multiple edges between two query vertices"

  let deps graph q =
    let v = vertex q in
    if G.mem_vertex graph v then
      G.succ_e graph v |> List.map (fun edge -> G.V.label (G.E.dst edge))
    else []
end
