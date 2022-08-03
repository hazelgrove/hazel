module StmtLabel0: Label.Gen.L;
module Map: (module type of Label.Map.Make(StmtLabel0));

include  (module type of StmtLabel0) with type t = StmtLabel0.t;
