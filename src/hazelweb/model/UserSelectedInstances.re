[@deriving sexp]
type t = {
  holes: MetaVarMap.t(MetaVarInst.t),
  livelits: MetaVarMap.t(MetaVarInst.t),
};

let init = {holes: MetaVarMap.empty, livelits: MetaVarMap.empty};

let _get_map = ({holes, livelits}, kind: TaggedNodeInstance.kind) => {
  switch (kind) {
  | Hole => holes
  | Livelit => livelits
  };
};

let lookup = (kind, u, si) => MetaVarMap.lookup(_get_map(si, kind), u);

let insert_or_update = ((kind, inst), usi) => {
  let map_ = MetaVarMap.insert_or_update(_get_map(usi, kind), inst);
  switch (kind) {
  | Hole => {...usi, holes: map_}
  | Livelit => {...usi, livelits: map_}
  };
};
