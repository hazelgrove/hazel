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

let find_opt = (kind, u, si) => MetaVarMap.find_opt(u, _get_map(si, kind));

let add = ((kind, (u, i)), usi) => {
  let map_ = MetaVarMap.add(u, i, _get_map(usi, kind));
  switch (kind) {
  | Hole => {...usi, holes: map_}
  | Livelit => {...usi, livelits: map_}
  };
};
