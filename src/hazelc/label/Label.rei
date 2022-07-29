module Label = Label0;
module Gen = Gen;

module Map: {module Make: (L: Gen.L) => Map.S with type key = L.t;};
module Set: {module Make: (L: Gen.L) => Set.S with type elt = L.t;};
