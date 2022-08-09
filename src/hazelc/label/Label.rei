include Gen.L;

module GenMonad: Gen.MonadS with type label = t;
module Gen: Gen.S with type label = t;
module Map: Util.MapSexp.S with type key = t;
