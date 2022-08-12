module Gen_ = Gen;

module Label0 = Label0;

module Gen = Gen_.Make(Label0);
module GenMonad = Gen_.Monad(Gen);

module Map = Util.MapSexp.Make(Label0);
module Set = Util.SetSexp.Make(Label0);

include Label0;
