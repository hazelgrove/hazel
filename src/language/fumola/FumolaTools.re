/* What reading a Fumola value back into Hazel needs to know about Hazel's
   types.

   A Fumola result is untyped JSON: a tagged value is `{tag, payload}` and says
   nothing about which Hazel sum it belongs to. Which Hazel constructor it
   becomes, and whether a record's fields are in the order Hazel wants, both
   depend on the type expected at the position the value is landing in. These
   two functions are the whole of that dependency.

   On fumola-livelit-mvp this lived in LivelitCtx, because a livelit's expand
   was the only caller. Nothing here is about livelits, so it lives with the
   Fumola code that uses it. */

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  /* The type of constructor [name] at a position expected to have type
     [ana] -- for a constructor carrying a payload, an arrow from the payload
     type. None when the name is not a constructor of any type in scope. */
  resolve_ctr: (~ana: TermBase.Typ.t, string) => option(TermBase.Typ.t),
  /* Unfold type aliases, so an expected type written as a name can be
     destructured. */
  normalize: TermBase.Typ.t => TermBase.Typ.t,
};

/* What to use where there is no context to ask: the worker, and the test
   runner. Nothing unfolds and no constructor resolves, so a result comes
   back with whatever shape it has rather than the shape something expected
   -- which is a worse answer than the real tools give, and a better one than
   a guess. */
let unknown: t = {
  resolve_ctr: (~ana as _, _) => None,
  normalize: ty => ty,
};
