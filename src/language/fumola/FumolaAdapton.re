/* The Adapton store's own vocabulary, as Hazel types.

   `Adapton.peekHistory` hands back three lists -- the events, the nodes and
   the edges -- and each is a Fumola value that FumolaValue can translate into
   a Hazel value, provided something tells it what type to expect. Inside a
   program that something is the program's own type declarations, which is
   what the `Fumola (Tiles) / Node info` slide writes out by hand. The Fumola
   panel is not inside a program, so it carries its own copy here.

   These mirror that slide, which in turn mirrors `fumola/system/adapton.fumola`
   -- Align is `#aligned` or `#signaled` (adapton.fumola:87) and Action's four
   cases all carry `Any` (adapton.fumola:125-128), which is why their payloads
   are Unknown here. Checked against a live runtime as well: across six
   instances the only actions seen were put, get, force_ and forceBegin, and
   the only align seen was aligned.

   Two of these have no counterpart on the slide, because the slide is about
   `peekInfo` and this is `peekHistory`: a node row and an edge row are the
   records those two lists are made of. */

/* The types themselves are builtins now (BuiltinsADT.Adapton), so the
   panel and a program that writes `: EventRow` or `: EdgeRow` read a value
   out of the same runtime at the same type. They were a private copy here,
   and a program saw every constructor in them as unbound. */
let symbol = BuiltinsADT.Symbol.t;

/* The rows peekHistory's node and edge lists are made of. */
let node_row = () => BuiltinsADT.Adapton.node_row;
let edge_row = () => BuiltinsADT.Adapton.edge_row;
let nodes = () => Typ.fresh(List(node_row()));
let edges = () => Typ.fresh(List(edge_row()));

/* What FumolaValue needs to push these types down through a value.

   Neither function consults a typing context, because there is none here.
   `normalize` unfolds only what these types can contain -- Symbol, which is
   recursive through its own alias -- and `resolve_ctr` looks a constructor up
   in the sum it is being read at, which is what a context lookup would have
   answered anyway for types nothing else declares. */
let rec normalize = (ty: Typ.t): Typ.t =>
  switch (ty.term) {
  | Var("Symbol") => normalize(symbol)
  | Rec(_, body) => body
  | _ => ty
  };

let resolve_ctr = (~ana: Typ.t, name: string): option(Typ.t) => {
  let found = (variants: list(ConstructorMap.variant(Typ.t))) =>
    List.find_map(
      (variant: ConstructorMap.variant(Typ.t)) =>
        switch (variant) {
        | Variant(n, _, payload) when n == name =>
          Some(
            switch (payload) {
            | Some(payload) => Typ.fresh(Arrow(payload, ana))
            | None => ana
            },
          )
        | Variant(_, _, _)
        | BadEntry(_) => None
        },
      variants,
    );
  switch (normalize(ana).term) {
  | Sum(variants) => found(variants)
  | _ => None
  };
};

let tools: FumolaTools.t = {
  resolve_ctr,
  normalize,
};
