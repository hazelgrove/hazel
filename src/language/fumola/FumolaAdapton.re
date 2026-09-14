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

module Fresh = IdTagged.FreshGrammar;

let unknown = () => Typ.fresh(Unknown(Internal));

let sum = (variants: list((string, option(Typ.t)))): Typ.t =>
  variants
  |> List.map(((name, payload)) =>
       ConstructorMap.Variant(
         name,
         ConstructorMap.mk_variant_ann(~ids=[Id.mk()], ()),
         payload,
       )
     )
  |> Fresh.Typ.sum;

/* A labelled tuple, which is what a Fumola record becomes: see the Record
   case of FumolaValue.typ_of_json. */
let field = (name: string, ty: Typ.t): Typ.t =>
  Typ.fresh(TupLabel(Typ.fresh(Label(name)), ty));

let record = (fields: list((string, Typ.t))): Typ.t =>
  Typ.fresh(Prod(List.map(((name, ty)) => field(name, ty), fields)));

let tuple = (tys: list(Typ.t)): Typ.t => Typ.fresh(Prod(tys));

let int = () => Typ.fresh(Atom(Int));

let symbol = BuiltinsADT.Symbol.t;

/* Space and Time carry a symbol rather than its text. The builtin Space and
   Time in BuiltinsADT carry a String instead, deliberately -- a symbol
   arrives as its text where a String is asked for -- but the panel wants the
   structure, so that a node's name reads as `Num(1)` rather than "1" and can
   be compared with the symbol in an event. */
let space = () => sum([("Here", None), ("Symbol", Some(symbol))]);

let time = () => sum([("Now", None), ("Symbol", Some(symbol))]);

let node_id = () => tuple([space(), time(), int()]);

let edge_id = () => sum([("EdgeId", Some(int()))]);

let align = () => sum([("Aligned", None), ("Signaled", None)]);

/* Every payload is Any on the Fumola side: a put carries whatever was put, a
   force carries the thunk and its result. A closed type here would be a claim
   about the runtime that is not true. */
let action = () =>
  sum([
    ("ForceBegin", Some(unknown())),
    ("Force_", Some(tuple([unknown(), unknown()]))),
    ("Put", Some(unknown())),
    ("Get", Some(unknown())),
  ]);

let edge = () =>
  record([
    ("source", node_id()),
    ("target", node_id()),
    ("action", action()),
    ("metaTimes", tuple([int(), int()])),
    /* `status` since Adapton/fumola#133; FumolaHistory renames the older
       `align` on the way in, so either spelling reaches here as this one. */
    ("status", align()),
  ]);

let thunk_node = () =>
  record([
    ("body", unknown()),
    ("result", BuiltinsADT.Option.t),
    ("space", space()),
    ("trace", Typ.fresh(List(edge_id()))),
  ]);

let node = () =>
  sum([("NonThunk", Some(unknown())), ("Thunk_", Some(thunk_node()))]);

/* The rows peekHistory's two lists are made of. */
let node_row = () =>
  record([("metaTime", int()), ("node", node()), ("nodeId", node_id())]);

/* Three fields, not two: an edge row carries the moment it was recorded at
   as well, which the node row also does. Read off a live instance -- the
   `Node info` slide never sees this record, since it is peekHistory's and not
   peekInfo's. */
let edge_row = () =>
  record([("edgeId", edge_id()), ("edge", edge()), ("metaTime", int())]);

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
