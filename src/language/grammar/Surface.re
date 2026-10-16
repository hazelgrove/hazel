/* The surface syntax the editor is currently reading and writing.
 *
 * Hazel and ReasonML share one tile engine, one term AST and one back
 * end; what differs is which form definitions are live when tokens
 * are molded (Form's rows), how atoms are read (Token), and how a few
 * forms translate to terms (MakeTerm) or print (ExpToSegment). Tiles
 * store their form family, so already-built syntax keeps its meaning;
 * the surface is consulted when classifying, parsing and printing.
 *
 * This is dynamically scoped: one surface is current at a time (the
 * page mode sets it), and code that must read or write a specific
 * surface regardless of mode wraps itself in `with_`. */

[@deriving (show({with_path: false}), sexp, yojson, eq, enumerate)]
type t =
  | Hazel
  | Reason;

let current: ref(t) = ref(Hazel);

let get = (): t => current^;
let is_reason = (): bool => current^ == Reason;

let set = (s: t): unit => current := s;

let with_ = (s: t, f: unit => 'a): 'a => {
  let prev = current^;
  current := s;
  switch (f()) {
  | r =>
    current := prev;
    r;
  | exception e =>
    current := prev;
    raise(e);
  };
};

/* A per-surface lazily-built value: the builder runs at most once per
 * surface, on first use while that surface is current. */
let memo = (build: t => 'a): (unit => 'a) => {
  let hazel = lazy(build(Hazel));
  let reason = lazy(build(Reason));
  () =>
    switch (current^) {
    | Hazel => Lazy.force(hazel)
    | Reason => Lazy.force(reason)
    };
};
