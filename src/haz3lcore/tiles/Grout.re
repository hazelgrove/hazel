open Util;

[@deriving (show({with_path: false}), sexp, yojson, eq)]
type shape =
  | Convex
  | Concave;

[@deriving (show({with_path: false}), sexp, yojson)]
type t = {
  id: Id.t,
  shape,
};
let equal = (a: t, b: t) => a.shape == b.shape;
let id = g => g.id;

let shapes = g =>
  switch (g.shape) {
  | Convex => Nib.Shape.(Convex, Convex)
  | Concave => Nib.Shape.(Concave(Precedence.min), Concave(Precedence.min))
  };

let grout_cache: ref(option(Id.t)) = ref(None);

let cache_id = (id: option(Id.t)) => {
  ignore(
    switch (id) {
    | Some(id) => print_endline("setting grout cache to " ++ Id.str3(id))
    | None => print_endline("setting grout cache to none")
    },
  );
  grout_cache := id;
};

let get_cached_id = () =>
  switch (grout_cache.contents) {
  | Some(id) =>
    print_endline("using grout cache: " ++ Id.str3(id));
    grout_cache := None;
    id;
  | None =>
    print_endline("no grout cache");
    grout_cache := None;
    Id.mk();
  };

let mk_fits_shape = (s: Nib.Shape.t): t => {
  id: get_cached_id(),
  shape:
    switch (s) {
    | Convex => Concave
    | Concave(_) => Convex
    },
};

let fits_shape = (g: t, s: Nib.Shape.t) =>
  switch (g.shape, s) {
  | (Convex, Convex)
  | (Concave, Concave(_)) => false
  | (Convex, Concave(_))
  | (Concave, Convex) => true
  };

let fits = (g: t, g': t) =>
  switch (g.shape, g'.shape) {
  | (Convex, Convex)
  | (Concave, Concave) => false
  | (Convex, Concave)
  | (Concave, Convex) => true
  };

let merge = (gs: list(t)): option(t) =>
  switch (gs) {
  | [] => None
  | [hd, ...tl] =>
    switch (ListUtil.split_last_opt(tl)) {
    | None => Some(hd)
    | Some((_, ft)) => hd.shape == ft.shape ? Some(hd) : None
    }
  };
