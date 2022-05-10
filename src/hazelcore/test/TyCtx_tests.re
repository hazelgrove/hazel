open TyCtxStateMonad;

let verbose = false;

let report = vars => {
  print_endline(
    Sexplib.Sexp.to_string_hum(VarMap.sexp_of_t(HTyp.sexp_of_t, vars)),
  );
  // print_endline(Sexplib.Sexp.to_string_hum(TyCtx.sexp_of_t(tyctx)));
  true;
};

let test_vars =
    (
      want: list((Var.t, HTyp.t)),
      got: list((Var.t, HTypSyntax.t(Index.absolute))),
    )
    : bool => {
  if (verbose) {
    print_endline(
      Sexplib.Sexp.to_string_hum(
        VarMap.sexp_of_t(HTypSyntax.sexp_of_t(Index.sexp_of_absolute), got),
      ),
    );
  };
  List.for_all2(
    ((want_x, want_ty), (got_x, got_ty)) =>
      Var.eq(want_x, got_x)
      && HTypSyntax.equal(HTyp.unsafe(want_ty), got_ty),
    want,
    got,
  );
};

let test_tyvars =
    (
      want: list((TyVar.t, Kind.t)),
      got: list((TyVar.t, KindCore.t(Index.absolute))),
    )
    : bool => {
  if (verbose) {
    print_endline(
      Sexplib.Sexp.to_string_hum(
        TyVarMap.sexp_of_t(KindCore.sexp_of_t(Index.sexp_of_absolute), got),
      ),
    );
  };
  List.for_all2(
    ((want_t, want_k), (got_t, got_k)) =>
      TyVar.equal(want_t, got_t) && KindCore.equal(want_k, got_k),
    want,
    got,
  );
};

let k = Kind.singleton(HTyp.int);
let tyvar = (i: int, name: string): HTyp.t =>
  HTyp.tyvar(Index.Abs.of_int(i), name);

let pairing = (f: 'a => 'b, g: 'a => 'c, x: 'a): ('b, 'c) => (f(x), g(x));
let product = (f: 'a => 'b, g: 'c => 'd, (x: 'a, y: 'c)): ('b, 'd) => (
  f(x),
  g(y),
);
let meet = ((x: bool, y: bool)): bool => x && y;

// [(x, @1), -1-, (y, @1), -3-, (z, @1), -5-]
//
// --> [(0, (x,  @1)), (1, -1-), (2, (y, @1)), (3, -3-), (4, (z, @1)), (5, -5-)]
//
// --> [(0, x, @1), (2, y, @1), (4, z, @1)]
//
// --> [(x, @1 + 0), (y, @1 + 2), (z, @1 + 4)]
let%test _ =
  TyCtx.empty
  |> exec(
       sequence([
         push_tyvar("a", k),
         bind_var("x", tyvar(1, "a")),
         push_tyvar("b", k),
         bind_var("y", tyvar(1, "b")),
         push_tyvar("c", k),
         bind_var("z", tyvar(1, "c")),
       ]),
     )
  |> pairing(TyCtx.vars, TyCtx.tyvars)
  |> product(
       test_vars([
         ("z", tyvar(1, "c")),
         ("y", tyvar(3, "b")),
         ("x", tyvar(5, "a")),
       ]),
       test_tyvars([("c", k), ("b", k), ("a", k)]),
     )
  |> meet;

// [(x, @5), -1-, (y, @3), -3-, (z, @1), -5-]
//
// --> [(0, (x, @5)), (1, -1-), (2, (y, @3)), (3, -3-), (4, (z, @1)), (5, -5-)]
//
// --> [(0, x, @5), (2, y, @3), (4, z, @1)]
//
// --> [(x, @5 + 0), (y, @3 + 2), (z, @1 + 4)]
let%test _ =
  TyCtx.empty
  |> exec(
       sequence([
         push_tyvar("a", k),
         bind_var("x", tyvar(1, "a")),
         push_tyvar("b", k),
         bind_var("y", tyvar(3, "a")),
         push_tyvar("c", k),
         bind_var("z", tyvar(5, "a")),
       ]),
     )
  |> pairing(TyCtx.vars, TyCtx.tyvars)
  |> product(
       test_vars([
         ("z", tyvar(5, "a")),
         ("y", tyvar(5, "a")),
         ("x", tyvar(5, "a")),
       ]),
       test_tyvars([("c", k), ("b", k), ("a", k)]),
     )
  |> meet;

// [(t, @3), -1-, (u, @9), -3-, (v, @3), -5-, (w, @7), -7-, (x, @1), -9-, (y, @3), -11-, (z, @1), -13-]
//
// --> [(0, (t, @3)), (1, -1-), (2, (u, @9)), (3, -3-),  (4, (v, @3)),  (5, -5-),
//      (6, (w, @7)), (7, -7-), (8, (x, @1)), (9, -9-), (10, (y, @3)), (11, -11-), (12, (z, @1)), (13, -13-)]
//
// --> [(0, t, @3), (2, u, @9), (4, v, @3), (6, w, @7), (8, x, @1), (10, y, @3), (12, z, @1)]
//
// --> [(t, @3 + 0), (u, @9 + 2), (v, @3 + 4), (w, @7 + 6), (x, @1 + 8), (y, @3 + 10), (z, @1 + 12)]
let%test _ =
  TyCtx.empty
  |> exec(
       sequence([
         push_tyvar("a", k),
         bind_var("t", tyvar(1, "a")),
         push_tyvar("b", k),
         bind_var("u", tyvar(3, "a")),
         push_tyvar("c", k),
         bind_var("v", tyvar(1, "c")),
         push_tyvar("d", k),
         bind_var("w", tyvar(7, "a")),
         push_tyvar("e", k),
         bind_var("x", tyvar(3, "d")),
         push_tyvar("f", k),
         bind_var("y", tyvar(9, "b")),
         push_tyvar("g", k),
         bind_var("z", tyvar(3, "f")),
       ]),
     )
  |> pairing(TyCtx.vars, TyCtx.tyvars)
  |> product(
       test_vars([
         ("z", tyvar(3, "f")),
         ("y", tyvar(11, "b")),
         ("x", tyvar(7, "d")),
         ("w", tyvar(13, "a")),
         ("v", tyvar(9, "c")),
         ("u", tyvar(13, "a")),
         ("t", tyvar(13, "a")),
       ]),
       test_tyvars([
         ("g", k),
         ("f", k),
         ("e", k),
         ("d", k),
         ("c", k),
         ("b", k),
         ("a", k),
       ]),
     )
  |> meet;
