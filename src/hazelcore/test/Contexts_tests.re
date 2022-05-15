open ContextsMonad;

let verbose = false;

let report = vars => {
  print_endline(
    Sexplib.Sexp.to_string_hum(
      Sexplib.Std.sexp_of_list(Var.sexp_of_t, vars),
    ),
  );
  // print_endline(Sexplib.Sexp.to_string_hum(Contexts.sexp_of_t(tyctx)));
  true;
};

let test_vars =
    (
      want: list((int, Var.t, HTyp.t)),
      got: list((Index.Abs.t, Var.t, HTypSyntax.t(Index.absolute))),
    )
    : bool => {
  if (verbose) {
    print_endline(
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_list(
          ((idx, x, ty)) =>
            Sexplib.Sexp.List([
              Index.Abs.sexp_of_t(idx),
              Var.sexp_of_t(x),
              HTypSyntax.sexp_of_t(Index.sexp_of_absolute, ty),
            ]),
          got,
        ),
      ),
    );
  };
  List.for_all2(
    ((want_i, want_x, want_ty), (got_idx, got_x, got_ty)) =>
      want_i == Index.Abs.to_int(got_idx)
      && Var.eq(want_x, got_x)
      && HTypSyntax.equal(HTyp.unsafe(want_ty), got_ty),
    want,
    got,
  );
};

let test_tyvars =
    (
      want: list((int, TyVar.t, Kind.t)),
      got: list((Index.Abs.t, TyVar.t, KindCore.t(Index.absolute))),
    )
    : bool => {
  if (verbose) {
    print_endline(
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_list(
          ((idx, t, k)) =>
            Sexplib.Sexp.List([
              Index.Abs.sexp_of_t(idx),
              TyVar.sexp_of_t(t),
              Kind.sexp_of_t(Index.sexp_of_absolute, k),
            ]),
          got,
        ),
      ),
    );
  };
  List.for_all2(
    ((want_i, want_t, want_k), (got_idx, got_t, got_k)) =>
      want_i == Index.Abs.to_int(got_idx)
      && TyVar.equal(want_t, got_t)
      && KindCore.equal(want_k, got_k),
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

// TODO: (eric) implement index shifting at type abstraction boundaries

// [("a", Singleton(Rec.@0 + @1)), -1-]

// TODO: (eric) make sure there are no unbound type variables in anything exported from Context

// [(x, @1), -1-, (y, @1), -3-, (z, @1), -5-]
//
// --> [(0, (x,  @1)), (1, -1-), (2, (y, @1)), (3, -3-), (4, (z, @1)), (5, -5-)]
//
// --> [(0, x, @1), (2, y, @1), (4, z, @1)]
//
// --> [(x, @1 + 0), (y, @1 + 2), (z, @1 + 4)]
let%test _ =
  Contexts.initial
  |> exec(
       sequence([
         add_tyvar("a", k),
         add_var("x", tyvar(1, "a")),
         add_tyvar("b", k),
         add_var("y", tyvar(1, "b")),
         add_tyvar("c", k),
         add_var("z", tyvar(1, "c")),
       ]),
     )
  |> pairing(Contexts.vars, Contexts.tyvars)
  |> product(
       test_vars([
         (4, "z", tyvar(1, "c")),
         (2, "y", tyvar(3, "b")),
         (0, "x", tyvar(5, "a")),
       ]),
       test_tyvars([(5, "c", k), (3, "b", k), (1, "a", k)]),
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
  Contexts.initial
  |> exec(
       sequence([
         add_tyvar("a", k),
         add_var("x", tyvar(1, "a")),
         add_tyvar("b", k),
         add_var("y", tyvar(3, "a")),
         add_tyvar("c", k),
         add_var("z", tyvar(5, "a")),
       ]),
     )
  |> pairing(Contexts.vars, Contexts.tyvars)
  |> product(
       test_vars([
         (4, "z", tyvar(5, "a")),
         (2, "y", tyvar(5, "a")),
         (0, "x", tyvar(5, "a")),
       ]),
       test_tyvars([(5, "c", k), (3, "b", k), (1, "a", k)]),
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
  Contexts.initial
  |> exec(
       sequence([
         add_tyvar("a", k),
         add_var("t", tyvar(1, "a")),
         add_tyvar("b", k),
         add_var("u", tyvar(3, "a")),
         add_tyvar("c", k),
         add_var("v", tyvar(1, "c")),
         add_tyvar("d", k),
         add_var("w", tyvar(7, "a")),
         add_tyvar("e", k),
         add_var("x", tyvar(3, "d")),
         add_tyvar("f", k),
         add_var("y", tyvar(9, "b")),
         add_tyvar("g", k),
         add_var("z", tyvar(3, "f")),
       ]),
     )
  |> pairing(Contexts.vars, Contexts.tyvars)
  |> product(
       test_vars([
         (12, "z", tyvar(3, "f")),
         (10, "y", tyvar(11, "b")),
         (8, "x", tyvar(7, "d")),
         (6, "w", tyvar(13, "a")),
         (4, "v", tyvar(9, "c")),
         (2, "u", tyvar(13, "a")),
         (0, "t", tyvar(13, "a")),
       ]),
       test_tyvars([
         (13, "g", k),
         (11, "f", k),
         (9, "e", k),
         (7, "d", k),
         (5, "c", k),
         (3, "b", k),
         (1, "a", k),
       ]),
     )
  |> meet;
