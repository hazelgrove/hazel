open ContextsMonad;

let verbose = false;

let test_vars =
    (
      want: list((int, Var.t, HTyp.t)),
      got: list((Index.Abs.t, Var.t, HTypSyntax.t(Index.absolute))),
    )
    : bool => {
  if (verbose) {
    print_endline("\nWANT VARS:");
    print_endline(
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_list(
          ((i, x, ty)) =>
            Sexplib.Sexp.List([
              Sexplib.Std.sexp_of_int(i),
              Var.sexp_of_t(x),
              HTyp.sexp_of_t(ty),
            ]),
          want,
        ),
      ),
    );
    print_endline("GOT VARS:");
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
    print_endline("\nWANT TYVARS:");
    print_endline(
      Sexplib.Sexp.to_string_hum(
        Sexplib.Std.sexp_of_list(
          ((i, t, k)) =>
            Sexplib.Sexp.List([
              Sexplib.Std.sexp_of_int(i),
              TyVar.sexp_of_t(t),
              Kind.sexp_of_t(Index.sexp_of_absolute, k),
            ]),
          want,
        ),
      ),
    );
    print_endline("GOT TYVARS:");
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

// [(a, k)]
// ~> tyvars=[(0, a, k)]
//      vars=[]
let%test _ =
  Contexts.initial
  |> exec(add_tyvar("a", k))
  |> pairing(Contexts.tyvars, Contexts.vars)
  |> product(test_tyvars([(0, "a", k)]), test_vars([]))
  |> meet;

// [(x, a), (a, k)]
// ~> tyvars=[(1, a, k)]
//      vars=[(0, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(add_tyvar("a", k) $++ add_var("x", tyvar(0, "a")))
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(1, "a", k)]),
         test_vars([(0, "x", tyvar(1, "a"))]),
       )
    |> meet
  );

// [(b, k), (x, a), (a, k)]
// ~> tyvars=[(0, b, k), (2, a, k)]
//      vars=[(1, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_var("x", tyvar(0, "a"))
         $++ add_tyvar("b", k),
       )
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(0, "b", k), (2, "a", k)]),
         test_vars([(1, "x", tyvar(2, "a"))]),
       )
    |> meet
  );

// [(y, b), (b, k), (x, a), (a, k)]
// ~> tyvars=[(1, b, k), (3, a, k)]
//      vars=[(0, y, b), (2, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_var("x", tyvar(0, "a"))
         $++ add_tyvar("b", k)
         $++ add_var("y", tyvar(0, "b")),
       )
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(1, "b", k), (3, "a", k)]),
         test_vars([(0, "y", tyvar(1, "b")), (2, "x", tyvar(3, "a"))]),
       )
    |> meet
  );

// [(y, a), (b, k), (x, a), (a, k)]
// ~> tyvars=[(1, b, k), (3, a, k)]
//      vars=[(0, y, a), (2, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_var("x", tyvar(0, "a"))
         $++ add_tyvar("b", k)
         $++ add_var("y", tyvar(2, "a")),
       )
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(1, "b", k), (3, "a", k)]),
         test_vars([(0, "y", tyvar(3, "a")), (2, "x", tyvar(3, "a"))]),
       )
    |> meet
  );

// [(z, a), (c, k), (y, a), (b, k), (x, a), (a, k)]
// ~> tyvars=[(1, c, k), (3, b, k), (5, a, k)]
//      vars=[(0, z, a), (2, y, a), (4, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_var("x", tyvar(0, "a"))
         $++ add_tyvar("b", k)
         $++ add_var("y", tyvar(2, "a"))
         $++ add_tyvar("c", k)
         $++ add_var("z", tyvar(4, "a")),
       )
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(1, "c", k), (3, "b", k), (5, "a", k)]),
         test_vars([
           (0, "z", tyvar(5, "a")),
           (2, "y", tyvar(5, "a")),
           (4, "x", tyvar(5, "a")),
         ]),
       )
    |> meet
  );

// [(z, c), (c, k), (y, b), (b, k), (x, a), (a, k)]
// ~> tyvars=[(1, c, k), (3, b, k), (5, a, k)]
//      vars=[(0, z, c), (2, y, b), (4, x, a)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_var("x", tyvar(0, "a"))
         $++ add_tyvar("b", k)
         $++ add_var("y", tyvar(0, "b"))
         $++ add_tyvar("c", k)
         $++ add_var("z", tyvar(0, "c")),
       )
    |> pairing(Contexts.tyvars, Contexts.vars)
    |> product(
         test_tyvars([(1, "c", k), (3, "b", k), (5, "a", k)]),
         test_vars([
           (0, "z", tyvar(1, "c")),
           (2, "y", tyvar(3, "b")),
           (4, "x", tyvar(5, "a")),
         ]),
       )
    |> meet
  );

// [(c, k), (b, k), (a, k)]
// ~> tyvars=[(0, c, k), (1, b, k), (2, a, k)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(add_tyvar("a", k) $++ add_tyvar("b", k) $++ add_tyvar("c", k))
    |> Contexts.tyvars
    |> test_tyvars([(0, "c", k), (1, "b", k), (2, "a", k)])
  );

// [(b, S(a)), (a, k)]
// ~> tyvars=[(0, b, S(a)), (1, a, k)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_tyvar("b", Kind.singleton(tyvar(0, "a"))),
       )
    |> Contexts.tyvars
    |> test_tyvars([(0, "b", Kind.singleton(tyvar(1, "a"))), (1, "a", k)])
  );

// [(c, S(a)), (b, k), (a, k)]  -->  [(c, S(a)), (a, k)]
// ~> tyvars=[(0, c, S(a)), (1, a, k)]
let%test _ =
  ContextsMonad.Infix.(
    Contexts.initial
    |> exec(
         add_tyvar("a", k)
         $++ add_tyvar("b", k)
         $++ add_tyvar("c", Kind.singleton(tyvar(1, "a")))
         $++ remove_tyvar(Index.Abs.of_int(1), HTyp.unsafe(HTyp.hole)),
       )
    |> Contexts.tyvars
    |> test_tyvars([(0, "c", Kind.singleton(tyvar(1, "a"))), (1, "a", k)])
  );
