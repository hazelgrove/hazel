/* open ContextMonad; */
/* let verbose = true; */
/* let test_vars = */
/*     ( */
/*       want: list((int, Var.t, HTyp.t)), */
/*       got: list((Index.Abs.t, Var.t, HTyp.t)), */
/*     ) */
/*     : bool => { */
/*   if (verbose) { */
/*     Format.printf( */
/*       "\nWANT VARS:\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((i, x, ty)) => */
/*             Sexplib.Sexp.List([ */
/*               Sexplib.Std.sexp_of_int(i), */
/*               Var.sexp_of_t(x), */
/*               HTyp.sexp_of_t(ty), */
/*             ]), */
/*           want, */
/*         ), */
/*       ), */
/*     ); */
/*     Format.printf( */
/*       "GOT VARS:\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((idx, x, ty)) => */
/*             Sexplib.Sexp.List([ */
/*               Index.Abs.sexp_of_t(idx), */
/*               Var.sexp_of_t(x), */
/*               HTyp.sexp_of_t(ty), */
/*             ]), */
/*           got, */
/*         ), */
/*       ), */
/*     ); */
/*   }; */
/*   List.for_all2( */
/*     ((want_i, want_x, want_ty), (got_idx, got_x, got_ty)) => */
/*       want_i == Index.Abs.to_int(got_idx) */
/*       && Var.eq(want_x, got_x) */
/*       && HTyp.equivalent(Context.initial, want_ty, got_ty), */
/*     want, */
/*     got, */
/*   ); */
/* }; */
/* let test_tyvars_abs = */
/*     ( */
/*       want: list((int, TyVar.t, Kind.t)), */
/*       got: list((Index.Abs.t, TyVar.t, Kind.t)), */
/*     ) */
/*     : bool => { */
/*   if (verbose) { */
/*     Format.printf( */
/*       "\nWANT TYVARS (absolute):\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((i, t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               Sexplib.Std.sexp_of_int(i), */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_t(k), */
/*             ]), */
/*           want, */
/*         ), */
/*       ), */
/*     ); */
/*     Format.printf( */
/*       "GOT TYVARS (absolute):\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((idx, t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               Index.Abs.sexp_of_t(idx), */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_t(k), */
/*             ]), */
/*           got, */
/*         ), */
/*       ), */
/*     ); */
/*   }; */
/*   List.for_all2( */
/*     ((want_i, want_t, want_k), (got_idx, got_t, got_k)) => */
/*       want_i == Index.Abs.to_int(got_idx) */
/*       && TyVar.equal(want_t, got_t) */
/*       && Kind.equivalent(Context.initial, want_k, got_k), */
/*     want, */
/*     got, */
/*   ); */
/* }; */
/* let test_tyvars_rel = */
/*     ( */
/*       want: list((TyVar.t, KindSystem.Kind_core.s(Index.relative))), */
/*       got: list((TyVar.t, KindSystem.Kind_core.s(Index.relative))), */
/*     ) */
/*     : bool => { */
/*   if (verbose) { */
/*     Format.printf( */
/*       "\nWANT TYVARS (relative):\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_s(Index.sexp_of_relative, k), */
/*             ]), */
/*           want, */
/*         ), */
/*       ), */
/*     ); */
/*     Format.printf( */
/*       "GOT TYVARS (relative):\n%s\n", */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_s(Index.sexp_of_relative, k), */
/*             ]), */
/*           got, */
/*         ), */
/*       ), */
/*     ); */
/*   }; */
/*   List.for_all2( */
/*     ((want_t, want_k), (got_t, got_k)) => */
/*       want_t == got_t && want_k == got_k, */
/*     want, */
/*     got, */
/*   ); */
/* }; */
/* let k = Kind.singleton(HTyp.int()); */
/* let tyvar = (i: int, stamp: int, t: TyVar.t): HTyp.t => */
/*   HTyp.of_syntax( */
/*     KindSystem.HTyp_syntax.TyVar({index: Index.Abs.of_int(i), stamp}, t), */
/*   ); */
/* let pairing = (f: 'a => 'b, g: 'a => 'c, x: 'a): ('b, 'c) => (f(x), g(x)); */
/* let product = (f: 'a => 'b, g: 'c => 'd, (x: 'a, y: 'c)): ('b, 'd) => ( */
/*   f(x), */
/*   g(y), */
/* ); */
/* let meet = ((x: bool, y: bool)): bool => x && y; */
/* // TODO: (eric) implement index shifting at type abstraction boundaries */
/* // [("a", Singleton(Rec.@0 + @1)), -1-] */
/* // TODO: (eric) make sure there are no unbound type variables in anything exported from Context */
/* // [(a, k)] */
/* // ~> tyvars=[(0, a, k)] */
/* //      vars=[] */
/* let%test _ = */
/*   Context.initial */
/*   |> exec(add_tyvar("a", k)) */
/*   |> pairing(Context.tyvars, Context.vars) */
/*   |> product(test_tyvars_abs([(0, "a", k)]), test_vars([])) */
/*   |> meet; */
/* // [(x, a), (a, k)] */
/* // ~> tyvars=[(1, a, k)] */
/* //      vars=[(0, x, a#1)] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec(add_tyvar("a", k) $++ add_var("x", tyvar(0, 2, "a"))) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(1, "a", k)]), */
/*          test_vars([(0, "x", tyvar(1, 2, "a"))]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(b, a), (a, k)] */
/* // ~< tyvars=[(b, a#0), (a, k)] */
/* //      vars=[] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_tyvar("b", Kind.singleton(tyvar(0, 2, "a"))), */
/*        ) */
/*     |> Context.to_list */
/*     |> snd */
/*     |> test_tyvars_rel([ */
/*          ("b", S(TyVar(Index.Rel.of_int(0), 2, "a"))), */
/*          ("a", S(Int)), */
/*        ]) */
/*   ); */
/* // [(c, a), (b, a), (a, k)] */
/* // ~> tyvars=[(c, a#1), (b, a#0), (a, k)] */
/* //      vars=[] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_tyvar("b", Kind.singleton(tyvar(0, 1, "a"))) */
/*          $++ add_tyvar("c", Kind.singleton(tyvar(1, 2, "a"))), */
/*        ) */
/*     |> Context.to_list */
/*     |> snd */
/*     |> test_tyvars_rel([ */
/*          ("c", S(TyVar(Index.Rel.of_int(1), 2, "a"))), */
/*          ("b", S(TyVar(Index.Rel.of_int(0), 2, "a"))), */
/*          ("a", S(Int)), */
/*        ]) */
/*   ); */
/* /\* */
/*    a : k |- ((b, c) => {add "b" b; add "c" c}) (a, a) */
/*    want:                       /          / */
/*      b = a#0 -----------------+          / */
/*      c = a#1 ---------------------------+ */
/*    got: (b, c) = (a#0, a#0) */
/*    After adding b to the context, a is no longer at absolute position 0, so all */
/*    existing references to a are no longer valid. In particular, c is now */
/*    invalid. The current system lets us add c, but the new binding will behave */
/*    like b - the new binding at absolute position 0 - and it will contain the */
/*    name "a", which could be confusing to see in diagnostic messages or the user */
/*    interface. */
/*    Our current architecture is entirely unaware of this violation of intent. We */
/*    would need something like a borrow checker to ensure stale references are */
/*    never in scope. */
/*  *\/ */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_tyvar("b", Kind.singleton(tyvar(0, 1, "a"))) */
/*          $++ add_tyvar("c", Kind.singleton(tyvar(0, 2, "a"))), */
/*        ) */
/*     |> Context.to_list */
/*     |> snd */
/*     |> test_tyvars_rel([ */
/*          ("c", S(TyVar(Index.Rel.of_int(0), 2, "a"))), */
/*          ("b", S(TyVar(Index.Rel.of_int(0), 1, "a"))), */
/*          ("a", S(Int)), */
/*        ]) */
/*   ); */
/* /\* */
/*    [(a, k)] */
/*    ~> (x : a#0) [(x, a#0), (a, k)] */
/*    [(y, ty), (a, k)] */
/*    ~> (x : a#1) [(x, a#1), (y, ty), (a, k)] */
/*    [(b, k), (a, k)] */
/*    ~> (x : a#1) [(x, a#1), (b, k), (a, k)] */
/*    [(b, k), (a, k)] */
/*    ~> (x : b#0) [(x, b#0), (b, k), (a, k)] */
/*    [b x:a0 a]  -->  x : a|0+1+1=2| */
/*  i= 0 1    2 */
/*    [y:? b x:a0 a]  -->  x : a|0+2+1=3| */
/*  i= 0   1 2    3 */
/*    [c b x:a0 a]  -->  x : a|0+2+1=3| */
/*  i= 0 1 2    3 */
/*   *\/ */
/* // [(b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, b, k), (2, a, k)] */
/* //      vars=[(1, x, a#2)] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, 1, "a")) */
/*          $++ add_tyvar("b", k), */
/*        ) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(0, "b", k), (2, "a", k)]), */
/*          test_vars([(1, "x", tyvar(2, 1, "a"))]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(y, b), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(1, b, k), (3, a, k)] */
/* //      vars=[(0, y, b#1), (2, x, a#3)] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, 1, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(0, 3, "b")), */
/*        ) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(1, "b", k), (3, "a", k)]), */
/*          test_vars([ */
/*            (0, "y", tyvar(1, 3, "b")), */
/*            (2, "x", tyvar(3, 1, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(y, a), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(1, b, k), (3, a, k)] */
/* //      vars=[(0, y, a#3), (2, x, a#3] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, 1, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(1, 3, "a")), */
/*        ) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(1, "b", k), (3, "a", k)]), */
/*          test_vars([ */
/*            (0, "y", tyvar(3, 3, "a")), */
/*            (2, "x", tyvar(3, 3, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(z, a), (c, k), (y, a), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(1, c, k), (3, b, k), (5, a, k)] */
/* //      vars=[(0, z, a#5), (2, y, a#5), (4, x, a#5)] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, 1, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(1, 3, "a")) */
/*          $++ add_tyvar("c", k) */
/*          $++ add_var("z", tyvar(2, 5, "a")), */
/*        ) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(1, "c", k), (3, "b", k), (5, "a", k)]), */
/*          test_vars([ */
/*            (0, "z", tyvar(5, 5, "a")), */
/*            (2, "y", tyvar(5, 3, "a")), */
/*            (4, "x", tyvar(5, 1, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(z, c), (c, k), (y, b), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(1, c, k), (3, b, k), (5, a, k)] */
/* //      vars=[(0, z, c#1), (2, y, b#3), (4, x, a#5)] */
/* let%test _ = */
/*   ContextMonad.Infix.( */
/*     Context.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, 1, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(0, 3, "b")) */
/*          $++ add_tyvar("c", k) */
/*          $++ add_var("z", tyvar(0, 5, "c")), */
/*        ) */
/*     |> pairing(Context.tyvars, Context.vars) */
/*     |> product( */
/*          test_tyvars_abs([(1, "c", k), (3, "b", k), (5, "a", k)]), */
/*          test_vars([ */
/*            (0, "z", tyvar(1, 5, "c")), */
/*            (2, "y", tyvar(3, 3, "b")), */
/*            (4, "x", tyvar(5, 1, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
