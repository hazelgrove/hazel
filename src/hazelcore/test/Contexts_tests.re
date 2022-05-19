/* open Context.onad; */
/* let verbose = true; */
/* let test_vars = */
/*     ( */
/*       want: list((int, Var.t, HTyp.t)), */
/*       got: list((Index.Abs.t, Var.t, HTypSyntax.t(Index.absolute))), */
/*     ) */
/*     : bool => { */
/*   if (verbose) { */
/*     print_endline("\nWANT VARS:"); */
/*     print_endline( */
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
/*     print_endline("GOT VARS:"); */
/*     print_endline( */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((idx, x, ty)) => */
/*             Sexplib.Sexp.List([ */
/*               Index.Abs.sexp_of_t(idx), */
/*               Var.sexp_of_t(x), */
/*               HTypSyntax.sexp_of_t(Index.sexp_of_absolute, ty), */
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
/*       && HTypSyntax.equal(HTyp.unsafe(want_ty), got_ty), */
/*     want, */
/*     got, */
/*   ); */
/* }; */
/* let test_tyvars = */
/*     ( */
/*       want: list((int, TyVar.t, Kind.t)), */
/*       got: list((Index.Abs.t, TyVar.t, KindCore.t(Index.absolute))), */
/*     ) */
/*     : bool => { */
/*   if (verbose) { */
/*     print_endline("\nWANT TYVARS:"); */
/*     print_endline( */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((i, t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               Sexplib.Std.sexp_of_int(i), */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_t(Index.sexp_of_absolute, k), */
/*             ]), */
/*           want, */
/*         ), */
/*       ), */
/*     ); */
/*     print_endline("GOT TYVARS:"); */
/*     print_endline( */
/*       Sexplib.Sexp.to_string_hum( */
/*         Sexplib.Std.sexp_of_list( */
/*           ((idx, t, k)) => */
/*             Sexplib.Sexp.List([ */
/*               Index.Abs.sexp_of_t(idx), */
/*               TyVar.sexp_of_t(t), */
/*               Kind.sexp_of_t(Index.sexp_of_absolute, k), */
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
/*       && KindCore.equal(want_k, got_k), */
/*     want, */
/*     got, */
/*   ); */
/* }; */
/* let k = Kind.singleton(HTyp.int); */
/* let tyvar = (i: int, name: string): HTyp.t => */
/*   HTyp.tyvar(Index.Abs.of_int(i), name); */
/* let pairing = (f: 'a => 'b, g: 'a => 'c, x: 'a): ('b, 'c) => (f(x), g(x)); */
/* let product = (f: 'a => 'b, g: 'c => 'd, (x: 'a, y: 'c)): ('b, 'd) => ( */
/*   f(x), */
/*   g(y), */
/* ); */
/* let meet = ((x: bool, y: bool)): bool => x && y; */
/* // TODO: (eric) implement index shifting at type abstraction boundaries */
/* // [("a", Singleton(Rec.@0 + @1)), -1-] */
/* // TODO: (eric) make sure there are no unbound type variables in anything exported from Context */
/* /\* /\\* The initial context has no bindings. *\\/ *\/ */
/* /\* let%test _ = *\/ */
/* /\*   Context.initial |>  *\/ */
/* /\* // [(a, k)] *\/ */
/* /\* // ~> tyvars=[(0, a, k)] *\/ */
/* /\* //      vars=[] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   Context.initial *\/ */
/* /\*   |> exec(add_tyvar("a", k)) *\/ */
/* /\*   |> pairing(Context.tyvars, Context.vars) *\/ */
/* /\*   |> product(test_tyvars([(0, "a", k)]), test_vars([])) *\/ */
/* /\*   |> meet; *\/ */
/* /\* // [(x, a), (a, k)] *\/ */
/* /\* // ~> tyvars=[(0, a, k)] *\/ */
/* /\* //      vars=[(0, x, a:0)] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   Context.onad.Infix.( *\/ */
/* /\*     Context.initial *\/ */
/* /\*     |> exec(add_tyvar("a", k) $++ add_var("x", tyvar(0, "a"))) *\/ */
/* /\*     |> pairing(Context.tyvars, Contexts.vars) *\/ */
/* /\*     |> product( *\/ */
/* /\*          test_tyvars([(0, "a", k)]), *\/ */
/* /\*          test_vars([(0, "x", tyvar(0, "a"))]), *\/ */
/* /\*        ) *\/ */
/* /\*     |> meet *\/ */
/* /\*   ); *\/ */
/* /\* */
/*   [(a, k)] */
/*   ~> (x : a#0) [(x, a#0), (a, k)] */
/*   [(y, ty), (a, k)] */
/*   ~> (x : a#1) [(x, a#1), (y, ty), (a, k)] */
/*   [(b, k), (a, k)] */
/*   ~> (x : a#1) [(x, a#1), (b, k), (a, k)] */
/*   [(b, k), (a, k)] */
/*   ~> (x : b#0) [(x, b#0), (b, k), (a, k)] */
/*    [b x:a0 a]  -->  x : a|0+1+1=2| */
/*  i= 0 1    2 */
/*    [y:? b x:a0 a]  -->  x : a|0+2+1=3| */
/*  i= 0   1 2    3 */
/*    [c b x:a0 a]  -->  x : a|0+2+1=3| */
/*  i= 0 1 2    3 */
/*   *\/ */
/* // [(b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, b, k), (1, a, k)] */
/* //      vars=[(0, x, a:1)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, "a")) */
/*          $++ add_tyvar("b", k), */
/*        ) */
/*     |> pairing(Context.tyvars, Contexts.vars) */
/*     |> product( */
/*          test_tyvars([(0, "b", k), (1, "a", k)]), */
/*          test_vars([(0, "x", tyvar(1, "a"))]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(y, b), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, b, k), (1, a, k)] */
/* //      vars=[(0, y, b:0), (1, x, a:1)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(0, "b")), */
/*        ) */
/*     |> pairing(Context.tyvars, Contexts.vars) */
/*     |> product( */
/*          test_tyvars([(0, "b", k), (1, "a", k)]), */
/*          test_vars([(0, "y", tyvar(0, "b")), (1, "x", tyvar(1, "a"))]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(y, a), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, b, k), (1, a, k)] */
/* //      vars=[(0, y, a:1), (1, x, a:1] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(1, "a")), */
/*        ) */
/*     |> pairing(Context.tyvars, Contexts.vars) */
/*     |> product( */
/*          test_tyvars([(0, "b", k), (1, "a", k)]), */
/*          test_vars([(0, "y", tyvar(1, "a")), (1, "x", tyvar(1, "a"))]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(z, a), (c, k), (y, a), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, c, k), (1, b, k), (2, a, k)] */
/* //      vars=[(0, z, a:2), (1, y, a:2), (2, x, a:2)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(1, "a")) */
/*          $++ add_tyvar("c", k) */
/*          $++ add_var("z", tyvar(2, "a")), */
/*        ) */
/*     |> pairing(Context.tyvars, Contexts.vars) */
/*     |> product( */
/*          test_tyvars([(0, "c", k), (1, "b", k), (2, "a", k)]), */
/*          test_vars([ */
/*            (0, "z", tyvar(2, "a")), */
/*            (1, "y", tyvar(2, "a")), */
/*            (2, "x", tyvar(2, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(z, c), (c, k), (y, b), (b, k), (x, a), (a, k)] */
/* // ~> tyvars=[(0, c, k), (1, b, k), (2, a, k)] */
/* //      vars=[(0, z, c:0), (1, y, b:1), (2, x, a:2)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_var("x", tyvar(0, "a")) */
/*          $++ add_tyvar("b", k) */
/*          $++ add_var("y", tyvar(0, "b")) */
/*          $++ add_tyvar("c", k) */
/*          $++ add_var("z", tyvar(0, "c")), */
/*        ) */
/*     |> pairing(Context.tyvars, Contexts.vars) */
/*     |> product( */
/*          test_tyvars([(0, "c", k), (1, "b", k), (2, "a", k)]), */
/*          test_vars([ */
/*            (0, "z", tyvar(0, "c")), */
/*            (1, "y", tyvar(1, "b")), */
/*            (2, "x", tyvar(2, "a")), */
/*          ]), */
/*        ) */
/*     |> meet */
/*   ); */
/* // [(c, k), (b, k), (a, k)] */
/* // ~> tyvars=[(0, c, k), (1, b, k), (2, a, k)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec(add_tyvar("a", k) $++ add_tyvar("b", k) $++ add_tyvar("c", k)) */
/*     |> Context.tyvars */
/*     |> test_tyvars([(0, "c", k), (1, "b", k), (2, "a", k)]) */
/*   ); */
/* // [(b, S(a)), (a, k)] */
/* // ~> tyvars=[(0, b, S(a:1)), (1, a, k)] */
/* let%test _ = */
/*   ContextsMonad.Infix.( */
/*     Contexts.initial */
/*     |> exec( */
/*          add_tyvar("a", k) */
/*          $++ add_tyvar("b", Kind.singleton(tyvar(0, "a"))), */
/*        ) */
/*     |> Context.tyvars */
/*     |> test_tyvars([(0, "b", Kind.singleton(tyvar(1, "a"))), (1, "a", k)]) */
/*   ); */
/* /\* // [(c, S(a)), (b, k), (a, k)]  -->  [(c, S(a)), (a, k)] *\/ */
/* /\* // ~> tyvars=[(0, c, S(a:1)), (1, a, k)] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   ContextsMonad.Infix.( *\/ */
/* /\*     Contexts.initial *\/ */
/* /\*     |> exec( *\/ */
/* /\*          add_tyvar("a", k) *\/ */
/* /\*          $++ add_tyvar("b", k) *\/ */
/* /\*          $++ add_tyvar("c", Kind.singleton(tyvar(1, "a"))) *\/ */
/* /\*          $++ remove_tyvar(Index.Abs.of_int(1), HTyp.unsafe(HTyp.hole)), *\/ */
/* /\*        ) *\/ */
/* /\*     |> Context.tyvars *\/ */
/* /\*     |> test_tyvars([(0, "c", Kind.singleton(tyvar(1, "a"))), (1, "a", k)]) *\/ */
/* /\*   ); *\/ */
/* /\* // [(c, S(b)), (b, k), (a, k)]  -->  [(c, ty_k), (a, k)] *\/ */
/* /\* // ~> tyvars=[(0, c, ty_k), (1, a, k)] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   ContextsMonad.Infix.( *\/ */
/* /\*     Contexts.initial *\/ */
/* /\*     |> exec( *\/ */
/* /\*          add_tyvar("a", k) *\/ */
/* /\*          $++ add_tyvar("b", k) *\/ */
/* /\*          $++ add_tyvar("c", Kind.singleton(tyvar(0, "b"))) *\/ */
/* /\*          $++ remove_tyvar(Index.Abs.of_int(1), HTyp.unsafe(HTyp.hole)), *\/ */
/* /\*        ) *\/ */
/* /\*     |> Context.tyvars *\/ */
/* /\*     |> test_tyvars([(0, "c", Kind.singleton(HTyp.int)), (1, "a", k)]) *\/ */
/* /\*   ); *\/ */
/* /\* // [(x, S(a)), (a, k)]  -->  [(x, ty_k)] *\/ */
/* /\* // ~> tyvars=[] *\/ */
/* /\* //      vars=[(0, x, ty_k)] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   ContextsMonad.Infix.( *\/ */
/* /\*     Contexts.initial *\/ */
/* /\*     |> exec( *\/ */
/* /\*          add_tyvar("a", k) *\/ */
/* /\*          $++ add_var("x", tyvar(0, "a")) *\/ */
/* /\*          $++ remove_tyvar(Index.Abs.of_int(0), HTyp.unsafe(HTyp.hole)), *\/ */
/* /\*        ) *\/ */
/* /\*     |> pairing(Context.tyvars, Contexts.vars) *\/ */
/* /\*     |> product(test_tyvars([]), test_vars([(0, "x", HTyp.hole)])) *\/ */
/* /\*     |> meet *\/ */
/* /\*   ); *\/ */
/* /\* // [(x, a), (b, k), (a, k)]  -->  [(x, a), (a, k)] *\/ */
/* /\* // ~> tyvars=[(0, a, k)]] *\/ */
/* /\* //      vars=[(0, x, a:0)] *\/ */
/* /\* let%test _ = *\/ */
/* /\*   ContextsMonad.Infix.( *\/ */
/* /\*     Contexts.initial *\/ */
/* /\*     |> exec( *\/ */
/* /\*          add_tyvar("a", k) *\/ */
/* /\*          $++ add_tyvar("b", k) *\/ */
/* /\*          $++ add_var("x", tyvar(1, "a")) *\/ */
/* /\*          $++ remove_tyvar(Index.Abs.of_int(0), HTyp.unsafe(HTyp.hole)), *\/ */
/* /\*        ) *\/ */
/* /\*     |> pairing(Context.tyvars, Contexts.vars) *\/ */
/* /\*     |> product( *\/ */
/* /\*          test_tyvars([(0, "a", k)]), *\/ */
/* /\*          test_vars([(0, "x", tyvar(0, "a"))]), *\/ */
/* /\*        ) *\/ */
/* /\*     |> meet *\/ */
/* /\*   ); *\/ */
