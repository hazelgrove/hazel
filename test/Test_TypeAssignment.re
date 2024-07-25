open Alcotest;
open QCheck.Gen;
open Haz3lcore;
open Haz3lcore.Term;

let rec is_output_typ = (ty_fun: Typ.t, ty: Typ.t): bool => {
  switch (ty_fun) {
  | Arrow(_, ty2) => Typ.eq(ty2, ty) ? true : is_output_typ(ty2, ty)
  | _ => false
  };
};

module Ctx = {
  include TypBase.Ctx;

  //Get a list of all the variables in ctx which have type typ
  let list_typ = (ctx: t, typ: Typ.t): list(Var.t) =>
    List.fold_left(
      (acc, entry) =>
        switch (entry) {
        | VarEntry(v) when v.typ == typ => [v.name, ...acc]
        | _ => acc
        },
      [],
      ctx,
    );

  //Get a list of all the variables for functions in ctx which
  //have an output type of typ
  let list_fun_typ = (ctx: t, typ: Typ.t): list(Var.t) => {
    List.fold_left(
      (acc, entry) =>
        switch (entry) {
        | VarEntry(v) when is_output_typ(v.typ, typ) => [v.name, ...acc]
        | _ => acc
        },
      [],
      ctx,
    );
  };
};

type rule =
  | Base
  | Ap
  | If
  | Match;

type val_var =
  | Val
  | Var;

type list_rule =
  | ListLit
  | Cons;

//Helper random generators using the QCheck library
let rules: QCheck.Gen.t(rule) =
  frequency([
    (1, pure(Base)),
    (1, pure(Ap)),
    (1, pure(If)),
    (1, pure(Match)),
  ]);

let val_or_var: QCheck.Gen.t(val_var) =
  frequency([(1, pure(Val)), (1, pure(Var))]);

let list_rules: QCheck.Gen.t(list_rule) =
  frequency([(1, pure(ListLit)), (1, pure(Cons))]);

let utyp = (u: UTyp.term): UTyp.t => {ids: [Id.mk()], term: u};
let upat = (u: UPat.term): UPat.t => {ids: [Id.mk()], term: u};
let uexp = (u: UExp.term): UExp.t => {ids: [Id.mk()], term: u};

let gen1 = QCheck.Gen.generate1;

let printable_string = string_size(~gen=printable, int_range(1, 8));

let var_name = (): Var.t => printable_string |> gen1;

let binop = (op: UExp.op_bin): QCheck.Gen.t(UExp.op_bin) => pure(op);

let int_int_binop: QCheck.Gen.t(UExp.op_bin) =
  frequency([
    (1, binop(Int(Plus))),
    (1, binop(Int(Minus))),
    (1, binop(Int(Times))),
    (1, binop(Int(Power))),
    (1, binop(Int(Divide))),
  ]);

let int_bool_binop: QCheck.Gen.t(UExp.op_bin) =
  frequency([
    (1, binop(Int(LessThan))),
    (1, binop(Int(LessThanOrEqual))),
    (1, binop(Int(GreaterThan))),
    (1, binop(Int(GreaterThanOrEqual))),
    (1, binop(Int(Equals))),
    (1, binop(Int(NotEquals))),
  ]);

let bool_bool_binop: QCheck.Gen.t(UExp.op_bin) =
  frequency([(1, binop(Bool(And))), (1, binop(Bool(Or)))]);

let float_float_binop: QCheck.Gen.t(UExp.op_bin) =
  frequency([
    (1, binop(Float(Plus))),
    (1, binop(Float(Minus))),
    (1, binop(Float(Times))),
    (1, binop(Float(Power))),
    (1, binop(Float(Divide))),
  ]);

let float_bool_binop: QCheck.Gen.t(UExp.op_bin) =
  frequency([
    (1, binop(Float(LessThan))),
    (1, binop(Float(LessThanOrEqual))),
    (1, binop(Float(GreaterThan))),
    (1, binop(Float(GreaterThanOrEqual))),
    (1, binop(Float(Equals))),
    (1, binop(Float(NotEquals))),
  ]);

//UTyp helpers
let utyp_list = (ty: UTyp.t) => List(ty) |> utyp;
let utyp_arrow = (ty1: UTyp.t, ty2: UTyp.t) => Arrow(ty1, ty2) |> utyp;

//UPat helpers
let upat_int = (x: int) => Int(x) |> upat;
let upat_bool = (x: bool) => Bool(x) |> upat;
let upat_float = (x: float) => Float(x) |> upat;
let upat_string = (x: string) => String(x) |> upat;
let upat_list = (l: list(UPat.t)) => ListLit(l) |> upat;
let upat_var = (name: Var.t) => Var(name) |> upat;
let upat_typeann = (name: Var.t, ty: UTyp.t) =>
  TypeAnn(upat_var(name), ty) |> upat;

//UExp helpers
let uexp_parens = (u: UExp.t) => uexp(Parens(u));
let uexp_binop = (op: UExp.op_bin, u1: UExp.t, u2: UExp.t) =>
  BinOp(op, u1, u2) |> uexp;
let uexp_int = (x: int) => Int(x) |> uexp;
let uexp_bool = (x: bool) => Bool(x) |> uexp;
let uexp_float = (x: float) => Float(x) |> uexp;
let uexp_string = (x: string) => String(x) |> uexp;
let uexp_fun = (name: Var.t, ty: UTyp.t, u: UExp.t) =>
  Fun(upat_typeann(name, ty), u) |> uexp;
let uexp_var = (name: Var.t) => Var(name) |> uexp;
let uexp_list = (l: list(UExp.t)) => ListLit(l) |> uexp;
let uexp_ap = (u1: UExp.t, u2: UExp.t) => Ap(u1, u2) |> uexp;
let uexp_if = (u1: UExp.t, u2: UExp.t, u3: UExp.t) => If(u1, u2, u3) |> uexp;
let uexp_match = (u1: UExp.t, l: list((UPat.t, UExp.t))) =>
  Match(u1, l) |> uexp;

//Get variable of certain type if it exists
let find_var =
    (ctx: Ctx.t, typ: Typ.t, default: QCheck.Gen.t(UExp.t))
    : QCheck.Gen.t(UExp.t) => {
  let l = Ctx.list_typ(ctx, typ);
  List.length(l) == 0 ? default : map(uexp_var, oneofl(l));
};

//Get function where the output type is ty if it exists
let find_fn =
    (ctx: Ctx.t, typ: Typ.t, default: QCheck.Gen.t(UExp.t))
    : QCheck.Gen.t(UExp.t) => {
  let l = Ctx.list_fun_typ(ctx, typ);
  List.length(l) == 0 ? default : map(uexp_var, oneofl(l));
};

//Generate a random arrow type
//Combinators such as fix and int_range are
//explained in the QCheck documentation
let utyp_arrow_gen = (ty: UTyp.t): QCheck.Gen.t(UTyp.t) =>
  QCheck.Gen.sized_size(
    int_range(1, 4),
    fix((self, n) =>
      switch (n) {
      | 0 => pure(ty)
      | n =>
        frequency([
          (1, map2(utyp_arrow, self(n / 2), self(n / 2))),
          (2, map2(utyp_arrow, pure(utyp(Int)), self(n / 2))),
          (2, map2(utyp_arrow, pure(utyp(Bool)), self(n / 2))),
          (2, map2(utyp_arrow, pure(utyp(Float)), self(n / 2))),
          (2, map2(utyp_arrow, pure(utyp(String)), self(n / 2))),
          (1, map2(utyp_arrow, map(utyp_list, self(n / 2)), self(n / 2))),
        ])
      }
    ),
  );

//Generates the type of the scrutinized expression in a match statement
let utyp_match_gen: QCheck.Gen.t(UTyp.t) =
  QCheck.Gen.sized_size(
    int_range(1, 2),
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, pure(utyp(Int))),
          (1, pure(utyp(Bool))),
          (1, pure(utyp(Float))),
          (1, pure(utyp(String))),
        ])
      | n =>
        frequency([(1, self(n / 2)), (1, map(utyp_list, self(n / 2)))])
      }
    ),
  );

//Generate a Hazel list UPat
let rec upat_list_gen =
        (entries: list(Ctx.entry), ty: UTyp.t, n: int)
        : (list(Ctx.entry), UPat.t) => {
  let rule = n == 0 ? ListLit : list_rules |> gen1;
  switch (rule) {
  | ListLit =>
    let length = int_range(0, 3) |> gen1;
    let rec upat_list_helper =
            (length: int, entry_acc: list(Ctx.entry), pat_acc: list(UPat.t))
            : (list(Ctx.entry), list(UPat.t)) => {
      switch (length) {
      | 0 => (entry_acc, pat_acc)
      | m =>
        let (l, p) = upat_gen(entry_acc, ty, n / 2);
        upat_list_helper(m - 1, l, [p, ...pat_acc]);
      };
    };
    let (entries, pat_list) = upat_list_helper(length, entries, []);
    (entries, upat(ListLit(pat_list)));
  | Cons =>
    let (entries, hd) = upat_gen(entries, ty, n / 2);
    let (entries, tl) = upat_list_gen(entries, ty, n / 2);
    (entries, upat(Cons(hd, tl)));
  };
}

//Generate a UPat
and upat_gen =
    (entries: list(Ctx.entry), ty: UTyp.t, n: int)
    : (list(Ctx.entry), UPat.t) => {
  let v = val_or_var |> gen1;
  switch (v) {
  | Val =>
    switch (ty.term) {
    | Int => (entries, gen1(int) |> upat_int)
    | Bool => (entries, gen1(bool) |> upat_bool)
    | Float => (entries, gen1(float) |> upat_float)
    | String => (entries, gen1(printable_string) |> upat_string)
    | List(ty) => upat_list_gen(entries, ty, n / 2)
    | _ => ([], EmptyHole |> upat)
    }
  | Var =>
    let name = var_name();
    let entry =
      Ctx.VarEntry({name, id: Id.invalid, typ: UTyp.to_typ([], ty)});
    ([entry, ...entries], name |> upat_var);
  };
};

//Generate integer expression
let uexp_int_gen = (ctx: Ctx.t): QCheck.Gen.t(UExp.t) =>
  QCheck.Gen.sized_size(
    small_nat,
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(uexp_int, int)),
          (1, find_var(ctx, Int, map(uexp_int, int))),
        ])
      | n =>
        frequency([
          (1, map(uexp_int, int)),
          (1, map(uexp_parens, self(n / 2))),
          (1, map3(uexp_binop, int_int_binop, self(n / 2), self(n / 2))),
          (1, find_var(ctx, Int, map(uexp_int, int))),
        ])
      }
    ),
  );

let uexp_float_gen = (ctx: Ctx.t): QCheck.Gen.t(UExp.t) =>
  QCheck.Gen.sized_size(
    small_nat,
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(uexp_float, float)),
          (1, find_var(ctx, Float, map(uexp_float, float))),
        ])
      | n =>
        frequency([
          (1, map(uexp_float, float)),
          (1, map(uexp_parens, self(n / 2))),
          (
            1,
            map3(uexp_binop, float_float_binop, self(n / 2), self(n / 2)),
          ),
          (1, find_var(ctx, Float, map(uexp_float, float))),
        ])
      }
    ),
  );

let uexp_string_gen = (ctx: Ctx.t): QCheck.Gen.t(UExp.t) =>
  QCheck.Gen.sized_size(
    small_nat,
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(uexp_string, printable_string)),
          (1, find_var(ctx, String, map(uexp_string, printable_string))),
        ])
      | n =>
        frequency([
          (1, map(uexp_string, printable_string)),
          (1, map(uexp_parens, self(n / 2))),
          (
            1,
            map2(uexp_binop(String(Concat)), self(n / 2), self(n / 2)),
          ),
          (1, find_var(ctx, String, map(uexp_string, printable_string))),
        ])
      }
    ),
  );

let uexp_bool_gen = (ctx: Ctx.t): QCheck.Gen.t(UExp.t) =>
  QCheck.Gen.sized_size(
    small_nat,
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(uexp_bool, bool)),
          (1, find_var(ctx, Bool, map(uexp_bool, bool))),
        ])
      | n =>
        frequency([
          (1, map(uexp_bool, bool)),
          (1, map(uexp_parens, self(n / 2))),
          (
            1,
            map3(
              uexp_binop,
              int_bool_binop,
              uexp_int_gen(ctx),
              uexp_int_gen(ctx),
            ),
          ),
          (
            1,
            map3(
              uexp_binop,
              float_bool_binop,
              uexp_float_gen(ctx),
              uexp_float_gen(ctx),
            ),
          ),
          (1, map3(uexp_binop, bool_bool_binop, self(n / 2), self(n / 2))),
          (1, find_var(ctx, Bool, map(uexp_bool, bool))),
        ])
      }
    ),
  );

//Base expressions are generated when the bound has been reached
let rec uexp_base_gen = (ctx: Ctx.t, ty: UTyp.t): UExp.t => {
  switch (ty.term) {
  | Int => uexp_int_gen(ctx) |> gen1
  | Float => uexp_float_gen(ctx) |> gen1
  | Bool => uexp_bool_gen(ctx) |> gen1
  | String => uexp_string_gen(ctx) |> gen1
  | Arrow(ty1, ty2) =>
    let name = var_name();
    let entry =
      Ctx.VarEntry({name, id: Id.invalid, typ: UTyp.to_typ(ctx, ty1)});
    uexp_fun(name, ty1, uexp_base_gen(Ctx.extend(ctx, entry), ty2));
  | List(ty) =>
    let length = int_range(1, 5) |> gen1;
    let rec uexp_list_helper = (length: int, acc: list(UExp.t)) => {
      switch (length) {
      | 0 => acc
      | m => uexp_list_helper(m - 1, [uexp_base_gen(ctx, ty), ...acc])
      };
    };
    uexp_list_helper(length, []) |> uexp_list;
  | _ => EmptyHole |> uexp
  };
};

//Generate a function with bound n
let rec uexp_fun_gen = (ctx: Ctx.t, ty: UTyp.t, n: int): UExp.t => {
  switch (ty.term) {
  | Arrow(ty1, ty2) =>
    let name = var_name();
    let entry =
      Ctx.VarEntry({name, id: Id.invalid, typ: UTyp.to_typ(ctx, ty1)});
    uexp_fun(name, ty1, uexp_fun_gen(Ctx.extend(ctx, entry), ty2, n));
  | _ => uexp_gen(ctx, ty, n / 2)
  };
}

//Generate an ap expression with bound n
and uexp_ap_gen = (ctx: Ctx.t, ty: UTyp.t, fn: UExp.t, n: int): UExp.t => {
  switch (ty.term) {
  | Arrow(ty1, ty2) =>
    let arg = uexp_gen(ctx, ty1, n / 2);
    uexp_ap_gen(ctx, ty2, uexp(Ap(fn, arg)), n);
  | _ => fn
  };
}

//Generate match expression with bound n
and uexp_match_gen = (ctx: Ctx.t, ty: UTyp.t, n: int): UExp.t => {
  let scrut_ty = utyp_match_gen |> gen1;
  let scrut = uexp_gen(ctx, scrut_ty, n / 2);

  let length = int_range(1, 4) |> gen1;
  let rec uexp_match_helper =
          (length: int, acc: list((UPat.t, UExp.t)))
          : list((UPat.t, UExp.t)) => {
    switch (length) {
    | 0 => acc
    | m =>
      let (entries, p) = upat_gen([], scrut_ty, n / 2);
      let ctx' =
        List.fold_left(
          (ctx, entry) => Ctx.extend(ctx, entry),
          ctx,
          entries,
        );
      let u = uexp_gen(ctx', ty, n / 2);
      uexp_match_helper(m - 1, [(p, u), ...acc]);
    };
  };
  let l = uexp_match_helper(length, []);
  Match(scrut, l) |> uexp;
}

//Generates a UExp with bound n
//if n is 0, then generate a base expression
and uexp_gen = (ctx: Ctx.t, ty: UTyp.t, n: int): UExp.t => {
  let rule = n == 0 ? Base : rules |> gen1;
  switch (rule) {
  | Base => uexp_base_gen(ctx, ty)
  | Ap =>
    let fn_typ = utyp_arrow_gen(ty) |> gen1;
    let fn = uexp_fun_gen(ctx, fn_typ, n / 2);
    uexp_ap_gen(ctx, fn_typ, fn, n / 2);
  | If =>
    If(
      uexp_gen(ctx, utyp(Bool), n / 2),
      uexp_gen(ctx, ty, n / 2),
      uexp_gen(ctx, ty, n / 2),
    )
    |> uexp
  | Match => uexp_match_gen(ctx, ty, n / 2)
  };
};

let pure_random_utyp: QCheck.Gen.t(UTyp.t) =
  QCheck.Gen.sized_size(
    int_range(0, 1),
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, pure(utyp(Int))),
          (1, pure(utyp(Bool))),
          (1, pure(utyp(Float))),
          (1, pure(utyp(String))),
        ])
      | n =>
        frequency([
          (1, map2(utyp_arrow, self(n / 2), self(n / 2))),
          (1, map(utyp_list, self(n / 2))),
        ])
      }
    ),
  );

let pure_random_upat: QCheck.Gen.t(UPat.t) =
  QCheck.Gen.sized_size(
    int_range(1, 2),
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(upat_int, int)),
          (1, map(upat_bool, bool)),
          (1, map(upat_float, float)),
          (1, map(upat_string, printable_string)),
          (1, map(upat_var, printable_string)),
        ])
      | n =>
        frequency([
          (1, self(n / 2)),
          (1, map(upat_list, list_size(int_range(1, 2), self(n / 2)))),
        ])
      }
    ),
  );

//Generates a UExp with no regard to the typing rules
let pure_random_uexp: QCheck.Gen.t(UExp.t) =
  QCheck.Gen.sized_size(
    int_range(1, 5),
    fix((self, n) =>
      switch (n) {
      | 0 =>
        frequency([
          (1, map(uexp_int, int)),
          (1, map(uexp_bool, bool)),
          (1, map(uexp_float, float)),
          (1, map(uexp_string, printable_string)),
          (1, map(uexp_var, printable_string)),
        ])
      | n =>
        frequency([
          (1, map3(uexp_binop, int_int_binop, self(n / 2), self(n / 2))),
          (1, map3(uexp_binop, int_bool_binop, self(n / 2), self(n / 2))),
          (
            1,
            map3(uexp_binop, float_float_binop, self(n / 2), self(n / 2)),
          ),
          (
            1,
            map3(uexp_binop, float_bool_binop, self(n / 2), self(n / 2)),
          ),
          (1, map3(uexp_binop, bool_bool_binop, self(n / 2), self(n / 2))),
          (
            1,
            map3(uexp_fun, printable_string, pure_random_utyp, self(n / 2)),
          ),
          (1, map(uexp_list, list_size(int_range(1, 5), self(n / 2)))),
          (1, map2(uexp_ap, self(n / 2), self(n / 2))),
          (1, map3(uexp_if, self(n / 2), self(n / 2), self(n / 2))),
          (
            1,
            map2(
              uexp_match,
              self(n / 2),
              list_size(
                int_range(1, 4),
                pair(pure_random_upat, self(n / 2)),
              ),
            ),
          ),
        ])
      }
    ),
  );

//Generates alcotest test cases
let rec testcases_gen =
        (size: int, ty: UTyp.t, l: list(test_case(unit)))
        : list(test_case(unit)) => {
  switch (size) {
  | 0 => l
  | n =>
    let u = uexp_gen([], ty, 2);
    let m = Test_Elaboration.mk_map(u);
    let d = Elaborator.dhexp_of_uexp(m, u, false);
    let test = () =>
      Alcotest.check(
        Alcotest.bool,
        "Random expression",
        true,
        TypeAssignment.property_test(Some(Term.UTyp.to_typ([], ty)), d, m),
      );
    let case = test_case("Type assignment", `Quick, test);
    testcases_gen(n - 1, ty, [case, ...l]);
  };
};

let type_assignment_tests = testcases_gen(10, utyp(Int), []);

//Generate purely random uexp test cases for alcotest
let random_tests =
  List.map(
    (u: UExp.t) => {
      let m = Test_Elaboration.mk_map(u);
      let d = Elaborator.dhexp_of_uexp(m, u, false);
      let ty = Elaborator.fixed_exp_typ(m, u);
      let test = () =>
        Alcotest.check(
          Alcotest.bool,
          "Random expression",
          true,
          TypeAssignment.property_test(ty, d, m),
        );
      test_case("Type assignment", `Quick, test);
    },
    QCheck.Gen.generate(~n=10, pure_random_uexp),
  );
