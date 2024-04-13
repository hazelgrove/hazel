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
  | Val
  | Ap
  | If;

let rules: QCheck.Gen.t(rule) =
  frequency([(1, pure(Val)), (1, pure(Ap)), (1, pure(If))]);

let utyp = (u: UTyp.term): UTyp.t => {ids: [Id.mk()], term: u};
let upat = (u: UPat.term): UPat.t => {ids: [Id.mk()], term: u};
let uexp = (u: UExp.term): UExp.t => {ids: [Id.mk()], term: u};

let gen1 = QCheck.Gen.generate1;

let printable_string = string_size(~gen=printable, int_range(1, 8));

let var_name = (): Var.t => printable_string |> gen1;

//UTyp helpers
let utyp_list = (ty: UTyp.t): UTyp.t => List(ty) |> utyp;
let utyp_arrow = (ty1: UTyp.t, ty2: UTyp.t): UTyp.t =>
  Arrow(ty1, ty2) |> utyp;

//UPat helpers
let upat_var = (name: Var.t): UPat.t => Var(name) |> upat;
let upat_typeann = (name: Var.t, ty: UTyp.t): UPat.t =>
  TypeAnn(upat_var(name), ty) |> upat;

//UExp helpers
let uexp_parens = (u: UExp.t): UExp.t => uexp(Parens(u));
let uexp_binop = (op: UExp.op_bin, u1: UExp.t, u2: UExp.t): UExp.t =>
  BinOp(op, u1, u2) |> uexp;
let uexp_int = (x: int): UExp.t => Int(x) |> uexp;
let uexp_bool = (x: bool): UExp.t => Bool(x) |> uexp;
let uexp_float = (x: float): UExp.t => Float(x) |> uexp;
let uexp_string = (x: string): UExp.t => String(x) |> uexp;
let uexp_fun = (name: Var.t, ty: UTyp.t, u: UExp.t): UExp.t =>
  Fun(upat_typeann(name, ty), u) |> uexp;
let uexp_var = (name: Var.t): UExp.t => Var(name) |> uexp;

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
          (2, map2(utyp_arrow, map(utyp_list, self(n / 2)), self(n / 2))),
        ])
      }
    ),
  );

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
          (1, map2(uexp_binop(Int(Plus)), self(n / 2), self(n / 2))),
          (1, find_var(ctx, Int, map(uexp_int, int))),
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
            map2(
              uexp_binop(Int(LessThan)),
              uexp_int_gen(ctx),
              uexp_int_gen(ctx),
            ),
          ),
          (1, find_var(ctx, Bool, map(uexp_bool, bool))),
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
          (1, map2(uexp_binop(Float(Plus)), self(n / 2), self(n / 2))),
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

let rec uexp_val_gen = (ctx: Ctx.t, ty: UTyp.t): UExp.t => {
  switch (ty.term) {
  | Int => uexp_int_gen(ctx) |> gen1
  | Float => uexp_float_gen(ctx) |> gen1
  | Bool => uexp_bool_gen(ctx) |> gen1
  | String => uexp_string_gen(ctx) |> gen1
  | Arrow(ty1, ty2) =>
    let name = var_name();
    let entry =
      Ctx.VarEntry({name, id: Id.invalid, typ: UTyp.to_typ(ctx, ty1)});
    uexp_fun(name, ty1, uexp_val_gen(Ctx.extend(ctx, entry), ty2));
  | List(ty) =>
    let length = int_range(0, 5) |> gen1;
    let rec uexp_list_helper = (length: int, acc: list(UExp.t)) => {
      switch (length) {
      | 0 => acc
      | m => uexp_list_helper(m - 1, [uexp_val_gen(ctx, ty), ...acc])
      };
    };
    ListLit(uexp_list_helper(length, [])) |> uexp;
  | _ => EmptyHole |> uexp |> pure |> gen1
  };
};

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

and uexp_ap_gen = (ctx: Ctx.t, ty: UTyp.t, fn: UExp.t, n: int): UExp.t => {
  switch (ty.term) {
  | Arrow(ty1, ty2) =>
    let arg = uexp_gen(ctx, ty1, n / 2);
    uexp_ap_gen(ctx, ty2, uexp(Ap(fn, arg)), n);
  | _ => fn
  };
}

/*and uexp_match_gen = (ctx: Ctx.t, ty: UTyp.t, fn: UExp.t, n: int): UExp.t => {
    EmptyHole |> uexp; //First generate the guard from an arbitrary type
                     //Second create a list of pats that are based on the arbitrary type
  }*/

and uexp_gen = (ctx: Ctx.t, ty: UTyp.t, n: int): UExp.t => {
  switch (n) {
  | 0 => uexp_val_gen(ctx, ty)
  | n =>
    let rule = rules |> gen1;
    switch (rule) {
    | Val => uexp_val_gen(ctx, ty)
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
    };
  };
};
