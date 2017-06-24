/* Comments below indicate the corresponding rule names from the appendix in
 * the extended version of the POPL 2017 paper: https://arxiv.org/pdf/1607.04180.pdf */
module HTyp = {
  type t =
    | Num
    | Arrow t t
    | Sum t t
    | Hole;
  let rec eq ty1 ty2 =>
    switch (ty1, ty2) {
    | (Num, Num) => true
    | (Arrow ty1_left ty1_right [@implicit_arity], Arrow ty2_left ty2_right [@implicit_arity]) =>
      eq ty1_left ty2_left && eq ty1_right ty2_right
    | (Sum ty1_left ty1_right [@implicit_arity], Sum ty2_left ty2_right [@implicit_arity]) =>
      eq ty1_left ty2_left && eq ty1_right ty2_right
    | (Hole, Hole) => true
    | _ => false
    };
  let rec consistent ty1 ty2 =>
    eq ty1 ty2 || (
      switch (ty1, ty2) {
      | (Hole, _) /* TCHole1 */ => true
      | (_, Hole) /* TCHole2 */ => true
      | (Arrow ty1_left ty1_right [@implicit_arity], Arrow ty2_left ty2_right [@implicit_arity]) /* TCArr */ =>
        consistent ty1_left ty1_right && consistent ty2_left ty2_right
      | (Sum ty1_left ty1_right [@implicit_arity], Sum ty2_left ty2_right [@implicit_arity]) /* 19 */ =>
        consistent ty1_left ty1_right && consistent ty2_left ty2_right
      | _ => eq ty1 ty2 /* TCRefl */
      }
    );
  let inconsistent ty1 ty2 => not (consistent ty1 ty2); /* ICNumArr1, ICNumArr2, ICArr1, ICArr2 */
  let matched_arrow ty =>
    switch ty {
    | Arrow ty1 ty2 [@implicit_arity] /* MAArr */ => Some (ty1, ty2)
    | Hole /* MAHole */ => Some (Hole, Hole)
    | _ => None
    };
  let has_matched_arrow ty =>
    switch (matched_arrow ty) {
    | Some _ => true
    | None => false
    };
  let matched_sum ty =>
    switch ty {
    | Sum ty1 ty2 [@implicit_arity] /* 20b */ => Some (ty1, ty2)
    | Hole /* 20a */ => Some (Hole, Hole)
    | _ => None
    };
  let has_matched_sum ty =>
    switch (matched_sum ty) {
    | Some _ => true
    | None => false
    };
  let rec complete ty =>
    switch ty {
    | Num => true
    | Arrow ty1 ty2 [@implicit_arity] => complete ty1 && complete ty2
    | Sum ty1 ty2 [@implicit_arity] => complete ty1 && complete ty2
    | Hole => false
    };
};

module Var = {
  type t = string;
};

module Ctx: {
  type t;
  let empty: t;
  let extend: t => (Var.t, HTyp.t) => t;
  let lookup: t => Var.t => option HTyp.t;
} = {
  type t = list (Var.t, HTyp.t);
  let empty = [];
  let extend ctx (x, ty) => [(x, ty), ...ctx];
  let rec lookup ctx x =>
    switch ctx {
    | [] => None
    | [(y, ty), ...ctx'] =>
      switch (String.compare x y) {
      | 0 => Some ty
      | _ => lookup ctx' x
      }
    };
};

module HExp = {
  type inj_side =
    | L
    | R;
  let pick_side side l r =>
    switch side {
    | L => l
    | R => r
    };
  type t =
    | Asc t HTyp.t
    | Var Var.t
    | Let Var.t t t
    | Lam Var.t t
    | Ap t t
    | NumLit int
    | Plus t t
    | Inj inj_side t
    | Case t (Var.t, t) (Var.t, t)
    | EmptyHole
    | NonEmptyHole t;
  exception IllTyped;
  let rec syn ctx e =>
    switch e {
    | Asc e' ty [@implicit_arity] /* SAsc */ =>
      let _ = ana ctx e' ty;
      ty
    | Var x /* SVar */ =>
      switch (Ctx.lookup ctx x) {
      | Some ty => ty
      | None => raise IllTyped
      }
    | Let x e1 e2 [@implicit_arity] =>
      let ty1 = syn ctx e1;
      let ctx' = Ctx.extend ctx (x, ty1);
      syn ctx' e2
    | Ap e1 e2 [@implicit_arity] /* SAp */ =>
      let ty1 = syn ctx e1;
      switch (HTyp.matched_arrow ty1) {
      | Some (ty1_left, ty1_right) =>
        let _ = ana ctx e2 ty1_left;
        ty1_right
      | _ => raise IllTyped
      }
    | NumLit i /* SNum */ =>
      if (i < 0) {
        raise IllTyped
      } else {
        HTyp.Num
      }
    | Plus e1 e2 [@implicit_arity] /* 3e */ =>
      let _ = ana ctx e1 HTyp.Num;
      let _ = ana ctx e2 HTyp.Num;
      HTyp.Num
    | EmptyHole /* SHole */ => HTyp.Hole
    | NonEmptyHole e' /* SNEHole */ =>
      let _ = syn ctx e';
      HTyp.Hole
    | _ => raise IllTyped
    }
  and ana ctx e ty =>
    switch e {
    | Let x e1 e2 [@implicit_arity] =>
      let ty1 = syn ctx e1;
      let ctx' = Ctx.extend ctx (x, ty1);
      ana ctx' e2 ty
    | Lam x e' [@implicit_arity] /* ALam */ =>
      switch (HTyp.matched_arrow ty) {
      | Some (ty1, ty2) =>
        let ctx' = Ctx.extend ctx (x, ty1);
        ana ctx' e' ty2
      | _ => raise IllTyped
      }
    | Inj side e' [@implicit_arity] /* 21a */ =>
      switch (HTyp.matched_sum ty) {
      | Some (ty1, ty2) => ana ctx e' (pick_side side ty1 ty2)
      | None => raise IllTyped
      }
    | Case e' (x, e1) (y, e2) [@implicit_arity] /* 21b */ =>
      let e'_ty = syn ctx e';
      switch (HTyp.matched_sum e'_ty) {
      | Some (ty1, ty2) =>
        let ctx1 = Ctx.extend ctx (x, ty1);
        let () = ana ctx1 e1 ty;
        let ctx2 = Ctx.extend ctx (y, ty2);
        ana ctx2 e2 ty
      | _ => raise IllTyped
      }
    | _ /* ASubsume */ =>
      let ty' = syn ctx e;
      if (HTyp.consistent ty ty') {
        ()
      } else {
        raise IllTyped
      }
    };
  let rec complete e =>
    switch e {
    | Asc e' ty [@implicit_arity] => complete e' && HTyp.complete ty
    | Var _ => true
    | Let _ e e' [@implicit_arity] => complete e && complete e'
    | Lam _ e' [@implicit_arity] => complete e'
    | Ap e1 e2 [@implicit_arity] => complete e1 && complete e2
    | NumLit _ => true
    | Plus e1 e2 [@implicit_arity] => complete e1 && complete e2
    | Inj _ e [@implicit_arity] => complete e
    | Case e (x, e1) (y, e2) [@implicit_arity] => complete e && complete e1 && complete e2
    | EmptyHole => false
    | NonEmptyHole _ => false
    };
};

module ZTyp = {
  type t =
    | CursorT HTyp.t
    | LeftArrow t HTyp.t
    | RightArrow HTyp.t t
    | LeftSum t HTyp.t
    | RightSum HTyp.t t;
  let rec erase zty =>
    switch zty {
    | CursorT ty /* ETTop */ => ty
    | LeftArrow zty1 ty2 [@implicit_arity] /* ETArrL */ =>
      HTyp.Arrow (erase zty1) ty2 [@implicit_arity]
    | RightArrow ty1 zty2 [@implicit_arity] /* ETArrR */ =>
      HTyp.Arrow ty1 (erase zty2) [@implicit_arity]
    | LeftSum zty1 ty2 [@implicit_arity] /* (omitted from paper) */ =>
      HTyp.Sum (erase zty1) ty2 [@implicit_arity]
    | RightSum ty1 zty2 [@implicit_arity] /* (omitted from paper) */ =>
      HTyp.Sum ty1 (erase zty2) [@implicit_arity]
    };
};

module ZExp = {
  type t =
    | CursorE HExp.t
    | LeftAsc t HTyp.t
    | RightAsc HExp.t ZTyp.t
    | LetZ1 Var.t t HExp.t
    | LetZ2 Var.t HExp.t t
    | LamZ Var.t t
    | LeftAp t HExp.t
    | RightAp HExp.t t
    | LeftPlus t HExp.t
    | RightPlus HExp.t t
    | InjZ HExp.inj_side t
    | CaseZ1 t (Var.t, HExp.t) (Var.t, HExp.t)
    | CaseZ2 HExp.t (Var.t, t) (Var.t, HExp.t)
    | CaseZ3 HExp.t (Var.t, HExp.t) (Var.t, t)
    | NonEmptyHoleZ t;
  let rec erase ze =>
    switch ze {
    | CursorE e /* EETop */ => e
    | LeftAsc ze' ty [@implicit_arity] /* EEAscL */ => HExp.Asc (erase ze') ty [@implicit_arity]
    | RightAsc e' zty [@implicit_arity] /* EEAscR */ =>
      HExp.Asc e' (ZTyp.erase zty) [@implicit_arity]
    | LetZ1 x ze e [@implicit_arity] => HExp.Let x (erase ze) e [@implicit_arity]
    | LetZ2 x e ze [@implicit_arity] => HExp.Let x e (erase ze) [@implicit_arity]
    | LamZ x ze' [@implicit_arity] /* EELam */ => HExp.Lam x (erase ze') [@implicit_arity]
    | LeftAp ze' e [@implicit_arity] /* EEApL */ => HExp.Ap (erase ze') e [@implicit_arity]
    | RightAp e ze' [@implicit_arity] /* EEApR */ => HExp.Ap e (erase ze') [@implicit_arity]
    | LeftPlus ze' e [@implicit_arity] /* EEPlusL */ => HExp.Plus (erase ze') e [@implicit_arity]
    | RightPlus e ze' [@implicit_arity] /* EEPlusR */ => HExp.Plus e (erase ze') [@implicit_arity]
    | InjZ side ze [@implicit_arity] /* (omitted from paper) */ =>
      HExp.Inj side (erase ze) [@implicit_arity]
    | CaseZ1 ze branch1 branch2 [@implicit_arity] /* (omitted from paper) */ =>
      HExp.Case (erase ze) branch1 branch2 [@implicit_arity]
    | CaseZ2 e (x, ze) branch2 [@implicit_arity] /* (omitted from paper) */ =>
      HExp.Case e (x, erase ze) branch2 [@implicit_arity]
    | CaseZ3 e branch1 (y, ze) [@implicit_arity] /* (omitted from paper) */ =>
      HExp.Case e branch1 (y, erase ze) [@implicit_arity]
    | NonEmptyHoleZ ze' /* EENEHole */ => HExp.NonEmptyHole (erase ze')
    };
};

module Action = {
  type direction =
    | Child int
    | Parent;
  type shape =
    | SArrow
    | SNum
    | SSum
    | SAsc
    | SLet Var.t
    | SVar Var.t
    | SLam Var.t
    | SAp
    | SLit int
    | SPlus
    | SInj HExp.inj_side
    | SCase Var.t Var.t
    | SNEHole;
  type t =
    | Move direction
    | Del
    | Construct shape
    | Finish;
  exception InvalidAction;
  exception Impossible;
  let rec performTyp a zty =>
    switch (a, zty) {
    /* Movement */
    | (Move (Child 1), ZTyp.CursorT (HTyp.Arrow ty1 ty2 [@implicit_arity])) /* TMArrChild1 */ =>
      ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2 [@implicit_arity]
    | (Move (Child 2), ZTyp.CursorT (HTyp.Arrow ty1 ty2 [@implicit_arity])) /* TMArrChild2 */ =>
      ZTyp.RightArrow ty1 (ZTyp.CursorT ty2) [@implicit_arity]
    | (Move Parent, ZTyp.LeftArrow (ZTyp.CursorT ty1) ty2 [@implicit_arity]) /* TMArrParent1 */ =>
      ZTyp.CursorT (HTyp.Arrow ty1 ty2 [@implicit_arity])
    | (Move Parent, ZTyp.RightArrow ty1 (ZTyp.CursorT ty2) [@implicit_arity]) /* TMArrParent2 */ =>
      ZTyp.CursorT (HTyp.Arrow ty1 ty2 [@implicit_arity])
    | (Move (Child 1), ZTyp.CursorT (HTyp.Sum ty1 ty2 [@implicit_arity])) /* 25a */ =>
      ZTyp.LeftSum (ZTyp.CursorT ty1) ty2 [@implicit_arity]
    | (Move (Child 2), ZTyp.CursorT (HTyp.Sum ty1 ty2 [@implicit_arity])) /* 25b */ =>
      ZTyp.RightSum ty1 (ZTyp.CursorT ty2) [@implicit_arity]
    | (Move Parent, ZTyp.LeftSum (ZTyp.CursorT ty1) ty2 [@implicit_arity]) /* 25c */ =>
      ZTyp.CursorT (HTyp.Sum ty1 ty2 [@implicit_arity])
    | (Move Parent, ZTyp.RightSum ty1 (ZTyp.CursorT ty2) [@implicit_arity]) /* 25d */ =>
      ZTyp.CursorT (HTyp.Sum ty1 ty2 [@implicit_arity])
    /* Del */
    | (Del, ZTyp.CursorT ty) /* TMDel */ => ZTyp.CursorT HTyp.Hole
    /* Construct */
    | (Construct SArrow, ZTyp.CursorT ty) /* TMConArrow */ =>
      ZTyp.RightArrow ty (ZTyp.CursorT HTyp.Hole) [@implicit_arity]
    | (Construct SNum, ZTyp.CursorT HTyp.Hole) /* TMConNum */ => ZTyp.CursorT HTyp.Num
    | (Construct SSum, ZTyp.CursorT ty) /* 22a */ =>
      ZTyp.RightSum ty (ZTyp.CursorT HTyp.Hole) [@implicit_arity]
    /* Zipper Rules */
    | (_, ZTyp.LeftArrow zty1 ty2 [@implicit_arity]) /* TMArrZip1 */ =>
      let zty1' = performTyp a zty1;
      ZTyp.LeftArrow zty1' ty2 [@implicit_arity]
    | (_, ZTyp.RightArrow ty1 zty2 [@implicit_arity]) /* TMArrZip2 */ =>
      let zty2' = performTyp a zty2;
      ZTyp.RightArrow ty1 zty2' [@implicit_arity]
    | (_, ZTyp.LeftSum zty1 ty2 [@implicit_arity]) /* 22b */ =>
      let zty1' = performTyp a zty1;
      ZTyp.LeftSum zty1' ty2 [@implicit_arity]
    | (_, ZTyp.RightSum ty1 zty2 [@implicit_arity]) /* 22c */ =>
      let zty2' = performTyp a zty2;
      ZTyp.RightSum ty1 zty2' [@implicit_arity]
    | _ => raise InvalidAction
    };
  let rec performEMove action ze =>
    switch action {
    | Move direction =>
      switch (direction, ze) {
      /* Ascription */
      | (Child 1, ZExp.CursorE (HExp.Asc e ty [@implicit_arity])) /* EMAscChild1 */ =>
        ZExp.LeftAsc (ZExp.CursorE e) ty [@implicit_arity]
      | (Child 2, ZExp.CursorE (HExp.Asc e ty [@implicit_arity])) /* EMAscChild2 */ =>
        ZExp.RightAsc e (ZTyp.CursorT ty) [@implicit_arity]
      | (Parent, ZExp.LeftAsc (ZExp.CursorE e) ty [@implicit_arity]) /* EMAscParent1 */ =>
        ZExp.CursorE (HExp.Asc e ty [@implicit_arity])
      | (Parent, ZExp.RightAsc e (ZTyp.CursorT ty) [@implicit_arity]) /* EMAscParent2 */ =>
        ZExp.CursorE (HExp.Asc e ty [@implicit_arity])
      /* Let */
      | (Child 1, ZExp.CursorE (HExp.Let x e e' [@implicit_arity])) =>
        ZExp.LetZ1 x (ZExp.CursorE e) e' [@implicit_arity]
      | (Child 2, ZExp.CursorE (HExp.Let x e e' [@implicit_arity])) =>
        ZExp.LetZ2 x e (ZExp.CursorE e') [@implicit_arity]
      | (Parent, ZExp.LetZ1 x (ZExp.CursorE e) e' [@implicit_arity]) =>
        ZExp.CursorE (HExp.Let x e e' [@implicit_arity])
      | (Parent, ZExp.LetZ2 x e (ZExp.CursorE e') [@implicit_arity]) =>
        ZExp.CursorE (HExp.Let x e e' [@implicit_arity])
      /* Lambda */
      | (Child 1, ZExp.CursorE (HExp.Lam x e [@implicit_arity])) /* EMLamChild1 */ =>
        ZExp.LamZ x (ZExp.CursorE e) [@implicit_arity]
      | (Parent, ZExp.LamZ x (ZExp.CursorE e) [@implicit_arity]) /* EMLamParent */ =>
        ZExp.CursorE (HExp.Lam x e [@implicit_arity])
      /* Application */
      | (Child 1, ZExp.CursorE (HExp.Ap e1 e2 [@implicit_arity])) /* EMApChild1 */ =>
        ZExp.LeftAp (ZExp.CursorE e1) e2 [@implicit_arity]
      | (Child 2, ZExp.CursorE (HExp.Ap e1 e2 [@implicit_arity])) /* EMApChild2 */ =>
        ZExp.RightAp e1 (ZExp.CursorE e2) [@implicit_arity]
      | (Parent, ZExp.LeftAp (ZExp.CursorE e1) e2 [@implicit_arity]) /* EMApParent1 */ =>
        ZExp.CursorE (HExp.Ap e1 e2 [@implicit_arity])
      | (Parent, ZExp.RightAp e1 (ZExp.CursorE e2) [@implicit_arity]) /* EMApParent2 */ =>
        ZExp.CursorE (HExp.Ap e1 e2 [@implicit_arity])
      /* Plus */
      | (Child 1, ZExp.CursorE (HExp.Plus e1 e2 [@implicit_arity])) /* EMPlusChild1 */ =>
        ZExp.LeftPlus (ZExp.CursorE e1) e2 [@implicit_arity]
      | (Child 2, ZExp.CursorE (HExp.Plus e1 e2 [@implicit_arity])) /* EMPlusChild2 */ =>
        ZExp.RightPlus e1 (ZExp.CursorE e2) [@implicit_arity]
      | (Parent, ZExp.LeftPlus (ZExp.CursorE e1) e2 [@implicit_arity]) /* EMPlusParent1 */ =>
        ZExp.CursorE (HExp.Plus e1 e2 [@implicit_arity])
      | (Parent, ZExp.RightPlus e1 (ZExp.CursorE e2) [@implicit_arity]) /* EMPlusParent2 */ =>
        ZExp.CursorE (HExp.Plus e1 e2 [@implicit_arity])
      /* Injection */
      | (Child 1, ZExp.CursorE (HExp.Inj side e [@implicit_arity])) /* (26a) */ =>
        ZExp.InjZ side (ZExp.CursorE e) [@implicit_arity]
      | (Parent, ZExp.InjZ side (ZExp.CursorE e) [@implicit_arity]) /* (26b) */ =>
        ZExp.CursorE (HExp.Inj side e [@implicit_arity])
      /* Case */
      | (Child 1, ZExp.CursorE (HExp.Case e branch1 branch2 [@implicit_arity])) /* (26c) */ =>
        ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2 [@implicit_arity]
      | (Child 2, ZExp.CursorE (HExp.Case e (x, e1) branch2 [@implicit_arity])) /* (26d) */ =>
        ZExp.CaseZ2 e (x, ZExp.CursorE e1) branch2 [@implicit_arity]
      | (Child 3, ZExp.CursorE (HExp.Case e branch1 (y, e2) [@implicit_arity])) /* (26e) */ =>
        ZExp.CaseZ3 e branch1 (y, ZExp.CursorE e2) [@implicit_arity]
      | (Parent, ZExp.CaseZ1 (ZExp.CursorE e) branch1 branch2 [@implicit_arity]) /* (26f) */ =>
        ZExp.CursorE (HExp.Case e branch1 branch2 [@implicit_arity])
      | (Parent, ZExp.CaseZ2 e (x, ZExp.CursorE e1) branch2 [@implicit_arity]) /* (26g) */ =>
        ZExp.CursorE (HExp.Case e (x, e1) branch2 [@implicit_arity])
      | (Parent, ZExp.CaseZ3 e branch1 (y, ZExp.CursorE e2) [@implicit_arity]) /* (26h) */ =>
        ZExp.CursorE (HExp.Case e branch1 (y, e2) [@implicit_arity])
      /* Non-Empty Hole */
      | (Child 1, ZExp.CursorE (HExp.NonEmptyHole e)) /* EMNEHoleChild1 */ =>
        ZExp.NonEmptyHoleZ (ZExp.CursorE e)
      | (Parent, ZExp.NonEmptyHoleZ (ZExp.CursorE e)) /* EMNEHoleChild2 */ =>
        ZExp.CursorE (HExp.NonEmptyHole e)
      | _ => raise InvalidAction
      }
    | _ => raise InvalidAction
    };
  let hsyn ctx e =>
    try (HExp.syn ctx e) {
    | HExp.IllTyped => raise InvalidAction
    };
  let hana ctx e ty =>
    try (HExp.ana ctx e ty) {
    | HExp.IllTyped => raise InvalidAction
    };
  let rec performSyn ctx a (ze, ty) =>
    try (performEMove a ze, ty) /* SAMove */ {
    | InvalidAction =>
      switch (a, (ze, ty)) {
      /* Deletion */
      | (Del, (ZExp.CursorE e, _)) /* SADel */ => (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)
      /* Construction */
      | (Construct SAsc, (ZExp.CursorE e, ty)) /* SAConAsc */ => (
          ZExp.RightAsc e (ZTyp.CursorT ty) [@implicit_arity],
          ty
        )
      | (Construct (SVar x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) /* SAConVar */ =>
        switch (Ctx.lookup ctx x) {
        | Some xty => (ZExp.CursorE (HExp.Var x), xty)
        | None => raise InvalidAction
        }
      | (Construct (SLet x), (ZExp.CursorE e, ty)) =>
        switch e {
        | HExp.EmptyHole => (
            ZExp.LetZ1 x (ZExp.CursorE e) HExp.EmptyHole [@implicit_arity],
            HTyp.Hole
          )
        | _ => (ZExp.LetZ2 x e (ZExp.CursorE HExp.EmptyHole) [@implicit_arity], HTyp.Hole)
        }
      | (Construct (SLam x), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) /* SAConLam */ => (
          ZExp.RightAsc
            (HExp.Lam x HExp.EmptyHole [@implicit_arity])
            (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole [@implicit_arity])
          [@implicit_arity],
          HTyp.Arrow HTyp.Hole HTyp.Hole [@implicit_arity]
        )
      | (Construct SAp, (ZExp.CursorE e, ty)) =>
        switch (HTyp.matched_arrow ty) {
        | Some (_, ty2) /* SAConApArr */ => (
            ZExp.RightAp e (ZExp.CursorE HExp.EmptyHole) [@implicit_arity],
            ty2
          )
        | None /* SAConApOtw */ => (
            ZExp.RightAp (HExp.NonEmptyHole e) (ZExp.CursorE HExp.EmptyHole) [@implicit_arity],
            HTyp.Hole
          )
        }
      | (Construct (SLit n), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) /* SAConNumLit */ =>
        if (n < 0) {
          raise InvalidAction
        } else {
          (ZExp.CursorE (HExp.NumLit n), HTyp.Num)
        }
      | (Construct SPlus, (ZExp.CursorE e, _)) =>
        if (HTyp.consistent ty HTyp.Num) {
          (
            /* SAConPlus1 */
            ZExp.RightPlus e (ZExp.CursorE HExp.EmptyHole) [@implicit_arity],
            HTyp.Num
          )
        } else {
          (
            /* SAConPlus2 */
            ZExp.RightPlus (HExp.NonEmptyHole e) (ZExp.CursorE HExp.EmptyHole) [@implicit_arity],
            HTyp.Num
          )
        }
      | (Construct (SInj side), (ZExp.CursorE HExp.EmptyHole, HTyp.Hole)) /* 24a */ => (
          ZExp.RightAsc
            (HExp.Inj side HExp.EmptyHole [@implicit_arity])
            (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole [@implicit_arity])
          [@implicit_arity],
          HTyp.Sum HTyp.Hole HTyp.Hole [@implicit_arity]
        )
      | (Construct (SCase x y [@implicit_arity]), (ZExp.CursorE e, ty)) =>
        switch (HTyp.matched_sum ty) {
        | Some _ /* 24b */ => (
            ZExp.LeftAsc
              (
                ZExp.CaseZ2 e (x, ZExp.CursorE HExp.EmptyHole) (y, HExp.EmptyHole) [@implicit_arity]
              )
              HTyp.Hole
            [@implicit_arity],
            HTyp.Hole
          )
        | None /* 24c */ => (
            ZExp.LeftAsc
              (
                ZExp.CaseZ1
                  (ZExp.NonEmptyHoleZ (ZExp.CursorE e)) (x, HExp.EmptyHole) (y, HExp.EmptyHole)
                [@implicit_arity]
              )
              HTyp.Hole
            [@implicit_arity],
            HTyp.Hole
          )
        }
      | (Construct SNEHole, (ZExp.CursorE e', ty)) /* SAConNEHole */ => (
          ZExp.NonEmptyHoleZ (ZExp.CursorE e'),
          HTyp.Hole
        )
      /* Finish */
      | (Finish, (ZExp.CursorE (HExp.NonEmptyHole e), HTyp.Hole)) /* SAFinish */ =>
        let ty' = hsyn ctx e;
        (ZExp.CursorE e, ty')
      /* Zipper Cases */
      | (a, (ZExp.LeftAsc ze ty [@implicit_arity], _)) /* SAZipAsc1 */ => (
          ZExp.LeftAsc (performAna ctx a ze ty) ty [@implicit_arity],
          ty
        )
      | (a, (ZExp.RightAsc e zty [@implicit_arity], _)) /* SAZipAsc2 */ =>
        let zty' = performTyp a zty;
        let ty' = ZTyp.erase zty';
        try {
          let _ = hana ctx e ty';
          (ZExp.RightAsc e zty' [@implicit_arity], ty')
        } {
        | HExp.IllTyped => raise InvalidAction
        }
      | (a, (ZExp.LetZ1 x ze1 e2 [@implicit_arity], _)) =>
        let e1 = ZExp.erase ze1;
        let ty1 = hsyn ctx e1;
        let (ze1', ty1') = performSyn ctx a (ze1, ty1);
        let ctx' = Ctx.extend ctx (x, ty1');
        try {
          let ty2' = hsyn ctx' e2;
          (ZExp.LetZ1 x ze1' e2 [@implicit_arity], ty2')
        } {
        | HExp.IllTyped => raise InvalidAction
        }
      | (a, (ZExp.LetZ2 x e1 ze2 [@implicit_arity], _)) =>
        let ty1 = hsyn ctx e1;
        let ctx' = Ctx.extend ctx (x, ty1);
        let (ze2', ty2') = performSyn ctx' a (ze2, ty);
        (ZExp.LetZ2 x e1 ze2' [@implicit_arity], ty2')
      | (_, (ZExp.LeftAp ze1 e2 [@implicit_arity], _)) /* SAZipApArr */ =>
        let e1 = ZExp.erase ze1;
        let ty2 = hsyn ctx e1;
        let (ze1', ty3) = performSyn ctx a (ze1, ty2);
        switch (HTyp.matched_arrow ty3) {
        | Some (ty4, ty5) =>
          let _ = hana ctx e2 ty4;
          (ZExp.LeftAp ze1' e2 [@implicit_arity], ty5)
        | None => raise InvalidAction
        }
      | (_, (ZExp.RightAp e1 ze2 [@implicit_arity], _)) /* SAZipApAna */ =>
        let ty2 = hsyn ctx e1;
        switch (HTyp.matched_arrow ty2) {
        | Some (ty3, ty4) =>
          let ze2' = performAna ctx a ze2 ty3;
          (ZExp.RightAp e1 ze2' [@implicit_arity], ty4)
        | None => raise InvalidAction
        }
      | (_, (ZExp.LeftPlus ze1 e2 [@implicit_arity], _)) /* SAZipPlus1 */ =>
        let ze1' = performAna ctx a ze1 HTyp.Num;
        (ZExp.LeftPlus ze1' e2 [@implicit_arity], HTyp.Num)
      | (_, (ZExp.RightPlus e1 ze2 [@implicit_arity], _)) /* SAZipPlus2 */ =>
        let ze2' = performAna ctx a ze2 HTyp.Num;
        (ZExp.RightPlus e1 ze2' [@implicit_arity], HTyp.Num)
      | (_, (ZExp.NonEmptyHoleZ ze1, _)) /* SAZipHole */ =>
        let e1 = ZExp.erase ze1;
        let ty1 = hsyn ctx e1;
        let (ze1', _) = performSyn ctx a (ze1, ty1);
        (ZExp.NonEmptyHoleZ ze1', HTyp.Hole)
      | _ => raise InvalidAction
      }
    }
  and performAna ctx a ze ty =>
    switch a {
    | Move _ /* AAMove */ =>
      /* try to use the non-zipper move actions */
      try (performEMove a ze) {
      | InvalidAction =>
        /* if it doesn't work, keep going --
         * it'll hit the subsumption rule at the bottom */
        performAna_postMoveCheck ctx a ze ty
      }
    | _ => performAna_postMoveCheck ctx a ze ty
    }
  and performAna_postMoveCheck ctx a ze ty =>
    switch (a, ze, ty) {
    /* Deletion */
    | (Del, ZExp.CursorE e, _) /* AADel */ => ZExp.CursorE HExp.EmptyHole
    /* Construction */
    | (Construct SAsc, ZExp.CursorE e, _) /* AAConAsc */ =>
      ZExp.RightAsc e (ZTyp.CursorT ty) [@implicit_arity]
    | (Construct (SVar x), ZExp.CursorE HExp.EmptyHole, ty)
        when
          switch (Ctx.lookup ctx x) {
          | Some xty => HTyp.inconsistent ty xty
          | None => false
          } /* SAConVar */ =>
      ZExp.NonEmptyHoleZ (ZExp.CursorE (HExp.Var x))
    | (Construct (SLet x), ZExp.CursorE HExp.EmptyHole, _) =>
      ZExp.LetZ1 x ze HExp.EmptyHole [@implicit_arity]
    | (Construct (SLam x), ZExp.CursorE HExp.EmptyHole, ty) =>
      switch (HTyp.matched_arrow ty) {
      | Some _ /* AAConLam1 */ => ZExp.LamZ x ze [@implicit_arity]
      | None /* AAConLam2 */ =>
        ZExp.NonEmptyHoleZ (
          ZExp.RightAsc
            (HExp.Lam x HExp.EmptyHole [@implicit_arity])
            (ZTyp.LeftArrow (ZTyp.CursorT HTyp.Hole) HTyp.Hole [@implicit_arity])
          [@implicit_arity]
        )
      }
    | (Construct (SLit n), ZExp.CursorE HExp.EmptyHole, ty)
        when HTyp.inconsistent ty HTyp.Num /* AAConNumLit */ =>
      if (n < 0) {
        raise InvalidAction
      } else {
        ZExp.NonEmptyHoleZ (ZExp.CursorE (HExp.NumLit n))
      }
    | (Construct (SInj side), ZExp.CursorE HExp.EmptyHole, ty) =>
      switch (HTyp.matched_sum ty) {
      | Some _ /* 23a */ => ZExp.InjZ side ze [@implicit_arity]
      | None /* 23b */ =>
        ZExp.NonEmptyHoleZ (
          ZExp.RightAsc
            (HExp.Inj side HExp.EmptyHole [@implicit_arity])
            (ZTyp.LeftSum (ZTyp.CursorT HTyp.Hole) HTyp.Hole [@implicit_arity])
          [@implicit_arity]
        )
      }
    | (Construct (SCase x y [@implicit_arity]), ZExp.CursorE e, ty) /* 23c */ =>
      ZExp.CaseZ1 (ZExp.CursorE HExp.EmptyHole) (x, HExp.EmptyHole) (y, HExp.EmptyHole)
      [@implicit_arity]
    /* Finishing */
    | (Finish, ZExp.CursorE (HExp.NonEmptyHole e), _) /* AAFinish */ =>
      let _ = hana ctx e ty;
      ZExp.CursorE e
    /* Zipper Cases */
    | (_, ZExp.LetZ1 x ze1 e2 [@implicit_arity], _) =>
      let e1 = ZExp.erase ze1;
      let ty1 = hsyn ctx e1;
      let (ze1', ty1') = performSyn ctx a (ze1, ty1);
      let ctx' = Ctx.extend ctx (x, ty1');
      try {
        let _ = hana ctx' e2 ty;
        ZExp.LetZ1 x ze1' e2 [@implicit_arity]
      } {
      | HExp.IllTyped => raise InvalidAction
      }
    | (_, ZExp.LetZ2 x e1 ze2 [@implicit_arity], _) =>
      let ty1 = hsyn ctx e1;
      let ctx' = Ctx.extend ctx (x, ty1);
      let ze2' = performAna ctx' a ze2 ty;
      ZExp.LetZ2 x e1 ze2' [@implicit_arity]
    | (_, ZExp.LamZ x ze' [@implicit_arity], ty) /* AAZipLam */ =>
      switch (HTyp.matched_arrow ty) {
      | Some (ty1, ty2) =>
        let ctx' = Ctx.extend ctx (x, ty1);
        let ze'' = performAna ctx' a ze' ty2;
        ZExp.LamZ x ze'' [@implicit_arity]
      | None => raise InvalidAction
      }
    | (_, ZExp.InjZ side ze [@implicit_arity], ty) /* 23d */ =>
      switch (HTyp.matched_sum ty) {
      | Some (ty1, ty2) =>
        ZExp.InjZ HExp.L (performAna ctx a ze (HExp.pick_side side ty1 ty2)) [@implicit_arity]
      | None => raise InvalidAction
      }
    | (_, ZExp.CaseZ1 ze (x, e1) (y, e2) [@implicit_arity], ty) /* 23e */ =>
      let e0 = ZExp.erase ze;
      let ty0 = hsyn ctx e0;
      let (ze', ty0') = performSyn ctx a (ze, ty0);
      switch (HTyp.matched_sum ty0') {
      | Some (ty1, ty2) =>
        let ctx1 = Ctx.extend ctx (x, ty1);
        let () = hana ctx1 e1 ty;
        let ctx2 = Ctx.extend ctx (y, ty2);
        let () = hana ctx2 e2 ty;
        ZExp.CaseZ1 ze' (x, e1) (y, e2) [@implicit_arity]
      | None => raise InvalidAction
      }
    | (_, ZExp.CaseZ2 e0 (x, ze1) (y, e2) [@implicit_arity], ty) /* 23f */ =>
      let ty0 = hsyn ctx e0;
      switch (HTyp.matched_sum ty0) {
      | Some (ty1, ty2) =>
        let ctx1 = Ctx.extend ctx (x, ty1);
        let ze1' = performAna ctx1 a ze1 ty;
        ZExp.CaseZ2 e0 (x, ze1') (y, e2) [@implicit_arity]
      | None => raise InvalidAction
      }
    | (_, ZExp.CaseZ3 e0 (x, e1) (y, ze2) [@implicit_arity], ty) /* 23g */ =>
      let ty0 = hsyn ctx e0;
      switch (HTyp.matched_sum ty0) {
      | Some (ty1, ty2) =>
        let ctx2 = Ctx.extend ctx (y, ty2);
        let ze2' = performAna ctx2 a ze2 ty;
        ZExp.CaseZ3 e0 (x, e1) (y, ze2') [@implicit_arity]
      | None => raise InvalidAction
      }
    /* Subsumption */
    | _ /* AASubsume */ =>
      let e = ZExp.erase ze;
      let ty1 = hsyn ctx e;
      let (ze', ty1') = performSyn ctx a (ze, ty1);
      if (HTyp.consistent ty ty1') {
        ze'
      } else {
        raise InvalidAction
      }
    };
};
