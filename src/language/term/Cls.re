[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  | Drv(Drv.Any.cls)
  | Exp(Exp.cls)
  | Pat(Pat.cls)
  | Typ(Typ.cls)
  | TPat(TPat.cls)
  | Rul(Rul.cls)
  | Secondary(Secondary.cls)
  | Mod(Mod.cls)
  | Sig(Sig.cls)
  | MPat(MPat.cls);

let show = (cls: t) =>
  switch (cls) {
  | Drv(cls) => "ALFA " ++ Drv.Any.show_cls(cls)
  | Exp(cls) => Exp.show_cls(cls)
  | Pat(cls) => Pat.show_cls(cls)
  | Typ(cls) => Typ.show_cls(cls)
  | TPat(cls) => TPat.show_cls(cls)
  | Rul(cls) => Rul.show_cls(cls)
  | Secondary(cls) => Secondary.show_cls(cls)
  | Mod(cls) => Mod.show_cls(cls)
  | Sig(cls) => Sig.show_cls(cls)
  | MPat(cls) => MPat.show_cls(cls)
  };

/* What the parts of a form whose types must meet are called, for messages. */
let elements_noun: t => string =
  fun
  | Exp(Match | If) => "Branches"
  | Exp(ListLit)
  | Pat(ListLit) => "Elements"
  | Exp(ListConcat)
  | Exp(BinOp(Poly(_))) => "Operands"
  | _ => "Sub-expressions";
