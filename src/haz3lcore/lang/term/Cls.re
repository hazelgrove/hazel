[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  | Exp(Exp.cls)
  | Pat(Pat.cls)
  | TypSlice(TypSlice.cls)
  | TPat(TPat.cls)
  | Rul(Rul.cls)
  | Secondary(Secondary.cls);

let show = (cls: t) =>
  switch (cls) {
  | Exp(cls) => Exp.show_cls(cls)
  | Pat(cls) => Pat.show_cls(cls)
  | TypSlice(cls) => TypSlice.show_cls(cls)
  | TPat(cls) => TPat.show_cls(cls)
  | Rul(cls) => Rul.show_cls(cls)
  | Secondary(cls) => Secondary.show_cls(cls)
  };
