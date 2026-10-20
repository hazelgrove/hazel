[@deriving (show({with_path: false}), sexp, yojson, enumerate)]
type t =
  | Drv(Drv.Any.cls)
  | Fumola(FumolaCls.t)
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
  /* No "Fumola" prefix, unlike ALFA above: Fumola is one sort, so the
     inspector's sort chip beside this already says FUMOLA and a prefix here
     would read as "FUMOLA / Fumola Variant". Callers that show the class on
     its own add the prefix themselves. */
  | Fumola(cls) => FumolaCls.show(cls)
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
