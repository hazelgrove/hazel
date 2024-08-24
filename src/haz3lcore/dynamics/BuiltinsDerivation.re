type naive_entry = (string, int);

let naive_entries = [
  ("Truth", 0),
  ("Falsity", 0),
  ("And", 2),
  ("Or", 2),
  ("Implies", 2),
  ("Entail", 2),
];

let mk_ctr_entry: naive_entry => Ctx.var_entry =
  ((name, args)) => {
    name,
    id: Id.mk(),
    typ:
      (
        switch (args) {
        | 0 => Prop
        | 1 => Arrow(Prop |> Typ.fresh, Prop |> Typ.fresh)
        | n =>
          Arrow(
            Prod(List.init(n, _ => Prop |> Typ.fresh)) |> Typ.fresh,
            Prop |> Typ.fresh,
          )
        }
      )
      |> Typ.fresh,
  };

let ctr_entries = naive_entries |> List.map(mk_ctr_entry);

let mk_variant: naive_entry => ConstructorMap.variant(Typ.t) =
  ((name, args)) => {
    Variant(
      name,
      [Id.mk()],
      switch (args) {
      | 0 => None
      | 1 => Some(Prop |> Typ.fresh)
      | n => Some(Prod(List.init(n, _ => Prop |> Typ.fresh)) |> Typ.fresh)
      },
    );
  };

let tvar_entries =
  Ctx.TVarEntry({
    name: "P",
    id: Id.invalid,
    kind:
      Ctx.Singleton(
        Sum(naive_entries |> List.map(mk_variant)) |> Typ.fresh,
      ),
  });
// let prop_entry: Ctx.entry = {
//   let prop_cons_map: ConstructorMap.t(Typ.t) = [
//     ConstructorMap.Variant("A", [Id.mk()], None),
//   ];
//   Ctx.TVarEntry({
//     name: "Prop",
//     id: Id.invalid,
//     kind: Ctx.Singleton(Sum(prop_cons_map) |> Typ.fresh),
//   });
// };
