open Language;

module View = {
  let view =
      (~globals, theorems: list((Id.t, TermBase.environment_t, Typ.t))) => {
    theorems
    |> List.map(((_a, _b, c)) =>
         CodeViewable.view_typ(
           ~globals,
           ~settings=
             Haz3lcore.ExpToSegment.Settings.of_core(
               ~inline=true,
               globals.settings.core,
             ),
           c,
         )
       );
  };
};
