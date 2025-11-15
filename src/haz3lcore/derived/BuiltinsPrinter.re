open Language;

/* Just a convenience for getting a string of the current builtin functions */

let builtin_printer_settings: ExpToSegment.Settings.t =
  ExpToSegment.Settings.of_core(~inline=Single, CoreSettings.off);

let builtin_typ: BuiltinsUtil.builtin => Typ.t =
  (builtin: BuiltinsUtil.builtin) =>
    switch (builtin) {
    | BuiltinsUtil.Const({typ, _}) => Typ.temp(typ)
    | BuiltinsUtil.Fn({arg, ret, _})
    | BuiltinsUtil.HazelFn({arg, ret, _}) =>
      BuiltinsUtil.Fresh.Typ.arrow(Typ.temp(arg), Typ.temp(ret))
    };

let typ_to_string: Typ.t => string =
  (typ: Typ.t) => {
    let segment: Segment.t =
      ExpToSegment.typ_to_segment(~settings=builtin_printer_settings, typ);
    Printer.of_segment(~holes="?", ~indent="", segment);
  };

let builtin_signature_line: BuiltinsUtil.builtin => string =
  (builtin: BuiltinsUtil.builtin) => {
    let name: string = BuiltinsUtil.name_of_builtin(builtin);
    let typ: Typ.t = builtin_typ(builtin);
    let typ_string: string = typ_to_string(typ);
    name ++ ": " ++ typ_string;
  };

let compare_builtin: (BuiltinsUtil.builtin, BuiltinsUtil.builtin) => int =
  (a, b) =>
    String.compare(
      BuiltinsUtil.name_of_builtin(a),
      BuiltinsUtil.name_of_builtin(b),
    );

let builtin_value_signatures: unit => string =
  () => {
    let builtins_list: list(BuiltinsUtil.builtin) = Builtins.builtins;
    let lines: list(string) =
      List.map(
        builtin_signature_line,
        List.sort(compare_builtin, builtins_list),
      );
    String.concat("\n", lines);
  };
