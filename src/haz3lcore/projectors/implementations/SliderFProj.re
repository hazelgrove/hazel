/* Slider over a Float literal. See SliderCore for the shared implementation. */
module M =
  SliderCore.Make({
    let name = "SliderF";
    let literal = "float";
    type t = float;
    let of_atom = (a: Language.Atom.t) =>
      switch (a) {
      | Float(f) => Some(f)
      | _ => None
      };
    let to_atom = (v: string): Language.Atom.t => Float(float_of_string(v));
    let to_string = Printf.sprintf("%.2f");
  });
