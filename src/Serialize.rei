let serialize:
  (~fmtr: Format.formatter=?, ~line_length: int=?, ~indent: int=?, UHExp.t) =>
  unit;
let string_of_uhexp: UHExp.t => string;
