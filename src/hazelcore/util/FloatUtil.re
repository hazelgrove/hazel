/* returns the number of digits in a float */
let num_digits = (f: float): int =>
  /* 1 is subtracted from length because Ocaml introduces extra 0 when float is < 1 */
  if (f < 1.0) {
    String.length(string_of_float(f)) - 1;
  } else {
    String.length(string_of_float(f));
  };
let num_digits_str = (f: string): int => String.length(f);
let leading_zeros = (f: string): int => {
  let whole_num_str = String.sub(f, 0, String.index(f, '.'));
  if (String.length(whole_num_str) == 0) {
    0;
  } else {
    let whole_num = string_of_int(int_of_string(whole_num_str));
    switch (whole_num) {
    | "0" => String.length(whole_num_str)
    | _ => String.index(whole_num_str, whole_num.[0])
    };
  };
};
let trailing_zeros = (f: string): int => {
  let dot_index = String.index(f, '.');
  if (dot_index + 1 < String.length(f)) {
    let decimal_str = String.sub(f, dot_index, String.length(f) - dot_index);
    /* 1 is added to length of trimmed float because Ocaml only allows for float with 0.xx, so an extra char is introduced */
    String.length(decimal_str)
    - String.length(string_of_float(float_of_string(decimal_str)))
    + 1;
  } else {
    0;
  };
};
