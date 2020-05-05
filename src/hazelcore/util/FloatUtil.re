/* returns the number of digits in a float */
let num_digits = (f: float): int => {
  String.length(string_of_float(f));
};

let less_than_one_str = (f: string): bool => {
  float_of_string(f) < 1.0;
};
