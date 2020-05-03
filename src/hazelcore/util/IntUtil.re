let num_digits = (n: int): int => String.length(string_of_int(n));
let num_digits_str = (n: string): int => String.length(n);
let leading_zeros = (n: string): int => {
  let trimmed_num = string_of_int(int_of_string(n));
  switch (trimmed_num) {
  | "0" => String.length(n) - 1
  | _ => String.index(n, trimmed_num.[0])
  };
};
