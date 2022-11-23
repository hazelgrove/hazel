let num_digits = n => String.length(string_of_int(n));

let modulo = (x, y) => {
  let result = x mod y;
  result >= 0 ? result : result + y;
};

let ipow = (base: int, exponent: int): int => {
  let rec ipow_iter = (b: int, e: int, r: int): int =>
    if (e === 0) {
      r;
    } else if (e land 1 !== 0) {
      ipow_iter(b * b, e lsr 1, r * b);
    } else {
      ipow_iter(b * b, e lsr 1, r);
    };
  ipow_iter(base, exponent, 1);
};
