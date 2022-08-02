let num_digits = n => String.length(string_of_int(n));

let modulo = (x, y) => {
  let result = x mod y;
  result >= 0 ? result : result + y;
};
