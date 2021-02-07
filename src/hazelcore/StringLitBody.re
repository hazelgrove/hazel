type t = string;

let of_string: string => t = x => x;
let to_string: t => string = x => x;

let sexp_of_t = s => Sexplib.Sexp.of_string(s);
let t_of_sexp = sexp => Sexplib.Sexp.to_string(sexp);

let to_unescaped_string = UnescapedString.unescaped_total;
