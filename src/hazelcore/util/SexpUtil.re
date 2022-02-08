open Sexplib;

let show: Sexp.t => string = Sexp.to_string_hum;

let print = (~at: option(string)=?, sexp: Sexp.t): unit => {
  Option.(at |> map(print_endline) |> value(~default=()));
  sexp |> show |> print_endline;
};

let print_many = (~at: option(string)=?, sexps: list(Sexp.t)): unit => {
  Option.(at |> map(print_endline) |> value(~default=()));
  sexps |> List.map(show) |> List.iter(print_endline);
};
