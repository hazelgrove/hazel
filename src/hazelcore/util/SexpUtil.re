open Sexplib;

let print = (~at: option(string)=?, sexp: Sexp.t): unit => {
  Option.(at |> map(print_endline) |> value(~default=()));
  sexp |> Sexp.to_string_hum |> print_endline;
};

let print_many = (~at: option(string)=?, sexps: list(Sexp.t)): unit => {
  Option.(at |> map(print_endline) |> value(~default=()));
  sexps |> List.map(Sexp.to_string_hum) |> List.iter(print_endline);
};
