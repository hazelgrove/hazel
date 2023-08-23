let regexp = (r, s) => {
  print_endline("CALLING OCAML VERSION");
  Re.Str.string_match(Re.Str.regexp(r), s, 0);
};
