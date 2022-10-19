let is_js: bool = (
  switch (Sys.backend_type) {
  | Sys.Other("js_of_ocaml") => true

  | _ => false
  }: bool
);
