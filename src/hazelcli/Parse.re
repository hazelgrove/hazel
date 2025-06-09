open Haz3lcore;

let parse_program = (s: string) =>
  switch (Parse.parse_exp(s)) {
  | Some(e) => e
  | None => failwith("Failed to parse expression: " ++ s)
  };
