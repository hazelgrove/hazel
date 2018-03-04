let force_opt opt =>
  switch opt {
  | Some x => x
  | _ => assert false
  };

let alphabet = [|
  "a",
  "b",
  "c",
  "d",
  "e",
  "f",
  "g",
  "h",
  "i",
  "j",
  "k",
  "l",
  "m",
  "n",
  "o",
  "p",
  "q",
  "r",
  "s",
  "t",
  "u",
  "v",
  "w",
  "x",
  "y",
  "z"
|];

let rec int_base26 (i: int) =>
  if (i < 26) {
    alphabet.(i)
  } else {
    let divided = i / 26;
    let remainder = i mod 26;
    let last_char = alphabet.(remainder);
    int_base26 (divided - 1) ^ last_char
  };
