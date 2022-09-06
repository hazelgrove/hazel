open Haz3lcore;

let precedence: DHPat.t => int;

let mk: (~parenthesize: bool=?, ~enforce_inline: bool, DHPat.t) => DHDoc.t;
