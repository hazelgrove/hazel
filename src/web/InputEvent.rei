open Haz3lcore;

/* [actions_of_input(~composed, value)] is the edit sequence taking the
   editor from [composed], the input text already dispatched, to the input's
   new [value]: a Destruct per grapheme of the divergent tail, then an Insert
   per new grapheme. Tabs are dropped; a newline is Token.linebreak. */
let actions_of_input: (~composed: string, string) => list(Action.t);
