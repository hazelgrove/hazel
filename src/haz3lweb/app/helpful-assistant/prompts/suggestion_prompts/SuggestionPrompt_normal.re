module Sexp = Sexplib.Sexp;

let self = (completion_token: string) => [
  "- CODE COMPLETION INSTRUCTIONS:",
  "- Reply with a functional, idiomatic replacement for the program hole marked '"
  ++ completion_token
  ++ "' in the provided program sketch",
  "- The program hole marked '"
  ++ completion_token
  ++ "' consists of the hole token '??' appended with a series of characters to help you differentiate between other holes in the program",
  // Uncomment either of the following lines to test error rounds
  // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
  // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
  "- Reply ONLY with a SINGLE replacement term for the unqiue distinguished hole marked '"
  ++ completion_token
  ++ "'",
  "- Reply ONLY with code",
  "- DO NOT suggest more replacements for other holes in the sketch (marked, '?', '??', or '?a'), or implicit holes",
  "- This is critical, and I am going to reiterate it: DO NOT suggest more than one replacement term. It should ONLY be for the hole marked '"
  ++ completion_token
  ++ "'",
  "- For example, if you are being asked to complete 'let f = ? in "
  ++ completion_token
  ++ "', your response should ONLY be a single replacement term for the hole marked '"
  ++ completion_token
  ++ "', NOT a replacement term for the hole marked '?'",
  "- i.e. You should ONLY respond with a function application, or something else which would be a valid replacement term for the hole marked '"
  ++ completion_token
  ++ "'",
  "- If you wish to include a hole in your response, use '??' only, without the appended characters that were used to identify the specific hole you were given",
  "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
  "- DO NOT include the program sketch in your reply",
  "- DO NOT include a period at the end of your response and DO NOT use markdown",
];
