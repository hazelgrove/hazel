module Sexp = Sexplib.Sexp;

let self = (completion_token: string) => [
  "CODE COMPLETION INSTRUCTIONS:",
  "- First, provide a brief discussion of your approach and reasoning",
  "- Then, provide your code completion for the hole marked '"
  ++ completion_token
  ++ "' enclosed in triple backticks",
  "- The program hole marked '"
  ++ completion_token
  ++ "' consists of the hole token "
  ++ completion_token
  ++ " appended with a series of characters to help you differentiate between other holes in the program",
  "- Your response MUST include two parts:",
  "  1. A discussion section explaining your approach",
  "  2. Your code completion inside triple backticks",
  "- DO NOT include anything else in your response",
  "- DO NOT provide multiple code suggestions",
  "- DO NOT include any text after the code block",
  "- Here is an example of the format you should follow:",
  "- Discussion:",
  "- The function takes an integer n as input and returns a float.",
  "- The base case returns 1.0 when n is 0, ensuring the function adheres to the expected Float return type.",
  "- For all other cases, the function returns 2.0, maintaining consistency in return type while providing a simple branching structure.",
  "  ```",
  "  fun n -> if n == 0 then 1.0 else 2.0",
  "  ```",
  "- The code completion should be a functional, idiomatic replacement for the program hole marked '"
  ++ completion_token
  ++ "' in the provided program sketch",
  // Uncomment either of the following lines to test error rounds
  // "- However, make sure that your initial response CAUSES A TYPE ERROR in the program. Then, fix it in your second response",
  // "- However, it is CRITICALLY important to make sure that your response ALWAYS CAUSES A TYPE ERROR in the program, no matter how many times you are re-prompted",
  "- Reply ONLY with a SINGLE replacement term for the unique distinguished hole marked '"
  ++ completion_token
  ++ "'",
  "- DO NOT suggest more replacements for other holes in the sketch (marked '?', '??', or '?a'), or implicit holes",
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
  "- If you wish to include a hole in your response, use '?a' only, without the appended characters that were used to identify the specific hole you were given",
  "- IT WOULD BE A HUGE MISTAKE TO RESPOND WITH A FUNCTION BODY FOR THE HOLE MARKED '?'",
  "- DO NOT include the program sketch in your reply",
  "- DO NOT include a period at the end of your response and DO NOT use markdown",
];
