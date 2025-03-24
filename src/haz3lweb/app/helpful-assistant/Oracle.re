open Haz3lcore;

/* Old code
   let sanitize_prompt = (prompt: string): string => {
     //HACK: replacement of ?? below
     let prompt = Str.global_replace(Str.regexp("\\?\\?"), "", prompt);
     let prompt =
       if (Str.string_match(Str.regexp("^\".*\"$"), prompt, 0)) {
         String.sub(prompt, 1, String.length(prompt) - 2);
       } else {
         prompt;
       };
     prompt;
   };
   */

let ask = (body: string): option(OpenRouter.prompt) => {
  /*
   let system_prompt = [
     "Respond as minimally as possible",
     "Do not include a period at the end of your response",
   ];
   */
  switch (String.trim(body)) {
  | "" => None
  | _ =>
    let input = [{OpenRouter.role: User, OpenRouter.content: body}];
    Some(input);
  };
};

let sanitize_response: string => string =
  Str.global_replace(Str.regexp("\""), "'");

let quote = s => "\"" ++ s ++ "\"";
