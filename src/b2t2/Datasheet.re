/* The B2T2 datasheet is markdown, not a program: the slide is a single
   TextArea projector holding the file's content. The blob is wrapped as
   `^^text("...")` slide TEXT (linebreaks \n-escaped — the content has
   no quotes or backslashes to escape), so it loads through the same
   text path as every other slide and the trigger materializes the
   projector. */
let content = [%blob "Datasheet.md"];

let slide_text: string =
  "^^text(\"" ++ Util_web.StringUtil.escape_linebreaks(content) ++ "\")";
