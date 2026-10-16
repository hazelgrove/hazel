/* ReasonML-surface example slides: the committed .reason files in
 * hazel-programs/reason, embedded at compile time and parsed (with the
 * Reason surface current) when the ReasonML mode loads them. */
let all_slides: list((string, Haz3lcore.PersistentZipper.t)) =
  [("Reason Basics", [%blob "basics.reason"])]
  |> List.map(((name, text)) =>
       (name, Haz3lcore.PersistentZipper.of_slide_text(text))
     );
