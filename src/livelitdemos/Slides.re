/* User-defined-livelit example slides, shipped as documentation.
   Sources are the .hz files in hazel-programs/livelits. Regenerate the
   encodings with regen-slides.sh there after editing a source — nothing
   checks that they are current. */

let all_slides: list((string, Haz3lcore.PersistentSegment.t)) =
  [LivelitSlider.out, LivelitColor.out]
  |> List.map(((name, seg)) => ("Livelits / " ++ name, seg));
