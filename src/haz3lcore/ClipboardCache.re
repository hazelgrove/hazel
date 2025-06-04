/* In order to retain projectors on cut/copy/paste, and to speed
 * up pasting after in-editor copy/cut, we maintain a cached of
 * the last copied selection segment and do segment insertion
 * instead of reparsing if the clipboard text contents are the
 * same as text serialization of the cached segment */

let cache: ref(option((string, Segment.t))) = ref(None);

let set = (seg: option(Segment.t), str: string): unit =>
  switch (seg) {
  | Some(seg) when Segment.deep_tile_complete(seg) =>
    /* This check makes sure we won't create backpack orphans */
    cache := Some((str, seg))
  | _ => ()
  };

let get = (pasted: string): Action.t => {
  /* Note the trimming of leading whitespace on each line */
  let trim = Util.StringUtil.trim_leading;
  let trimmed_pasted = trim(pasted);
  switch (cache^) {
  | None => Paste(String(trimmed_pasted))
  | Some((cached, segment)) =>
    /* Note that we must replace unique ids here if we want to
     * support copying and/or multiples pastes for a copy */
    trim(cached) == trimmed_pasted
      ? Paste(Segment(Segment.IDs.replace(segment)))
      : Paste(String(trimmed_pasted))
  };
};
