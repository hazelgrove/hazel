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

let get = (pasted: string): Action.t =>
  switch (cache^) {
  | None => Paste(String(pasted))
  | Some((cached, segment)) =>
    let trim = Util.StringUtil.trim_leading;
    /* Note the trim */
    let trimmed_pasted = trim(pasted);
    trim(cached) == trimmed_pasted
      ? Paste(Segment(segment)) : Paste(String(trimmed_pasted));
  };
