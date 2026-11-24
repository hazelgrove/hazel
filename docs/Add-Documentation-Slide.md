Documentation slides are committed `.hz` text files: the files in
`hazel-programs/world/documentation/` ARE the slides — generated into
`src/world` (WorldSlides) at build time and parsed at load (FastParse, with the recovering parser as
fallback). `?` is an explicit hole tile, `¿` an implicit hole (Grout);
probe/statics pins are `^^probe`/`^^statics` triggers in the text.

To add or update a slide:

1. Get the program text. Either write the `.hz` directly, or author the
   buffer in the editor and export it: `Nut Menu` > `File` >
   `Export for Init` (or `cmd+k` > `Export for Init`) downloads the
   current buffer in committed-`.hz` form.
2. Put the file under `hazel-programs/world/documentation/`, in the
   folder (module) it belongs to; a slide is module members
   (`let x = 1;`, not `let x = 1 in`).
3. Add a line `<file name without .hz>  <Title>` to that folder's `order`
   file, where you want it among the folder's slides (unlisted files
   follow, by name).

Leading indentation in the file is display-only (flattened at load and
recomputed by layout); the file ends with exactly one final newline.
`Test_FastParseCorpus` requires every committed `.hz` to take the fast
parse path — if your slide bails, the console names the reason.
