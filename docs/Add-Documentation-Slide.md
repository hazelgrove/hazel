Documentation slides are committed `.hz` text files: the files in
`hazel-programs/docs/` ARE the slides — embedded at compile time
(`[%blob]`) and parsed at load (FastParse, with the recovering parser as
fallback). `?` is an explicit hole tile, `¿` an implicit hole (Grout);
probe/statics pins are `^^probe`/`^^statics` triggers in the text.

To add or update a slide:

1. Get the program text. Either write the `.hz` directly, or author the
   buffer in the editor and export it: `Nut Menu` > `File` >
   `Export for Init` (or `cmd+k` > `Export for Init`) downloads the
   current buffer in committed-`.hz` form.
2. Put the file under `hazel-programs/docs/<category>/`
   (`reference/` for general docs, `b2t2/` for the table benchmark).
   The MVU example apps live in `hazel-programs/mvu/` instead, since
   they are also runnable from the CLI.
3. Add a `(title, [%blob "file.hz"])` entry to the matching slides
   module: `src/docslides/Slides.re`, `src/b2t2/Slides.re` or
   `src/mvu/Slides.re`. List order is slide order.

Leading indentation in the file is display-only (flattened at load and
recomputed by layout); the file ends with exactly one final newline.
`Test_FastParseCorpus` requires every committed `.hz` to take the fast
parse path — if your slide bails, the console names the reason.
