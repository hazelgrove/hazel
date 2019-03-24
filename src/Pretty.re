open Tyxml_js;
open Html5;
module NatMap = EditorBoxTypes.NatMap;
type div_attribs = list(Html5.attrib(Html_types.div_attrib));
type rev_path = EditorBoxTypes.rev_path;
type rev_paths = EditorBoxTypes.rev_paths;
module PP: {
  type doc;
  type cls = string;
  type id = string;
  let empty: doc;
  let (^^): (doc, doc) => doc;
  let nestRelative: (int, doc) => doc;
  let nestAbsolute: (int, doc) => doc;
  let text: (cls, string) => doc;
  let tagged:
    (list(cls), option((id, rev_path)), option(div_attribs), doc) => doc;
  let paletteView:
    (
      rev_path,
      Palettes.view_type,
      EditorBoxTypes.NatMap.t((HTyp.t, UHExp.block)),
      EditorBoxTypes.mk_editor_box
    ) =>
    doc;
  let blockBoundary: doc;
  let optionalBreak: string => doc;
  let mandatoryBreak: doc;
  type sdoc =
    | SEmpty
    | SText(cls, string, sdoc)
    | STagStart(list(cls), option(id), option(div_attribs), sdoc)
    | STagEnd(sdoc)
    | SPaletteView(
        rev_path,
        Palettes.view_type,
        EditorBoxTypes.NatMap.t((HTyp.t, UHExp.block)),
        EditorBoxTypes.mk_editor_box,
        sdoc,
      )
    | SLine(int, sdoc);
  let sdoc_of_doc: (int, doc, rev_paths) => sdoc;
} = {
  type cls = string;
  type id = string;
  type doc =
    | Empty
    | Concat(doc, doc)
    | NestRelative(int, doc)
    | NestAbsolute(int, doc)
    | Text(cls, string)
    | TagStart(list(cls), option((id, rev_path)), option(div_attribs))
    | TagEnd
    | BlockBoundary
    | OptionalBreak(string)
    | MandatoryBreak
    | PaletteView(
        rev_path,
        Palettes.view_type,
        EditorBoxTypes.NatMap.t((HTyp.t, UHExp.block)),
        EditorBoxTypes.mk_editor_box,
      );
  let empty = Empty;
  let (^^) = (x, y) => Concat(x, y);
  let nestRelative = (n, x) => NestRelative(n, x);
  let nestAbsolute = (n, x) => NestAbsolute(n, x);
  let text = (cls, s) => Text(cls, s);
  let tagged = (cls, metadata, attribs, x) =>
    Concat(TagStart(cls, metadata, attribs), Concat(x, TagEnd));
  let blockBoundary = BlockBoundary;
  let optionalBreak = s => OptionalBreak(s);
  let mandatoryBreak = MandatoryBreak;
  let paletteView = (rev_path, view, hole_map, mk_editor_box) =>
    PaletteView(rev_path, view, hole_map, mk_editor_box);
  type sdoc =
    | SEmpty
    | SText(cls, string, sdoc)
    | STagStart(list(cls), option(id), option(div_attribs), sdoc)
    | STagEnd(sdoc)
    | SPaletteView(
        rev_path,
        Palettes.view_type,
        NatMap.t((HTyp.t, UHExp.block)),
        EditorBoxTypes.mk_editor_box,
        sdoc,
      )
    | SLine(int, sdoc);
  let strlen = s =>
    if (String.equal(s, "​​") || String.equal(s, "‌")) {
      0;
    } else {
      CamomileLibrary.UTF8.length(s);
    };
  let rec sdoc_of_doc' = (table, width, k, zs) =>
    switch (zs) {
    | [] => SEmpty
    | [(i, x), ...zs'] =>
      switch (x) {
      | Empty => sdoc_of_doc'(table, width, k, zs')
      | Concat(x1, x2) =>
        sdoc_of_doc'(table, width, k, [(i, x1), (i, x2), ...zs'])
      | NestRelative(n, x') =>
        sdoc_of_doc'(table, width, k, [(n + k, x'), ...zs'])
      | NestAbsolute(n, x') =>
        sdoc_of_doc'(table, width, k, [(n + i, x'), ...zs'])
      | Text(cls, s) =>
        SText(cls, s, sdoc_of_doc'(table, width, k + strlen(s), zs'))
      | TagStart(tags, metadata, attribs) =>
        let id =
          switch (metadata) {
          | Some((id, data)) =>
            Hashtbl.add(table, id, data);
            Some(id);
          | _ => None
          };
        STagStart(tags, id, attribs, sdoc_of_doc'(table, width, k, zs'));
      | TagEnd => STagEnd(sdoc_of_doc'(table, width, k, zs'))
      | BlockBoundary =>
        if (i === k) {
          sdoc_of_doc'(table, width, k, zs');
        } else {
          SLine(i, sdoc_of_doc'(table, width, i, zs'));
        }
      | OptionalBreak(s) =>
        if (width - k <= 0) {
          SLine(i, sdoc_of_doc'(table, width, i, zs'));
        } else {
          SText("space", s, sdoc_of_doc'(table, width, k + strlen(s), zs'));
        }
      | MandatoryBreak => SLine(i, sdoc_of_doc'(table, width, i, zs'))
      | PaletteView(rev_path, view, hole_map, mk_editor_box) =>
        SPaletteView(
          rev_path,
          view,
          hole_map,
          mk_editor_box,
          sdoc_of_doc'(table, width, k, zs'),
        )
      }
    };

  let sdoc_of_doc = (width, x, rev_paths) =>
    sdoc_of_doc'(rev_paths, width, 0, [(0, x)]);
};
module HTML_Of_SDoc = {
  open Tyxml_js;
  open PP;
  exception InvalidLbl;
  let rec resolve =
          (
            rev_path,
            rev_paths,
            view_monad,
            hole_map,
            mk_editor_box: EditorBoxTypes.mk_editor_box,
          )
          : Palettes.div_type =>
    Palettes.HTMLWithCells.(
      switch (view_monad) {
      | NewCellFor(lbl) =>
        switch (NatMap.lookup(hole_map, lbl)) {
        | Some((_, e)) =>
          EditorBoxTypes.(
            mk_editor_box([lbl, ...rev_path], rev_paths, e).pp_view
          )
        | None => raise(InvalidLbl)
        }
      | Bind(in_monad, f) =>
        let pp_view =
          resolve(rev_path, rev_paths, in_monad, hole_map, mk_editor_box);
        resolve(rev_path, rev_paths, f(pp_view), hole_map, mk_editor_box);
      | Ret(v) => v
      }
    );

  let rec html_of_sdoc'' = (x, rev_paths) =>
    switch (x) {
    | SEmpty => ([Html5.(span(~a=[a_class(["SEmpty"])], []))], None)
    | SText(cls, s, x') =>
      let (h, x'') = html_of_sdoc''(x', rev_paths);
      let h' = [
        Html5.(span(~a=[a_class(["SText", cls])], [txt(s)])),
        ...h,
      ];
      (h', x'');
    | STagStart(tags, id, attribs, x') =>
      let (h, x'') = html_of_sdoc''(x', rev_paths);
      let (tl, rem) =
        switch (x'') {
        | Some(x'') => html_of_sdoc''(x'', rev_paths)
        | None => ([], None)
        };
      let attrs_classes = Html5.a_class(["inline-div", ...tags]);
      let attrs_lst =
        switch (id, attribs) {
        | (Some(id), Some(attribs)) =>
          Html5.[a_id(id), attrs_classes, ...attribs]
        | (Some(id), None) => Html5.[a_id(id), attrs_classes]
        | (None, Some(attribs)) => Html5.[attrs_classes, ...attribs]
        | (None, None) => Html5.[attrs_classes]
        };
      let h' = [Html5.(div(~a=attrs_lst, h)), ...tl];
      (h', rem);
    | STagEnd(x') => ([], Some(x'))
    | SLine(n, x') =>
      let newline = Html5.br();
      let indentation =
        Html5.(
          span(
            ~a=[a_class(["SIndentation"])],
            [txt(String.make(n, ' '))],
          )
        );

      let (tl, rem) = html_of_sdoc''(x', rev_paths);
      let h = [newline, indentation, ...tl];
      (h, rem);
    | SPaletteView(rev_path, view, hole_map, mk_editor_box, x') =>
      let (tl, rem) = html_of_sdoc''(x', rev_paths);
      switch (view) {
      | Inline(view_span) =>
        /* TODO support cells inline */
        let palette_view =
          Html5.(
            div(~a=[a_class(["palette-view", "inline-div"])], [view_span])
          );
        ([palette_view, ...tl], rem);
      | MultiLine(view_div_monad) =>
        let pp_view =
          resolve(
            rev_path,
            rev_paths,
            view_div_monad,
            hole_map,
            mk_editor_box,
          );
        /* TODO WTF */
        let palette_view_2 =
          Html5.(
            div(~a=[a_class(["palette-view", "inline-div"])], [pp_view])
          );
        ([palette_view_2, ...tl], rem);
      };
    };

  let html_of_sdoc = (x, rev_paths) => {
    let (h, _) = html_of_sdoc''(x, rev_paths);
    Html5.(div(~a=[a_class(["inline-div", "SDoc"])], h));
  };
};
