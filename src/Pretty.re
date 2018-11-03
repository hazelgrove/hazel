open Semantics.Core;
open Tyxml_js;
open Html5;
type div_attribs = list(Html5.attrib(Html_types.div_attrib));
module PP: {
  type doc('a);
  type cls = string;
  type id = string;
  let empty: doc('a);
  let (^^): (doc('a), doc('a)) => doc('a);
  let nestRelative: (int, doc('a)) => doc('a);
  let nestAbsolute: (int, doc('a)) => doc('a);
  let text: (cls, string) => doc('a);
  let tagged:
    (list(cls), option((id, 'a)), option(div_attribs), doc('a)) =>
    doc('a);
  let paletteView:
    (
      Palettes.view_type,
      NatMap.t((HTyp.t, UHExp.t)),
      UHExp.t => Palettes.div_type
    ) =>
    doc('a);
  let blockBoundary: doc('a);
  let optionalBreak: string => doc('a);
  let mandatoryBreak: doc('a);
  type sdoc =
    | SEmpty
    | SText(cls, string, sdoc)
    | STagStart(list(cls), option(id), option(div_attribs), sdoc)
    | STagEnd(sdoc)
    | SPaletteView(
        Palettes.view_type,
        NatMap.t((HTyp.t, UHExp.t)),
        UHExp.t => Palettes.div_type,
        sdoc,
      )
    | SLine(int, sdoc);
  let sdoc_of_doc: (int, doc('a)) => (sdoc, Hashtbl.t(id, 'a));
} = {
  type cls = string;
  type id = string;
  type doc('a) =
    | Empty
    | Concat(doc('a), doc('a))
    | NestRelative(int, doc('a))
    | NestAbsolute(int, doc('a))
    | Text(cls, string)
    | TagStart(list(cls), option((id, 'a)), option(div_attribs))
    | TagEnd
    | BlockBoundary
    | OptionalBreak(string)
    | MandatoryBreak
    | PaletteView(
        Palettes.view_type,
        NatMap.t((HTyp.t, UHExp.t)),
        UHExp.t => Palettes.div_type,
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
  let paletteView = (view, hole_map, mk_html_cell) =>
    PaletteView(view, hole_map, mk_html_cell);
  type sdoc =
    | SEmpty
    | SText(cls, string, sdoc)
    | STagStart(list(cls), option(id), option(div_attribs), sdoc)
    | STagEnd(sdoc)
    | SPaletteView(
        Palettes.view_type,
        NatMap.t((HTyp.t, UHExp.t)),
        UHExp.t => Palettes.div_type,
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
      | PaletteView(view, hole_map, mk_html_cell) =>
        SPaletteView(
          view,
          hole_map,
          mk_html_cell,
          sdoc_of_doc'(table, width, k, zs'),
        )
      }
    };

  let sdoc_of_doc = (width, x) => {
    let table = Hashtbl.create(64);
    let sdoc = sdoc_of_doc'(table, width, 0, [(0, x)]);
    (sdoc, table);
  };
};
module HTML_Of_SDoc = {
  open Tyxml_js;
  open PP;
  let rec html_of_sdoc'' = x =>
    switch (x) {
    | SEmpty => ([Html5.(span(~a=[a_class(["SEmpty"])], []))], None)
    | SText(cls, s, x') =>
      let (h, x'') = html_of_sdoc''(x');
      let h' = [
        Html5.(span(~a=[a_class(["SText", cls])], [pcdata(s)])),
        ...h,
      ];
      (h', x'');
    | STagStart(tags, id, attribs, x') =>
      let (h, x'') = html_of_sdoc''(x');
      let (tl, rem) =
        switch (x'') {
        | Some(x'') => html_of_sdoc''(x'')
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
            [pcdata(String.make(n, ' '))],
          )
        );

      let (tl, rem) = html_of_sdoc''(x');
      let h = [newline, indentation, ...tl];
      (h, rem);
    | SPaletteView(view, hole_map, mk_html_cell, x') =>
      let (tl, rem) = html_of_sdoc''(x');
      switch (view) {
      | Inline(view_span) =>
        let palette_view =
          Html5.(div(~a=[a_class(["inline-div"])], [view_span]));
        ([palette_view, ...tl], rem);
      | MultiLine(view_div_monad) =>
        let palette_view =
          Palettes.HTMLWithCells.resolve(
            view_div_monad,
            hole_map,
            mk_html_cell,
          );
        /* TODO WTF */
        let palette_view_2 =
          Html5.(div(~a=[a_class(["inline-div"])], [palette_view]));
        ([palette_view_2, ...tl], rem);
      };
    };

  let html_of_sdoc = x => {
    let (h, _) = html_of_sdoc''(x);
    Html5.(div(~a=[a_class(["inline-div", "SDoc"])], h));
  };
};
