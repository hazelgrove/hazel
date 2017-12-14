/* based closely on the paper "Strictly Pretty" by Christian Lindig
   *
   * URL: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=A7B1EF1668A1983E747286BB1A68FD19?doi=10.1.1.34.2200&rep=rep1&type=pdf
 */
module PP: {
  type doc 'a;
  type cls = string;
  type id = string;
  let empty: doc 'a;
  let (^^): doc 'a => doc 'a => doc 'a;
  let nestRelative: int => doc 'a => doc 'a;
  let nestAbsolute: int => doc 'a => doc 'a;
  let text: string => doc 'a;
  let tagged: cls => option (id, 'a) => doc 'a => doc 'a;
  let blockBoundary: doc 'a;
  let optionalBreak: string => doc 'a;
  let mandatoryBreak: doc 'a;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart cls (option id) sdoc
    | STagEnd sdoc
    | SLine int sdoc;
  let sdoc_of_doc: int => doc 'a => (sdoc, Hashtbl.t id 'a);
  let string_of_sdoc: sdoc => string;
} = {
  type cls = string;
  type id = string;
  type doc 'a =
    | Empty
    | Concat (doc 'a) (doc 'a)
    | NestRelative int (doc 'a)
    | NestAbsolute int (doc 'a)
    | Text string
    | TagStart cls (option (id, 'a))
    | TagEnd
    | BlockBoundary
    | OptionalBreak string
    | MandatoryBreak;
  let empty = Empty;
  let (^^) x y => Concat x y;
  let nestRelative n x => NestRelative n x;
  let nestAbsolute n x => NestAbsolute n x;
  let text s => Text s;
  let tagged cls metadata x => Concat (TagStart cls metadata) (Concat x TagEnd);
  let blockBoundary = BlockBoundary;
  let optionalBreak s => OptionalBreak s;
  let mandatoryBreak = MandatoryBreak;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart cls (option id) sdoc
    | STagEnd sdoc
    | SLine int sdoc;
  let strlen = CamomileLibrary.UTF8.length;
  /* let strlen = String.length */
  let rec sdoc_of_doc' table width k zs =>
    switch zs {
    | [] => SEmpty
    | [(i, x), ...zs'] =>
      switch x {
      | Empty => sdoc_of_doc' table width k zs'
      | Concat x1 x2 => sdoc_of_doc' table width k [(i, x1), (i, x2), ...zs']
      | NestRelative n x' => sdoc_of_doc' table width k [(n + k, x'), ...zs']
      | NestAbsolute n x' => sdoc_of_doc' table width k [(n + i, x'), ...zs']
      | Text s => SText s (sdoc_of_doc' table width (k + strlen s) zs')
      | TagStart tag metadata =>
        let id =
          switch metadata {
          | Some (id, data) =>
            Hashtbl.add table id data;
            Some id
          | _ => None
          };
        STagStart tag id (sdoc_of_doc' table width k zs')
      | TagEnd => STagEnd (sdoc_of_doc' table width k zs')
      | BlockBoundary =>
        if (i === k) {
          sdoc_of_doc' table width k zs'
        } else {
          SLine i (sdoc_of_doc' table width i zs')
        }
      | OptionalBreak s =>
        if (width - k <= 0) {
          SLine i (sdoc_of_doc' table width i zs')
        } else {
          SText s (sdoc_of_doc' table width (k + strlen s) zs')
        }
      | MandatoryBreak => SLine i (sdoc_of_doc' table width i zs')
      }
    };
  let sdoc_of_doc width x => {
    let table = Hashtbl.create 64;
    let sdoc = sdoc_of_doc' table width 0 [(0, x)];
    (sdoc, table)
  };
  let rec string_of_sdoc x =>
    switch x {
    | SEmpty => ""
    | SText s x' => s ^ string_of_sdoc x'
    | STagStart _ _ x' => string_of_sdoc x'
    | STagEnd x' => string_of_sdoc x'
    | SLine n x' => "\n" ^ String.make n ' ' ^ string_of_sdoc x'
    };
};

module HTML_Of_SDoc = {
  open Tyxml_js;
  open PP;
  let rec html_of_sdoc'' x =>
    switch x {
    | SEmpty => ([Html5.(span a::[a_class ["SEmpty"]] [])], None)
    | SText s x' =>
      let (h, x'') = html_of_sdoc'' x';
      let h' = [Html5.(span a::[a_class ["SText"]] [pcdata s]), ...h];
      (h', x'')
    | STagStart cls id x' =>
      let (h, x'') = html_of_sdoc'' x';
      let (tl, rem) =
        switch x'' {
        | Some x'' => html_of_sdoc'' x''
        | None => ([], None)
        };
      let attrs_lst =
        switch id {
        | Some id => Html5.[a_id id, a_class [cls]]
        | None => Html5.[a_class [cls]]
        };
      let h' = [Html5.(span a::attrs_lst h), ...tl];
      (h', rem)
    | STagEnd x' => ([], Some x')
    | SLine n x' =>
      let newline = Html5.br ();
      let indentation = Html5.(span a::[a_class ["SIndentation"]] [pcdata (String.make n ' ')]);
      let (tl, rem) = html_of_sdoc'' x';
      let h = [newline, indentation, ...tl];
      (h, rem)
    };
  let rec html_of_sdoc x => {
    let (h, _) = html_of_sdoc'' x;
    Html5.(div a::[a_class ["SDoc"]] h)
  };
};
