/* based closely on the paper "Strictly Pretty" by Christian Lindig
   *
   * URL: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=A7B1EF1668A1983E747286BB1A68FD19?doi=10.1.1.34.2200&rep=rep1&type=pdf
 */
module PP: {
  type doc;
  type tag = string;
  let empty: doc;
  let (^^): doc => doc => doc;
  let nestRelative: int => doc => doc;
  let nestAbsolute: int => doc => doc;
  let text: string => doc;
  let tagged: tag => doc => doc;
  let blockBoundary: doc;
  let optionalBreak: string => doc;
  let mandatoryBreak: doc;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart tag sdoc
    | STagEnd sdoc
    | SLine int sdoc;
  let sdoc_of_doc: int => doc => sdoc;
  let string_of_sdoc: sdoc => string;
} = {
  type tag = string;
  type doc =
    | Empty
    | Concat doc doc
    | NestRelative int doc
    | NestAbsolute int doc
    | Text string
    | TagStart tag
    | TagEnd
    | BlockBoundary
    | OptionalBreak string
    | MandatoryBreak;
  let empty = Empty;
  let (^^) x y => Concat x y [@implicit_arity];
  let nestRelative n x => NestRelative n x [@implicit_arity];
  let nestAbsolute n x => NestAbsolute n x [@implicit_arity];
  let text s => Text s;
  let tagged tag x => Concat (TagStart tag) (Concat x TagEnd [@implicit_arity]) [@implicit_arity];
  let blockBoundary = BlockBoundary;
  let optionalBreak s => OptionalBreak s;
  let mandatoryBreak = MandatoryBreak;
  type sdoc =
    | SEmpty
    | SText string sdoc
    | STagStart tag sdoc
    | STagEnd sdoc
    | SLine int sdoc;
  let strlen = CamomileLibrary.UTF8.length;
  /* let strlen = String.length */
  let rec sdoc_of_doc' width k zs =>
    switch zs {
    | [] => SEmpty
    | [(i, x), ...zs'] =>
      switch x {
      | Empty => sdoc_of_doc' width k zs'
      | Concat x1 x2 [@implicit_arity] => sdoc_of_doc' width k [(i, x1), (i, x2), ...zs']
      | NestRelative n x' [@implicit_arity] => sdoc_of_doc' width k [(n + k, x'), ...zs']
      | NestAbsolute n x' [@implicit_arity] => sdoc_of_doc' width k [(n + i, x'), ...zs']
      | Text s => SText s (sdoc_of_doc' width (k + strlen s) zs') [@implicit_arity]
      | TagStart tag => STagStart tag (sdoc_of_doc' width k zs') [@implicit_arity]
      | TagEnd => STagEnd (sdoc_of_doc' width k zs')
      | BlockBoundary =>
        if (i === k) {
          sdoc_of_doc' width k zs'
        } else {
          SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
        }
      | OptionalBreak s =>
        if (width - k <= 0) {
          SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
        } else {
          SText s (sdoc_of_doc' width (k + strlen s) zs') [@implicit_arity]
        }
      | MandatoryBreak => SLine i (sdoc_of_doc' width i zs') [@implicit_arity]
      }
    };
  let sdoc_of_doc width x => sdoc_of_doc' width 0 [(0, x)];
  let rec string_of_sdoc x =>
    switch x {
    | SEmpty => ""
    | SText s x' [@implicit_arity] => s ^ string_of_sdoc x'
    | STagStart tag x' [@implicit_arity] => string_of_sdoc x'
    | STagEnd x' => string_of_sdoc x'
    | SLine n x' [@implicit_arity] => "\n" ^ String.make n ' ' ^ string_of_sdoc x'
    };
};

module HTML_Of_SDoc = {
  open Tyxml_js;
  open PP;
  let rec html_of_sdoc'' x =>
    switch x {
    | SEmpty => ([Html5.(span a::[a_class ["SEmpty"]] [])], None)
    | SText s x' [@implicit_arity] =>
      let (h, x'') = html_of_sdoc'' x';
      let h' = [Html5.(span a::[a_class ["SText"]] [pcdata s]), ...h];
      (h', x'')
    | STagStart tag x' [@implicit_arity] =>
      let (h, x'') = html_of_sdoc'' x';
      let (tl, rem) =
        switch x'' {
        | Some x'' => html_of_sdoc'' x''
        | None => ([], None)
        };
      let h' = [Html5.(span a::[a_class [tag]] h), ...tl];
      (h', rem)
    | STagEnd x' => ([], Some x')
    | SLine n x' [@implicit_arity] =>
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
