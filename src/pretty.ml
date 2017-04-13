(* based closely on the paper "Strictly Pretty" by Christian Lindig 
  *
  * URL: http://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=A7B1EF1668A1983E747286BB1A68FD19?doi=10.1.1.34.2200&rep=rep1&type=pdf
*)
module PP : 
sig 
  type doc
  type tag = string
  val empty : doc
  val (^^) : doc -> doc -> doc
  val nestRelative : int -> doc -> doc
  val nestAbsolute : int -> doc -> doc
  val text : string -> doc 
  val tagged : tag -> doc -> doc
  val blockBoundary : doc
  val optionalBreak : string -> doc
  val mandatoryBreak : doc

  type sdoc = SEmpty 
            | SText of string * sdoc
            | STagStart of tag * sdoc
            | STagEnd of sdoc
            | SLine of int * sdoc
  val sdoc_of_doc : int -> doc -> sdoc
  val string_of_sdoc : sdoc -> string
end = 
struct
  type tag = string
  type doc = Empty
           | Concat of doc * doc
           | NestRelative of int * doc
           | NestAbsolute of int * doc
           | Text of string
           | TagStart of tag
           | TagEnd
           | BlockBoundary 
           | OptionalBreak of string
           | MandatoryBreak
  let empty = Empty
  let (^^) x y = Concat (x, y)
  let nestRelative n x = NestRelative (n, x)
  let nestAbsolute n x = NestAbsolute (n, x)
  let text s = Text s
  let tagged tag x = Concat (TagStart tag, Concat (x, TagEnd))
  let blockBoundary = BlockBoundary
  let optionalBreak s = OptionalBreak s 
  let mandatoryBreak = MandatoryBreak

  type sdoc = SEmpty 
            | SText of string * sdoc
            | STagStart of tag * sdoc
            | STagEnd of sdoc
            | SLine of int * sdoc

  let strlen = CamomileLibrary.UTF8.length
  (* let strlen = String.length *) 

  let rec sdoc_of_doc' width k zs = match zs with 
    | [] -> SEmpty
    | (i, x) :: zs' -> 
      begin match x with 
        | Empty -> sdoc_of_doc' width k zs'
        | Concat (x1, x2) -> 
          sdoc_of_doc' width k ((i, x1) :: (i, x2) :: zs')
        | NestRelative (n, x') -> 
          sdoc_of_doc' width k (((n + k), x') :: zs') 
        | NestAbsolute (n, x') ->
          sdoc_of_doc' width k (((n + i), x') :: zs')
        | Text s -> 
          SText (s, sdoc_of_doc' width (k + (strlen s)) zs')
        | TagStart tag -> 
          STagStart (tag, sdoc_of_doc' width k zs')
        | TagEnd -> 
          STagEnd (sdoc_of_doc' width k zs')
        | BlockBoundary -> 
          if i == k then sdoc_of_doc' width k zs' 
          else SLine (i, sdoc_of_doc' width i zs')
        | OptionalBreak s -> 
          if (width - k) <= 0 
          then 
            SLine (i, sdoc_of_doc' width i zs')
          else
            SText (s, sdoc_of_doc' width (k + (strlen s)) zs')
        | MandatoryBreak -> 
          SLine (i, sdoc_of_doc' width i zs')
      end 
  let sdoc_of_doc width x = sdoc_of_doc' width 0 [(0, x)]

  let rec string_of_sdoc x = match x with 
    | SEmpty -> ""
    | SText (s, x') -> s ^ (string_of_sdoc x')
    | STagStart (tag, x') -> string_of_sdoc x'
    | STagEnd x' -> string_of_sdoc x'
    | SLine(n, x') -> "\n" ^ (String.make n ' ') ^ (string_of_sdoc x')
end 

module HTML_Of_SDoc = 
struct 
  open Tyxml_js
  open PP

  let rec html_of_sdoc'' x = match x with 
    | SEmpty -> ([Html5.(span ~a:[a_class ["SEmpty"]] [])], None)
    | SText (s, x') -> 
      let (h, x'') = html_of_sdoc'' x' in 
      let h' = (Html5.(span ~a:[a_class ["SText"]]
                         [pcdata s])) :: h in 
      (h', x'')
    | STagStart (tag, x') -> 
      let (h, x'') = html_of_sdoc'' x' in 
      let (tl, rem) = 
        begin match x'' with 
          | Some x'' -> 
            html_of_sdoc'' x'' 
          | None -> ([], None)
        end in 
      let h' = (Html5.(span ~a:[a_class [tag]] h)) :: tl in 
      (h', rem)
    | STagEnd x' -> 
      ([], Some x')
    | SLine (n, x') -> 
      let newline = Html5.br () in 
      let indentation = Html5.(span ~a:[a_class ["SIndentation"]]
                                 [pcdata (String.make n ' ')]) in 
      let (tl, rem) = html_of_sdoc'' x' in 
      let h = newline :: indentation :: tl in 
      (h, rem)
  let rec html_of_sdoc x = 
    let (h, _) = html_of_sdoc'' x in 
    Html5.(div ~a:[a_class ["SDoc"]] h)
end

