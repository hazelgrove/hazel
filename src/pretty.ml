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
  val taggedText : tag -> string -> doc
  val blockBoundary : doc
  val optionalBreak : doc
  val mandatoryBreak : doc

  type sdoc = SEmpty 
            | STaggedText of tag * string * sdoc
            | SLine of int * sdoc
  val sdoc_of_doc : int -> doc -> sdoc
  val string_of_sdoc : sdoc -> string
  (* TODO: html_of_sdoc *)
end = 
struct
  type tag = string
  type doc = Empty
           | Concat of doc * doc
           | NestRelative of int * doc
           | NestAbsolute of int * doc
           | TaggedText of tag * string
           | BlockBoundary 
           | OptionalBreak
           | MandatoryBreak
  let empty = Empty
  let (^^) x y = Concat (x, y)
  let nestRelative n x = NestRelative (n, x)
  let nestAbsolute n x = NestAbsolute (n, x)
  let taggedText tag s = TaggedText (tag, s)
  let blockBoundary = BlockBoundary
  let optionalBreak = OptionalBreak
  let mandatoryBreak = MandatoryBreak

  type sdoc = SEmpty 
            | STaggedText of tag * string * sdoc
            | SLine of int * sdoc

  let rec sdoc_of_doc' width k zs = match zs with 
    | [] -> SEmpty
    | z :: zs' -> 
      begin match z with 
        | (i, Empty) -> sdoc_of_doc' width k zs'
        | (i, Concat (x1, x2)) -> 
          sdoc_of_doc' width k ((i, x1) :: (i, x2) :: zs')
        | (i, NestRelative (n, x')) -> 
          sdoc_of_doc' width k (((n + k), x') :: zs') 
        | (i, NestAbsolute (n, x')) ->
          sdoc_of_doc' width k (((n + i), x') :: zs')
        | (i, TaggedText (tag, s)) -> 
          STaggedText (tag, s, 
                       sdoc_of_doc' width (k + String.length s) zs')
        | (i, BlockBoundary) -> 
          if i == k then sdoc_of_doc' width k zs' 
          else SLine (i, sdoc_of_doc' width i zs')
        | (i, OptionalBreak) -> 
          if (width - k) <= 0 
          then 
            SLine (i, sdoc_of_doc' width i zs')
          else
            STaggedText ("space", " ", sdoc_of_doc' width k zs')
        | (i, MandatoryBreak) -> 
          SLine (i, sdoc_of_doc' width i zs')
      end 
  let sdoc_of_doc width x = sdoc_of_doc' width 0 [(0, x)]

  let rec string_of_sdoc x = match x with 
    | SEmpty -> ""
    | STaggedText(tag, s, x') -> s ^ (string_of_sdoc x')
    | SLine(n, x') -> "\n" ^ (String.make n ' ') ^ (string_of_sdoc x')
end 

module HTML_Of_SDoc = 
struct 
  open Tyxml_js
  open PP

  let rec html_of_sdoc' x = match x with 
    | SEmpty -> [Html5.(span ~a:[a_class ["SEmpty"]] [])]
    | STaggedText (tag, s, x') -> 
      (Html5.(span ~a:[a_class ["STaggedText"; tag]] [pcdata s])) 
      :: (html_of_sdoc' x')
    | SLine (n, x') -> 
      let newline = Html5.br () in 
      let indentation = Html5.(span ~a:[a_class ["SIndentation"]] [pcdata (String.make n ' ')]) in 
      newline :: indentation :: html_of_sdoc' x'

  let rec html_of_sdoc x = 
    Html5.(div ~a:[a_class ["SDoc"]] (html_of_sdoc' x))
end

