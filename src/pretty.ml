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

  exception TODO
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

 (*
  type mode = Flat
            | Breaks

  let rec fits w zs = 
    if w < 0 
    then false 
    else begin match zs with 
      | [] -> true
      | z :: zs' -> 
        begin match z with 
          | (i, m, Empty) -> fits w zs'
          | (i, m, Concat(x1, x2)) -> 
            fits w ((i, m, x1) :: (i, m, x2) :: zs')
          | (i, m, Text s) -> fits (w - String.length s) zs'
          | (i, m, Nest (n, x)) -> fits w ((i + n, m, x) :: zs')
          | (i, Flat, Break s) -> fits (w - String.length s) zs'
          | (i, Breaks, Break _) -> true
          | (i, m, Group x) -> fits w ((i, Flat, x) :: zs')
        end
    end

  let rec format w k zs = match zs with 
    | [] -> SEmpty
    | z :: zs' -> 
      begin match z with 
        | (i, m, Empty) -> format w k zs'
        | (i, m, Concat(x1, x2)) -> format w k ((i, m, x1) :: (i, m, x2) :: zs')
        | (i, m, Text s) -> SText(s, format w (k + String.length s) zs')
        | (i, m, Nest(n, x)) -> format w k ((n + i, m, x) :: zs')
        | (i, Flat, Break s) -> SText(s, format w (k + String.length s) zs')
        | (i, Breaks, Break s) -> SLine(i, format w i zs')
        | (i, m, Group x) -> 
          if fits (w - k) ((i, Flat, x) :: zs') 
          then format w k ((i, Flat, x) :: zs')
          else format w k ((i, Breaks, x) :: zs')
      end

  let rec pretty w x = string_of_sdoc (format w 0 [(0, Flat, Group x)]) *)
end
(* 
module PPTest = struct 
  let (^^) = PP.(^^)
  let arg = PP.text "abcde"
  let comma = PP.text ","
  let break = PP.break 
  let arg_group = (arg ^^ comma ^^ break)
  (* let ap  = (PP.text "f(") ^^ (PP.nest 2 
     (PP.group (PP.group (PP.group (PP.group (PP.group (PP.group (PP.group (
      arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg ^^ comma ^^ break) ^^ arg)
     ) ^^ (PP.text ")") *)

  let ap = (PP.text "f(") ^^ (PP.nest 2 (PP.group (arg_group ^^ arg_group ^^ arg_group ^^ arg_group ^^ arg_group ^^ arg_group ^^ arg_group ^^ arg)) ^^ (PP.text ")"))

  let _ = print_endline (PP.pretty 30 ap) 
  let _ = print_endline (PP.pretty 80 ap)
end
*)
