module Vdom = Virtual_dom.Vdom;
module Attr = Vdom.Attr;

// let deserialize = s => ZExp.t_of_sexp(Sexplib.Sexp.of_string(s));

let div = Vdom.Node.div;
let span = Vdom.Node.span;
let txt = Vdom.Node.text;
let p = Vdom.Node.p;
let ptxt = s => p([], [txt(s)]);

let code = s => span([Vdom.Attr.classes(["code"])], [txt(s)]);
let keyblock = s => span([Vdom.Attr.classes(["keyblock"])], [txt(s)]);

let intro_caption =
  div(
    [],
    [
      txt(
        "Welcome to Hazel, a live functional programming environment organized around typed holes.",
      ),
      ptxt("Below is our very first hole, numbered automatically as shown."),
      p(
        [],
        [
          txt(
            "Fill hole 1 with a number literal by placing the caret before or after the hole and typing in ",
          ),
          code("12345"),
          txt("."),
        ],
      ),
      ptxt("Click Next to go to the next card once you are finished."),
    ],
  );
let intro_init_zexp =
  ZExp.ZBlock.wrap(CursorE(OnDelim(0, Before), EmptyHole(0)));
// deserialize("(BlockZE()(CursorE(OnDelim 0 Before)(EmptyHole 0)))");
/*
 let intro_card: CardInfo.t = {
   caption: intro_caption,
   init_zexp: intro_init_zexp,
 };
 */
// let intro_card: CardInfo.t = {
//   name: "intro",
//   caption: div([], []),
//   init_zexp: intro_init_zexp,
// };
/*
 let backspace_caption =
   div(
     [],
     [
       txt("You can use "),
       keyblock("Backspace"),
       txt(" and "),
       keyblock("Delete"),
       txt(
         " to delete digits. Once you've deleted all of the digits, a hole again appears!",
       ),
       ptxt("Go ahead and delete all of the digits below to try it out."),
     ],
   );
 let backspace_init_zblock =
   // deserialize("(BlockZE()(CursorE(OnText 5)(IntLit NotInHole 12345)))");
 let backspace_card: CardInfo.t = {
   caption: backspace_caption,
   init_zexp: backspace_init_zblock,
 };

 let empty_hole_insertion_caption =
   div(
     [],
     [
       txt(
         "Hazel automatically inserts holes wherever a program term is expected "
         ++ "but you haven't yet provided one.",
       ),
       p(
         [],
         [
           txt("Try entering "),
           code("12345"),
           txt(" again, then press "),
           keyblock("+"),
           txt("."),
         ],
       ),
       p(
         [],
         [
           txt("Since the "),
           code("+"),
           txt(
             " operator requires two arguments, Hazel automatically inserts "
             ++ " a hole for the second argument.",
           ),
         ],
       ),
     ],
   );
 let empty_hole_insertion_init_zblock =
   deserialize("(BlockZE()(CursorE(OnDelim 0 Before)(EmptyHole 0)))");
 let empty_hole_insertion_card: CardInfo.t = {
   caption: empty_hole_insertion_caption,
   init_zexp: empty_hole_insertion_init_zblock,
 };
 */

// let cardstack: CardstackInfo.t = {
//   title: "Hazel Tutorial",
//   cards: [intro_card /*backspace_card, empty_hole_insertion_card */],
// };
