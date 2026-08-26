let polymorphism_documentation = {|
  <polymorphismDocumentation>
  # Polymorphism #

# We can take types as parameters to type functions, #
# and use them in annotations in the body: #
let id = typfun A -> fun x : A -> x in

# Such functions are applied like so: #
let ex1 = id@<Int>(1) in # 1 #

# We can annotate the type of a type function with a poly. #
let const : poly A -> poly B -> A -> B -> A =
  typfun A -> typfun B -> fun x -> fun y -> x in
let ex2 = const@<Int>@<String>(2)("Hello World") in # 2 #

# We can go beyond rank 1 polymorphism: #
let apply_both : poly A -> poly B -> (poly D -> D -> D) -> (A , B) -> (A , B) =
  typfun A -> typfun B -> fun f -> fun (x, y) -> (f@<A>(x), f@<B>(y)) in
let ex3 = apply_both@<Int>@<String>(id)(3, "Hello World") in # (3, "Hello World") #

# Finally, here is a more in-depth, yet applicable example: polymorphic map #
let emptylist : poly A -> [A] = typfun A -> [] in # polymorphic constant #
let map : poly A -> poly B -> (A -> B) -> ([A] -> [B]) =
  typfun A -> typfun B -> fun f : (A -> B) -> fun l : [A] ->
    case l
    | h :: t => f(h) :: map@<A>@<B>(f)(t)
    | _ => emptylist@<B>
    end in
let ex4 = map@<Int>@<Bool>(fun x : Int -> x > 1)([1,2,3]) in # [false, true, true] #


# Recursive types #

# A type alias whose top-level operator is a sum is implicitly #
# a least fixed point on its own name: #
type MyList = Nil + Cons(Int, MyList) in

# Hazel does not (yet) support higher-kinded or existential types, #
# So we cannot implement our own polymorphic lists. #

let x : MyList = Cons(1, Cons(2, Cons(3, Nil))) in

# The implicit fixed point only applies when the sum is at top level: #
type Broken = Int -> (HasInt(Int) + HasMore(Int, Broken)) in

# Explicit fixed-point types (rec A -> ...) exist, but their #
# constructors are not registered for use — prefer the implicit form. #


let list_of_mylist : (MyList -> [Int]) = fun myl : MyList ->
  case myl
  | Nil => []
  | Cons(h, t) => h :: list_of_mylist(t)
  end in
let ex5 = list_of_mylist(x) in # [1, 2, 3] #


# All output from examples: #
(ex1, ex2, ex3, ex4, ex5)
  </polymorphismDocumentation>
|};

let sample_tic_tac_toe_program = {|
<ticTacToeProgramSample>
The following is a sample Hazel program that implements a simple Tic-Tac-Toe game using the MVU (Model-View-Update) architecture.
Note the syntax, semantics, and structure:
```
type Player =
    + X
    + O
in

type Square =
    + SPlayer(Player)
    + SEmpty
in

type Board = (Square, Square, Square, Square, Square, Square, Square, Square, Square) in

type Model = (Board, Player) in let string_of_square : Square -> String =
   fun s ->
        case s
        | SPlayer(p) =>
            (case p
            | X => " X "
            | O => " O "
            end)
        | SEmpty => "   "
        end
  in
let view : Model -> String =
    fun m ->
        let (b, _) = m in
        let (s1, s2, s3, s4, s5, s6, s7, s8, s9) = b in
        string_of_square(s1) ++ "|" ++ string_of_square(s2) ++ "|" ++ string_of_square(s3) ++ "\n" ++
        "---|---|---\n" ++
        string_of_square(s4) ++ "|" ++ string_of_square(s5) ++ "|" ++ string_of_square(s6) ++ "\n" ++
        "---|---|---\n" ++
        string_of_square(s7) ++ "|" ++ string_of_square(s8) ++ "|" ++ string_of_square(s9)
in let update : (Model, Int) -> Model =
    fun m, move ->
        let (b, p) = m in
        let (s1, s2, s3, s4, s5, s6, s7, s8, s9) = b in
        let new_board =
            case move
            | 0 => (SPlayer(p), s2, s3, s4, s5, s6, s7, s8, s9)
            | 1 => (s1, SPlayer(p), s3, s4, s5, s6, s7, s8, s9)
            | 2 => (s1, s2, SPlayer(p), s4, s5, s6, s7, s8, s9)
            | 3 => (s1, s2, s3, SPlayer(p), s5, s6, s7, s8, s9)
            | 4 => (s1, s2, s3, s4, SPlayer(p), s6, s7, s8, s9)
            | 5 => (s1, s2, s3, s4, s5, SPlayer(p), s7, s8, s9)
            | 6 => (s1, s2, s3, s4, s5, s6, SPlayer(p), s8, s9)
            | 7 => (s1, s2, s3, s4, s5, s6, s7, SPlayer(p), s9)
            | 8 => (s1, s2, s3, s4, s5, s6, s7, s8, SPlayer(p))
            | _ => b
            end
        in
        let next_player = case p | X => O | O => X end in
        (new_board, next_player)
in
let initial_model : Model =
    let empty_board = (SEmpty, SEmpty, SEmpty, SEmpty, SEmpty, SEmpty, SEmpty, SEmpty, SEmpty) in
    (empty_board, X)
in

let model1 = update(initial_model, 0) in
let model2 = update(model1, 4) in
let model3 = update(model2, 1) in
let (b, p) = model3 in

test
    (view(model3) == " X | X |   \n---|---|---\n   | O |   \n---|---|---\n   |   |   ") &&
    (p == O)
end;
```
</ticTacToeProgramSample>
|};

let sample_emoji_paint = {|
<emojiPaintMVUSample>
Below is a sample Hazel program that implements a simple Emoji Paint application using the MVU (Model-View-Update) architecture.
Note the syntax, semantics, and structure:
```
# EMOJIPAINT MVU #
type Emoji = String in
type Canvas = [[Emoji]] in
type Row = Int in
type Col = Int in
type Model = (
  canvas = Canvas,    # The 2D grid of emojis #
  brush = Emoji,      # The currently selected emoji #
  palette = [Emoji]   # The list of available emojis #
) in
type Action =
  + SetBrush(Int)         # Set the brush using a palette index #
  + PaintCell(Row, Col)   # Stamp the current emoji at the specified position #
  + ClearCell(Row, Col)   # Clear the emoji at the specified position #
  + ClearCanvas           # Clear the entire grid #
  + PaintRow(Row)         # Fill the specified row with the current emoji #
in
let init: Model = (
  # The canvas starts empty #
  canvas = [
    ["","",""],
    ["","",""],
    ["","",""]
  ],
  # Initial emoji brush #
  brush = "😄",
  # Emoji palette #
  palette = ["😄", "😅", "😆", "😉", "😊"]
) in
let setCell: (Canvas, Row, Col, Emoji) -> Canvas =
  fun (canvas, row, col, emoji) -> mapi(
    canvas,
    fun (i, r) ->
      if i == row
      then mapi(
        r,
        fun (j, c) ->
          if j == col
          then emoji
          else c)
      else r)
in
let setRow: (Canvas, Row, Emoji) -> Canvas =
  fun (canvas, targetRow, emoji) ->
    mapi(
      canvas,
      fun (i, row) ->
        if i == targetRow
        then map(row, fun _ -> emoji)
        else row)
in
let setAll: (Canvas, Emoji) -> Canvas =
  fun (canvas, emoji) ->
    map(canvas, fun r -> map(r, fun _ -> emoji))
in
let updateGrid: (Model, Canvas -> Canvas) -> Model =
  fun (m, f) ->
    (f(m.canvas), m.brush, m.palette)
in
# Update the EmojiPaint app model based on an action #
let update: (Model, Action) -> Model =
  fun (m, action) ->
    case action
    | SetBrush(emoji) =>
      (m.canvas, nth(m.palette, emoji), m.palette)
    | PaintCell(row, col) =>
      updateGrid(m, fun c -> setCell(c, row, col, m.brush))
    | ClearCell(row, col) =>
      updateGrid(m, fun c -> setCell(c, row, col, ""))
    | ClearCanvas =>
      updateGrid(m, fun c -> setAll(c, ""))
    | PaintRow(row) =>
      updateGrid(m, fun c -> setRow(c, row, m.brush))
    end
in
let do = fun (init: Model, actions: [Action]) ->
  fold_left(actions, update, init)
in
let actions = [
  ClearCanvas,
  SetBrush(1),
  PaintCell(1, 1),
  SetBrush(3),
  PaintRow(0)
] in
do(init, actions)
```
</emojiPaintMVUSample>
|};
