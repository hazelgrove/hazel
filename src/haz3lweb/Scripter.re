open Haz3lcore;
let sketch_slide = 3;
let fill_marker = "FILL_ME";
//TODO(andrew): adjust promptgen to replace this with ??

let tests_raw = [
  ("one", "let lol = FILL_ME in lol + 2000"),
  ("two", "let lol: Int = FILL_ME in lol + 2000"),
];

let is_fill_marker: Piece.t => bool =
  fun
  | Tile({label: [t], _}) => t == fill_marker
  | _ => false;

let mk_script = (sketch_str: string): list(UpdateAction.t) => {
  [
    SwitchSlide(sketch_slide),
    PerformAction(Move(Extreme(Up))),
    PerformAction(Select(Resize(Extreme(Down)))),
    Paste(sketch_str),
    PerformAction(Move(Goal(Piece(is_fill_marker, Left)))),
    PerformAction(Select(Term(Current))),
    Paste(Form.expliciter_hole),
    PerformAction(Select(Term(Current))),
    Agent(Prompt(Filler)),
  ];
};

let test_scripts =
  List.map(
    ((name, sketch_str)) => (name, mk_script(sketch_str)),
    tests_raw,
  );

/*

 get timestamp at beginning of test
 and at end of test
 log if we send errors back

 indep vars:
 parametrize Filler.prompt to take a fn
 from model? -> prompt
 also want to be able to vary GPT 3.5/4


 1. create entry in model to store test results
 when we begin a run, we must specify a list of scripts
 each must have some kind of text description saying what were varying (think about more structure)
 test ledger in model is initially an empty list
 after each test, we cons the result
 and start the next test
 we use the length of the list to tell what test we're on,
 and stop when we've finished all the tests
 at which point we record the results to localstore, using some kind of timestamp in the key (so as not to overwrite)d
  */
