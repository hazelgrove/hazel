(* An introduction to top-down zippers for structured editing *)

(* Given an algebraic data type, we can mechanically generate another type,
   typically called the zipper or the type of 'one-holed contexts' relative
   to the original type. Zippers come in top-down and bottom-up varieties.
   Here we focus on top-down zippers, the variant used in Hazel.

   Conceptually a value of this new type carries the same information
   as a corresponding value of the original type, except that it also
   distinguishes a particular subterm, which we call the 'cursor term'.

   Given a zippered type, we can also mechanically define a corresponding
   type of movement actions, each of which coresponds to a way of locally
   changing which subterm is being indicated. *)

(* A simplified AST type, representing unannotated binary trees *)
type ast =
  | Atom
  | Bin of ast * ast

(* A zippered representation of the above AST. In general, the zippered
   version of a recursive ADT will have n variants for every n-ary
   constructor (n-ary in the sense of containing n references to the
   recursive type), plus an aditional variant representing the cursor
   itself. So in our example, we have 0 variants for the 0-ary Atom,
   2 variants for the binary Bin (representing the cursor being within
   the left or right subtrees respectively), and the cursor case,
   giving 3 variants in total *)
type zast =
 | Cursor of ast
 | BinL of zast * ast
 | BinR of ast * zast

 (* Simplified local movement actions on the zippered AST, representing either moving
    the cursor up towards the root, or down the left or right branch of a Bin *)
 type action = 
 | SelectParent
 | SelectChildBinLeft
 | SelectChildBinRight

 (* To get a handle on the nature of the zippered representation, implement the
    following four functions. In doing so, you will have created almost all the
    elements of a simple structured editor using a model-view-update architecture! *)

(* 1. Convert from a non-zippered ast to zippered ast by selecting the root. Note
      that the correspondance in the direction ast -> zast is one-to-many; each
      tree has many possible cursor placements. Here we just want to do the
      simplest possible thing; in fact, the unique thing which can be done
      without inspecting the ast structure *)
 let select_root : ast -> zast = failwith "TODO"
;; assert (select_root (Bin (Atom, Atom)) = Cursor (Bin (Atom, Atom)))

(* 2. Convert from zippered ast to non-zippered ast by forgetting about the cursor.
      Note that unlike the other direction, each zippered value corresponds to
      exactly one non-zippered value *)
let erase : zast -> ast = failwith "TODO"
;; assert (erase (BinL (Cursor Atom, Atom)) = Bin (Atom, Atom))

(* 3. Simplified view function: Print a zast, using a caret (>) to indicate the cursor term *)
let view : zast -> string = failwith "TODO"
;;[ assert (view (Cursor (Bin (Atom, Atom))) = ">(A A)"),
    assert (view (BinL (Cursor Atom, Atom)) = "(>A A)"),
    assert (view (BinL (BinR (Atom, Cursor Atom), Atom)) = "((A >A) A)")]

(* 4. Simplified update function: Perform the given move action. If the action is
      inapplicable to the current cursor position, just return the zast unaltered *)
let update : action -> zast -> zast = failwith "TODO"
;; [assert (update SelectChildBinRight (BinL (Cursor (Bin (Atom, Atom)), Atom)) = (BinL (BinR (Atom, Cursor Atom), Atom))),
    assert (update SelectParent (BinL (BinR (Atom, Cursor Atom), Atom)) = (BinL (Cursor (Bin (Atom, Atom)), Atom)))]

(* Congrats! You've pretty much made a structured editor. If you want to close the loop,
   two tasks remain. First, you want to be able to actually edit trees. Second, you might
   want an interface to user the editor interactively. A simple approach here would be to
   extend the action type with construction actions InsertAtom and InsertBinWithAtoms,
   and extend the update function to replace the cursor term with Atom or Bin(Atom,Atom)
   respectively. Then you need to write a 'run' function: an indefinite loop which
   (1) converts user keyboard input to an action, (2) feeds that action to update,
   (3) passes the result to view, and (4) prints the output to the console *)