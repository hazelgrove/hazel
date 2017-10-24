/**
 * Module that can be depended on by serialized hazelnut logic.
 */
type num = int;

type sum 'l 'r =
  | L 'l
  | R 'r;
/**
 * This module defines no types or values for holes, ensuring that Parsetrees
 * with holes can't actually compile. But this comment documents what hole
 * values in the Parsetree look like:
 * type hole =
 *   | EHole int
 *   | NEHole int 'a;
 */
