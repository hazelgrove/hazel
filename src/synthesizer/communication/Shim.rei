/*
    The goal is to convert a UHExp into an exp.

    This requires conversion of lines into expressions.
 */

let hazelToSynthesizer: UHExp.t => Synthesiscore.Types.input;

//TODO this return type might need to change:
let synthesizerToHazel:
  Synthesiscore.Types.output => Map.Make(Int).t(UHExp.operand);
