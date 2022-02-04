let compile = (d: DHExp.t) => {
  d |> Translator.translate |> Emit.emit;
};
