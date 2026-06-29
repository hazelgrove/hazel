open Haz3lcore;
open ProjectorViewBase;

/* The web view registry: pairs each projector kind with its Vdom view
 * module (the logic halves are registered in core ProjectorInit).
 * After adding a new projector view module, register it here. */

let to_module = (kind: ProjectorCore.Kind.t): (module CookedView) =>
  switch (kind) {
  | Fold => (module CookView(FoldProjView.V))
  | Statics => (module CookView(TypeProjView.V))
  | Probe => (module CookView(ProbeProjView.V))
  | Slider => (module CookView(SliderProjView.V))
  | SliderF => (module CookView(SliderFProjView.V))
  | Checkbox => (module CookView(CheckboxProjView.V))
  | TextArea => (module CookView(TextAreaProjView.V))
  | Livelit => (module CookView(LivelitProjView.V))
  | Card => (module CookView(CardProjView.V))
  | Table => (module CookView(TableProjView.V))
  | Csv => (module CookView(CSVProjectorView.V))
  | Seed => (module CookView(SeedProjView.V))
  };

/* Install web focus behavior into the core registry (consulted by
 * ProjectorPerform's Focus action). Called once at startup. */
let install = (): unit =>
  ProjectorBase.focusables :=
    (
      kind => {
        let (module CV) = to_module(kind);
        CV.focusable;
      }
    );
