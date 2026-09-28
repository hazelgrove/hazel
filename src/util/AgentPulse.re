/* A projector just committed new syntax (a livelit gesture, a slider,
   a checkbox): a discrete edit, not a keystroke in a burst of typing,
   so the statics debounce that smooths typing only adds latency to the
   interaction. Set by ProjectorPerform, consumed by the debounce. */
let projector_commit: ref(bool) = ref(false);
