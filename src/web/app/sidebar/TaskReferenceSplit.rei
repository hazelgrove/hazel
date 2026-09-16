/* Cutting a Task Reference document into the collapsible sections the
   sidebar renders. A `###` heading opens a section; anything before the
   first one is an unnamed preamble. */

/* The document's blocks, grouped in document order, each group paired with
   the heading that opened it -- None for the preamble. Headings of other
   levels are ordinary blocks and do not cut. An empty preamble is dropped,
   but a heading with nothing under it still names an empty group. */
let split: Omd.doc => list((option(string), Omd.doc));
