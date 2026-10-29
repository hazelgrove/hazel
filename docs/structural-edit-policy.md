# Structural edits, normalization, and identity

Agent tools and outline commands should be front ends to scoped structural
operations. The operation's target determines what it may replace; formatting
and presentation are not permissions to rewrite unrelated syntax.

## Scope

Prefer, in order: the particular nested definition or module member, its
containing definition, then a deliberately broader replacement. Insert new
definitions with insertion operations. Move existing definitions as existing
syntax, not as printed text parsed back into a new definition.

For a splice, the writable region consists of the selected syntax and the joins
created at its boundaries. Rebuilding the ancestor path is allowed; changing
unrelated siblings is not. Completion of incomplete syntax can require a wider
region, but that should be a consequence of the structural operation, not a
whole-document cleanup policy.

`update_body` currently means **all syntax following the selected binding's
`in`**, including subsequent definitions and tests. It remains available for
existing clients and trajectory recordings. It is not an append operation and
must not be recommended for routine definition insertion. Replacing only the
terminal expression currently requires targeting the last binding after
inspecting its body.

The intended replacements are operations to insert/update/delete a test or
statement and to replace a block's terminal expression. These operations are
not implemented by this cleanup. Once they cover existing uses, retire broad
body replacement from the preferred agent interface; preserve compatibility
for old recordings rather than silently redefining their meaning.

## Identity guarantees

There are three different questions:

* **Content:** did syntax, formatting, comments, or widget state change?
* **Durable identity:** is this the same syntax entity, with the same ID?
* **Object sharing:** can incremental consumers reuse the same immutable object?

Outside the edit and its changed joins, preserve all three. An unchanged
subtree should keep its IDs and objects. A changed ancestor can keep its ID
while receiving a new object. Moving syntax retains its identity; duplication
must allocate fresh identities for the copy.

Inside an explicitly replaced region, reuse is conservative and best-effort:
an identical replacement reuses ordinary syntax; equal prefixes and suffixes
can survive; compatible changed constructs can reuse unchanged children.
Changed constructs receive the parser's new IDs. There is no global search
for a matching printed definition, no semantic-equivalence inference, and no
promise to recover identity across an arbitrary reorder or wholesale rewrite.
The matched ranges are disjoint, so repeated equal values do not cause the
same old ID to be assigned to two new occurrences.

Stateful syntax projectors are an exception to textual matching. The same
printed expression is insufficient to establish that two widgets are the same
instance. Preserve existing instances through narrow edits; do not transplant
a newly parsed widget onto an old instance based only on its printed syntax.

## Normalization

Normalize incoming material and changed joins. Preserve existing whitespace
runs, comments, and separator spacing elsewhere. Reuse newline and indentation
pieces where possible and allocate only missing pieces. Normalizing an already
normal, unchanged program must be a no-op for IDs and immutable objects as well
as for text.

Current implementation uses the pre-edit program to identify surviving
boundaries. Module separators require the owning member as a boundary witness:
the same semicolon can be reused after inserting a different member, and a
member's RHS can change without changing the surrounding member boundary.
Indentation and zipper mapping preserve unchanged records too.

This is bounded **mutation**, not yet an edit-footprint performance guarantee.
The implementation still scans/indexes syntax to discover changes. Moving to an
explicit operation footprint would avoid that work and make the scope easier
to validate. Ordinary refactoring's existing auto-indent policy is not changed
by this cleanup. The explicit projector-trigger path and no-path insertion
also still invoke program-wide `Materialize.all`; restricting completion to
an operation footprint is a remaining task, not a guarantee established here
for pre-existing incomplete syntax.

## Presentation

Accepted structural operations are authoritative. Their presentation can be
metered by definition, but must converge to the exact accepted model.
Identity-only repairs travel with the preceding visible step. If an entire
accepted change is identity-only, publish one synchronization step, not one
animation per unchanged definition. Content comparison includes comments and
whitespace and treats widget instances conservatively.

## Remaining convergence work

The agent path (`CompositionGo`) and outline path (`ScratchRestructure`) still
have separate splice/separator mechanics. Their existing item/spine structures
are a useful foundation, but shared addressing is not a shared mutation engine.

The next architectural step is a core edit program with explicit targets and
footprints: insert/delete/move/replace an item, replace a definition or pattern,
edit a test, replace a terminal expression. Agent and outline adapters should
both use it. A batch can validate atomically while retaining its constituent
operations for presentation. Reuse the existing efficient splices; do not
implement this by printing and reparsing an enclosing block.

Tests should cover resulting syntax and statics, surviving IDs and objects,
fresh identities for copies, unchanged formatting outside scope, and exact
reconstruction of accepted presentation state. `Test_EditIdentity`, the agent
tool and outline suites, and `Test_CanvasPresentation` cover the first cleanup.
