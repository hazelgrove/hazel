# Zipper transitions

When the Zipper.t datatype changes, exercises become ill-typed.

To do a transition to a new Zipper datatype:

1. Load each exercise in the previous version of Hazel.
2. With Instructor Mode enabled, click "Export Transitionary Exercise Module" in the top bar. This will produce a version of the exercise with string components (not secure for student release).
3. Put these modules into the new version of Hazel and build and load.
4. Go through each newly loaded exercise and click "Export Exercise Module" in the top bar. This will restore a version of the exercise with Zipper components using the new Zipper datatype. Replace and you are done.

NOTE: the text parser is a little quirky with spacing around
holes sometimes, so you may want to check initial states 
to make sure they look right before step 4.