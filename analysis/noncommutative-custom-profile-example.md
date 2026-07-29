# Noncommutative custom-profile example

This proof of concept is assembled from the existing Algebra catalog. It is
not a new math level and does not add expression-specific matrix rules.

The test profile is created with:

```reason
let profile =
  Axioms.profile_with_capability_disabled(
    Axioms.math_profile(Axioms.Algebra),
    "mul.comm",
  );
```

`profile_with_capability_disabled` removes multiplication commutativity from
semantic cleanup and compiles it to `Disabled` for One Step, Check Result, and
Auto Simplify. Other inherited Algebra capabilities remain available.

## Expressions to try

These are accepted in both One Step and Check Result:

```text
(a*b)*c  -> a*(b*c)
a*(b+c)  -> a*b+a*c
(a+b)*c  -> a*c+b*c
a*1      -> a
```

These are rejected because they require multiplication commutativity:

```text
a*b      -> b*a
(a*b)*c  -> (c*b)*a
a*(b+c)  -> b*a+c*a
```

The implementation regression test is named
`custom noncommutative profile is enforced by the shared authorizer` in
`test/Test_RewriteChecker.re`. It also verifies that a fixed Auto Simplify
candidate and the same manually entered Check Result target receive identical
capability counts and profile fingerprints.
