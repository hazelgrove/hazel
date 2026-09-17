/* The naming convention on the Fumola/Hazel boundary, in one place.

   Fumola inherits Motoko's convention: a variant tag is lowerCamelCase, as
   `#leaf`, `#addNode`, `#forceBegin`. Hazel's constructors are UpperCamelCase,
   as `Leaf`, `AddNode`, `ForceBegin`. So every value crossing the boundary has
   its first letter recased, and that has to happen in BOTH directions or the
   crossing is not a round trip.

   It was not. FumolaValue capitalised on the way in and FumolaSource left the
   name alone on the way out, so a value read from Fumola as `#leaf`, shown in
   Hazel as `Leaf`, went back as `#Leaf` -- a different tag, silently. Nothing
   caught it because neither direction was tested against the other.

   This is the narrowest version of a problem that recurs wherever these
   languages meet, Motoko and Rust included: three conventions (lowerCamel,
   UpperCamel, snake_case) and no single answer about which crossing is
   lossless. What makes THIS crossing tractable is that only the first letter
   moves, so the two functions are inverse whenever the name starts with an
   ASCII letter of the expected case -- and `round_trips` says when they are
   not, rather than leaving a caller to find out by being wrong. */

/* A Fumola tag as Hazel writes the constructor: `leaf` -> `Leaf`. */
let to_hazel = (tag: string): string => String.capitalize_ascii(tag);

/* A Hazel constructor as Fumola writes the tag: `Leaf` -> `leaf`. */
let to_fumola = (ctr: string): string => String.uncapitalize_ascii(ctr);

/* Whether this name survives the crossing unchanged.

   It does not when the Fumola tag already begins with an upper-case letter --
   `#Leaf` becomes `Leaf` becomes `#leaf` -- or when it begins with anything
   that has no case at all, such as a digit or an underscore, where recasing is
   a no-op in one direction and the two names coincide anyway.

   The first case is the one that loses information, and a caller that cares
   should ask rather than assume. */
let round_trips = (tag: string): bool => to_fumola(to_hazel(tag)) == tag;
