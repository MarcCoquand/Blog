---
date: 2019-06-19
title: Introduction to Phantom types
author: Marc Coquand
---

A phantom type is a type which never shows up on the right hand side.

```
type cat('phantom) = Cat
```

In the above code `'phantom` can be anything. For example `let intCat: cat(int) = Cat` is a valid construction. This means that the same value can have
different types. A type can also have zero constructors, by declaring `type hungry` and never declaring what it equal. These values can never be
constructed, only declared with types. This goes well with phantom type. For
example, consider a zombie invasion. A gated community would never want to
accept any zombies, only humans that are alive. The gated community can be
entered using an enter function which takes a human as an argument. Of course a
zombie is still a human, thus at a value level they might look the same. Phantom
types allows adding additional information at the type level which can create a
"safe" enter function where no zombies are allowed.

```
type human('state) = Human
type zombie
type alive

let guard = ... /* Protect the human */

let enter = (human: human(alive)) =>
    guard(human)

// Now if a zombie attempts to enter
let badGuy: human(zombie) = Human

enter(badGuy) // Error, only alive humans allowed. Compiler saved us.
```

So phantom types can be used to encode things to the compiler when the type
information is not enough. This can be useful for separating encoded and
unencoded strings in Reasonml.

```
// Unencoded and encoded values are both strings
type str('a) = string
type unencoded
type encoded

let encode(str: str(unencoded): str(encoded) => ...
```

With phantom types, it is no longer possible to encode an encoded string, and
since the only way to create something of type `str(encoded)` is through the
`encode` function, the compiler ensures that strings are only encoded once by
using `encode`.
