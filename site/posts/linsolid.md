---
date: 2019-07-16
title: SOLID, Interface Segregation Principle
author: Marc Coquand
---

This the third part of a series on SOLID principles applied to functional
principles. Today's topic is Interface Segregation Principle. The
principle states

<q>No client should be forced to depend on methods it does not use. The general
idea is that you want to split big interfaces to smaller, specific ones.</q>

In Functional programming, we can use type classes and separate those into
smaller type classes as a form of dependency inversion and call that interface
segregation principle but I think that is not exciting. Instead, I will
translate the principle to <q>Interface Segregation Principle states that the
smallest set of data should be used for each function to work</q>. How does
this definition relate to interface segregation principle? To figure this out,
we need to think of types as sets.

### Types as Sets

A type, such as Int or Bool, can be thought of as a set of possible values it
can yield. So for example `type Bool = {True, False}`, `type Int = {-∞,...,-2,-1,0,1,2...,+∞}`. In set theory, the _cardinality_ of a type is the
amount of values it can yield. So for Bool the cardinality is 2 since it can
either yield True or False. For Int the cardinality is ∞.

This becomes useful when picking the data representation for our
structures. For example how might we represent Colors, here are some
suggestions:

```
type Color = Blue | Red | Green
-- alternatively
type Color2 = {Blue: Bool, Red : Bool, Green: Bool}
```

Which representation is best? Well let's look at the second representation
`Color2`. This representation works fine for values such as `{Blue = True, Red = False, Green = False}`, but what about `{Blue = True, Red= True, Green = False}`? We have created a value that should be impossible! The reason is
because the _cardinality_ of `Color2` is `2×2×2=8`. You might have heard the
name product types for records, the reason is because their cardinality is the
product of their members. Meanwhile the type `Color` only has `1+1+1=3` possible
values it can be (Red, green or blue). For this reason Color is called a sum
type, as the cardinality is the sum of it's members. By choosing the right data
representation, Color, we can greatly reduce the amount of bugs in our software.

The full table for cardinalities is as follows (taken from
[here](https://codewords.recurse.com/issues/three/algebra-and-calculus-of-algebraic-data-types#fn:answer)).

| Haskell                           | Math | Notes                                                                              |
| --------------------------------- | ---- | ---------------------------------------------------------------------------------- |
| `data void`                       | 0    | Impossible to construct                                                            |
| `data () = ()`                    | 1    | Only one value possible                                                            |
| `data Bool = True | False`        | 2    | Sum of two values, hence why<br> these are sometimes called union<br> types        |
| `data Maybe a = Just a | Nothing` | a+1  | Sum of whatever a is and 1                                                         |
| `data Either = Right a | Left b`  | a+b  |                                                                                    |
| `data (a,b) = (a,b)`              | a×b  | Both types cardinality combined.<br> Hence why these are called<br> product types. |
| `a -> b`                          | b^a  |                                                                                    |

Not only can we represent Int, Bool and Color as sets. Advanced types
such as `type Computation a b = a -> b` can also be represented as sets. The
cardinality of Computation is `|a|^|b|`.

This has implications for Interface Segregation principle. Given a user
library where we can store and fetch users, one might write

```
data = IUserRepo {
    getUser : Id -> IO User,
    storeUser : User -> Id -> IO ()
    }

getUserEndpoint : IUserRepo -> Request -> Response
-- ... Do stuff
```

`getUserEndpoint` will never use storeUser, but it is still included in the
getUserEndpoint as an argument. It's cardinality becomes

```
|getUser| × |storeUser| = |Id|^|(IO User)| × |User|^|Id|^1
```

In Haskell, the abstraction for `IO a` is `IO a = Realworld -> (a, Realworld)`
so the cardinality is `(a × Realworld) ^ Realworld` where `Realworld` would be
all the bits of the system (so a very big number). The cardinality of () is 1.
If instead `IUserRepo` is segregated into two, the cardinality of
`getUserEndpoint` is greatly reduced.

```
data IGetUser = {getUser : Id -> IO User}
data IStoreUser = {storeUser : User -> Id -> IO ()}

getUserEndpoint : IGetUser -> Request -> Response
-- ... Do stuff
```

By splitting them up, we have greatly reduced the amount of possible values that
the `getUserEndpoint` can represent and also segregated the interfaces so that
`IStoreUser` can be modified without `IGetUser` being affected.

Cardinality is not a perfect tool for analysis due to many sets being infinite.
But it can be a useful tool for glossing over which data structure to use.

So in Functional programming, the interface segregation principle can be
broadened to apply to all values and types, not just interfaces. So in that
sense that principles has been extended to apply to all data we deal with and
how we choose to represent it.
