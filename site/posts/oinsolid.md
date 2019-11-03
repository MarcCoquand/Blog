---
date: 2019-07-15
title: SOLID, Open/Closed Principle
author: Marc Coquand
---

This is second part of a series about functional programming and SOLID
principles. This post will touch the O in SOLID, Open/Closed principle.

Open/Closed principle states that software entities (classes, modules,
functions, etc.) should be open for extension, but closed for modification.

The OCP is an advice on how to write modules in such a way that we have
backwards compatibility and so that if extra functionality is needed, the
modifier does not need to look at the class in order to make modifications. So
if a class has some new requirements you do not need to modify the source code
but can instead extend the superclass.

When we apply this principle into Functional programming, we run into a well
known problem called the _expression problem_. The expression problem used to be
one of the strongest arguments against the use of functional programming! It
states

<q>The goal is to define a datatype by cases, where one can add new
cases to the datatype and new functions over the datatype, without recompiling
existing code, and while retaining static type safety (e.g., no casts).</q>

You can notice the similarity. Object-oriented programming uses classes that
should be open for extension and closed for modification. In functional
programming, when this principle is applied, new cases to datatype should be
possible and new functions.

OCP exists because modifying battle tested code is dangerous and might cause
regressions. Thus a preferable solution is to extend the previous code instead.

### Creating a OCP compliant paint programming

In a paint program, various shapes should be possible to paint: circles,
squares, stars and custom shapes. It should also have a custom menu depending on
the shape, a circle should be able to set the radius, a square the area and
stars the diameter.

A classic functional approach is to create a sum type (variant) of the various shape

```
type Shape =
      Star size
    | Custom [vector]
    | Circle radius curvature
    | Square size
```

Afterwards, one might create a function

```
render : Shape -> IO ()
render shape =
    case shape of
    | Star(size) => Star.render(size)
    ...

-- Do the same thing
renderMenu : Shape -> IO ()
renderMenu = ...
```

If we wish to add one more shape, we would have to modify the original code!
This means that Open/Closed principle is not being followed. It can cause a lot
of trouble down the line, one function is acceptable but what if we had
thousands of functions that depended on shape. Adding one shape would mean
changing thousands of lines of code scattered all over the place!

### Type classes and Contravariance

In order to render shapes, there needs some general format which we can use to
render them. Let's assume we have some function `render : Set Vector -> IO ()`
for rendering. This is great because we know that any shape can be represented
as a set of vectors in the end. Let us define `newtype Renderable a = Renderable (a -> Set Vector)`. Now it becomes possible to define a render
function `render : Renderable a -> a -> IO ()`, that works for all shapes.

Renderable can not be instanced as Functor and just having a function `a -> Set Vector` is not extendable. With Functors, informally we append an extra arrow at
the _end_ of the function. So if we have a `Maybe a` and want to turn it into
`Maybe b`, we append an arrow at the end so that `a -> b`. With fmap we can keep
transforming so `a -> b -> c ->....` but we can not add anything before `a`.

Now the opposite is true for a Contravariant functor. A Contravariant Functor
can only prepend arrows. So if we have `a`, it can be turned into `b -> a`, `c -> b -> a`. Nothing can be added after a however. This operation is called
contramap, and has the signature:

```
contramap : Contravariant c => (a -> b) -> c b -> c a
```

Turns out that while Renderable is not a Functor, it is a contravariant Functor:

```
instance Contravariant a => Renderable a where
    contramap cf b = \a -> b $ cf a

```

Shapes can be made in separation now by contramapping properties. So now we can
implement shapes in separation, for example a circle:

```
type Circle = {radius: Int}
circle : Renderable Circle
circle = circleToVector . radius -- circleToVector does mathy stuff

setRadiusFactor :  Int -> Renderable Circle -> Renderable Circle
setRadiusFactor factor = contramap ({radius = factor})
```

The circle is rather trivial and does not seem to require much modification.
What about Custom shapes? Well they too become trivial with contravariance:

```
type Custom = {scale : Int, shape : Set Vector}
custom : Renderable Custom
custom = scale × shape

addVertex : Vector -> Renderable Custom -> Renderable Custom
addVertex vertex = contramap (Set.union vertex)
```

Contravariance forces adherence to a certain interface but
leaves it open to modification, in spirit to the Open/Closed Principle. A
separate part of the code can contain `Renderable Square`, `Renderable Star`
without modifying the original code. Thus, in Functional programming,
contravariance and type classes can enable OCP compliant code.

Note here that OCP compliant code might not be that big of a problem. The first
example with using a sum type might actually be a simpler solution that is more
accessible for beginners. With modern static type checkers, if you make a change
in one place it will report errors everywhere so the developer just has to
follow the type and fix all the errors when making changes.
