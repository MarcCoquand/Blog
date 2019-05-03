---
date: 2019-02-03
title: Bug Eraser Data Structures in Typescript
author: Marc Coquand
---

The choices of data structures can have a big impact on our software. For
example, should we represent our list of users as a hash map, array or stack?
There are multiple aspects to take into account when choosing, such as
performance. Choosing the wrong one makes it easier to introduce bugs into our
software. This is what I will be demonstrating today, along with a tool how you
can check manually if you have the right data structure.

Say we want to create a program. The specification says this program will use
three colors: blue, red and green. As we are good developers, we start of
writing the types. We create an interface to represent our data type.

```
interface NaiveColor {
    isBlue: boolean;
    isRed: boolean;
    isGreen: boolean;
}
```

Now, we can represent a color as follows.

```
const blue: NaiveColor = {
    isBlue: true,
    isRed: false,
    isGreen: false
}
```

All good and well but what about this value?

```
const imImpossible: NaiveColor = {
    isBlue: true,
    isRed: true,
    isGreen: false
}
```

Our specification specified three different colors: red, green and blue.
`imImpossible` seems to be both blue and red at the same time. Our choice of
representation for color has made it possible for _invalid data_ to exist. This
is a potential source of bugs. How else might we have represented colors? By
using Union types.

```
type GoodColor = Blue | Green | Red
```

With this type, representing a color that is both red and blue at the same time
is impossible. By choosing a different data structure, we have made it
impossible to create _invalid data_. Why invalid data is impossible is because
GoodColor's _cardinality_ matches the amount of valid values of the colors we
are trying to represent. What is cardinality? Cardinality of a type is a measure
of the amount of values of a type are possible. Color has three possible values:
Blue, Green and Red. We therefore say that `cardinality(Color)=3`.

Now what about `NaiveColor`, what cardinality does it have? Let us first look at
the following type

```
type NaiveColorPrim = [boolean, boolean, boolean]
```

A boolean can have two possible values: true and false, thus
`cardinality(boolean) == 2`. To calculate `NaiveColorPrim`'s cardinality,
we multiply the cardinality of each of it's members. I.E.

```
cardinality(NaiveColorPrim) =
cardinality(boolean) ×
cardinality(boolean) ×
cardinality(boolean) =
2×2×2=8
```

Same goes for the interface `NaiveColor`. These types, or interfaces, are
usually called _product type_, since it's cardinality is the product of all the
cardinalities of each of it's members. If you are not convinced, try writing
down all possible values of `NaiveColorPrim`. In Typescript, there is a
separation between types and interfaces. In this article we consider them all to
be types, since an interface can be represented as a tuple of all it's members.

Since we know that the amount of valid representations of colors were 3 and that
cardinality of `NaiveColor` is 8, we can deduce that `NaiveColor` allows for
_invalid data_. Invalid data can be a source of errors and bugs.

There are product types, described above and there are also sum types, such as
`GoodColor`. Another example of a sum type is the type `MaybeBool`.

```
type MaybeBool = boolean | Void;
```

`MaybeBool` can either be true, false or Void. And since it is a union type with
two members, the cardinality is the cardinality of each of it's memebers.

```
cardinality(MaybeBool) =
cardinality(boolean) + cardinality(Void) =
2+1=3
```

So we call sum types as sum types because they're the sum of the cardinalities
of all their members. Product types are called product types because they are
the product of all their members, intuitive right?

Some other primitive types that often come up are strings and numbers, whose
cardinalities we both represent as ∞, to indicate that their cardinality is
large. Remember that cardinality(1+∞)≠∞. Different sizes of infinity matters!

## Cardinality in practice, a more difficult problem

Say we want to create a simple application that loads some data and displays it.
The application starts of by loading in information from a source and then
displays it to the user. If it fails it displays an error. How should we
structure this applications state?

We do not have a lot of information but there are already things we know are
needed in the data model.

- We have a state where data is being loaded.
- If downloaded successfully, we load the main state with the data.
- If it fails, we need to display an error.

So already we can start to reason about the cardinality of our applications
state. It has three different states: loading, failure and loaded. We do not
know about the cardinality of the data, so let us assume it is ∞. We can
represent errors as a string, thus the cardinality of errors should also be ∞.
We also note that the amount of possible errors and the amount of possible data
are not dependant on each other. We also have a state loading, which since it
only has one value, is 1. Thus we can guess that the cardinality should have a
form of 1+∞+∞. This means that that the following solution is not ideal.

```
interface NaiveState {
    loadingError: string | undefined ;
    isLoading: boolean;
    data: Data | undefined;
}
```

Why? The cardinality of `NaiveState` is

```
cardinality(loadingError) = ∞ + 1
cardinality(isLoading) = 2
cardinality(data) = ∞ + 1

cardinality(NaiveState) =
cardinality(loadingError) × cardinality(isLoading) × cardinality(data) =
(∞+1)×2×(∞+1)
```

It is a product type, not a sum type. Also the cardinality of each factor seems
to be higher than desired. It is easy to conjure up a state that actually should
be impossible to happen! Look at the following example.

```
const state : NaiveState = {
    loadingError = "data did not load",
    isLoading: false,
    data = {...}
}

function displayState(state : NaiveState) {
    if (state.isLoading) {
        displayLoading()
    } else if (state.loadingError) {
        console.log(state.error)
    } else if(data) {
        displayData()
    }
}
```

It has given us an error, saying data did not load. However it has also loaded
some data. This means that we have an invalid state. Somewhere in our code there
is a bug!

Let us look at another solution. What if we start off with the following
discriminated union type:

```
type State = Loading | Loaded | Error
```

Now we three possible states and the cardinality will be of the form

```
cardinality(State) =
cardinality(Loading) + cardinality(Loaded) + cardinality(Error)
```

Let us implement the interfaces for State.

```

interface Loading {
    kind: "loading";
}

interface Loaded {
    kind: "loaded";
    data: Data
}

interface Error {
    kind: "error";
    message: string
}

```

The field "kind" has a cardinality of 1 since it's simply a given value. Enable
Strict null checks in Typescript to enforce this. Now we can calculate the
cardinality of State.

```
cardinality(Loading) = 1
cardinality(Loaded) = 1×cardinality(Data) = ∞
cardinality(Error) = 1×cardinality(Error) = ∞
cardinality(State) = 1+∞+∞
```

Much better, by taking a closer look at the problem we were solving and using
cardinality we could reason that we had the wrong representation for our state.
We could then take an alternative implementation and reason that that was a
better representation of our state and use cardinality as a tool to reason as to
why.
