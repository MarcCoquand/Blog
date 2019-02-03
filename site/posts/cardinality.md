---
title: Bug Eraser Data Structures in Typescript
author: Marc Coquand
date: 2019-02-03
---

The choices of data structures can have a big impact on our software. For
example, should we represent our list of users as a hash map, array or stack?
There are multiple aspects to take into account when choosing, such as
performance. Choosing the wrong one makes it easier to introduce _bugs_ into our
software. This is what I will be demonstrating today, along with a tool how you
can check manually if you have the right data structure.

Say we want to create a program. The specification says this program will use
three colors: blue, red and green. As we are good developers, we start of
writing the types. We create an interface to represent our data type as follows:

```
interface NaiveColor {
    isBlue: boolean;
    isRed: boolean;
    isGreen: boolean;
}
```

Now, we can represent blue color as follows.

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

Our program specified three different colors; red, green and blue.
`imImpossible` seems to be both blue and red at the same time. Should we
represent it as purple? No, that was not in the specification. Our choice of
representation for color has made it possible for _invalid data_ to exist. Our
developers would have to be extra disciplined to ensure that `NaiveColor` does
not introduce unintended bugs in the system.

Now, how else might we have done it? By using Enums. Check out the following
representation:

```
type Color = Blue | Green | Red
```

Now, representing a color that is both red and blue at the same time is
impossible. By choosing a different data structure, we have made it impossible
to create _invalid data_. The reason, why invalid data is impossible is because
the enum's _cardinality_ matches the amount of valid values of the colors we are
trying to represent. What is cardinality? Cardinality of a type is a measure of
the amount of values of a type are possible. Color has three possible values:
Blue, Green and Red. We say that `cardinality(Color)=3`.

Now what about `NaiveColor`, what cardinality does it have? Let us first look at
the following type

```
type NaiveColorPrim = [boolean, boolean, boolean]
```

A boolean can have two possible values: true and false, thus
`cardinality(boolean) == 2`. To calculate `NaiveColorPrim`'s cardinality,
we multiply the cardinality of each of it's members together. I.E.

```
cardinality(NaiveColorPrim) =
cardinality(boolean) ×
cardinality(boolean) ×
cardinality(boolean) =
2×2×2=8
```

Same goes for the interface `NaiveColor`. These types, or interfaces, are
usually called _product type_, since it's cardinality is the product of all the
cardinalities of each of it's members. In Typescript, there is a separation
between types and interfaces. In this article we consider them all to be
types, since an interface can be represented as a tuple of all it's members.

Since we knew that the amount of valid representations of colors were 3 and that
cardinality of `NaiveColor` is 8, we can deduce that `NaiveColor` allows for
_invalid data_. Invalid data can be a source of errors and bugs, thus we want to
eliminate them by using types to protect us.

Another concept we introduce is sum types. For example, let's look at the
type `MaybeBool`.

```
type MaybeBool = boolean | Void;
```

`MaybeBool` can either be true, false or Void. Thus

```
cardinality(MaybeBool) =
cardinality(boolean) + cardinality(Void) =
2+1=3
```

Some other types that often come up are strings and numbers, whose cardinalities
we both represent as ∞ (although number is closer to 2^54, still we
distinguish that this is a big number). It is important to remember that
cardinality(1+∞)≠ ∞. Different sizes of infinity matters!

## A more difficult problem

We want to create a simple application that loads some data and displays it. The
application starts of by loading in information from a source and then displays
it to the user. If it fails it displays an error. How should we structure this
applications state?

We do not have a lot of information but there are already a few things we know
will be needed in the data model.

- We have a state where data is being loaded.
- If downloaded successfully, we load the main state with the data.
- If it fails, we need to display an error.

So already we can start to reason about the cardinality of our applications
state. It has three different states: loading, failure and loaded. We do not
know about the cardinality of the data but we can try to represent errors as a
string. We also note that the amount of possible errors and the amount of
possible datas are not dependant on eachother, or the fact that it is loading.
Thus we can guess that the cardinality should have some form similar to 1+∞+∞.
This means that we can dismiss the following solution:

```
interface NaiveState {
    error: string | undefined ;
    isLoading: boolean;
    data: Data | undefined;
}
```

Why? The cardinality of NaiveState is

```
cardinality(errors) = ∞ + 1
cardinality(isLoading) = 2
cardinality(data) = ∞ + 1

cardinality(NaiveState) =
cardinality(errors) × cardinality(isLoading) × cardinality(data) =
(∞+1)×2×(∞+1)
```

Which is not what we are looking for and we can also find the
following value

```
const state : NaiveState = {
    error = "data did not load",
    isLoading: false,
    data = {...}
}
```

It has given us an error, saying data did not load but still has data to show!
This means that we have an invalid state! This should be impossible!

Let us look at another solution. What if we start off with the following
union type:

```
type State = Loading | Loaded | Error
```

Now we three possible states and the cardinality will be of the form
`cardinality(State) = cardinality(Loading) + cardinality(Loaded) + cardinality(Error)`. Let us implement the interfaces as well as discriminated
union types.

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

The field "kind" has a cardinality of 1 since it's just a given value. For
Typescript to enforce this we must enable strict null checks. So we can
calculate the cardinality of State and find it's total cardinality is

```
cardinality(Loading) = 1
cardinality(Loaded) = 1×cardinality(Data) = ∞
cardinality(Error) = 1×cardinality(Error) = ∞
cardinality(State) = 1+∞+∞
```

Much better, by taking a closer look at the problem we were solving and using
cardinality we could conclude that we probably had the wrong implementation for
cardinality and found an alternative solution that is much more elegant and
makes impossible states impossible.
