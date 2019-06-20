---
date: 2019-06-15
title: All the variants of Variants
author: Marc Coquand
---

ReasonML and Ocaml feature a language construct called variants. Most languages
feature records which are a way of saying this **and** that, while variants
allows expressing this **or** that. From other languages you might know a
simplified version called enums, variants are an extension of those. The
strength of variants is it's ability to reduce bugs.

ReasonML features three variants that this article will cover: variants,
polymorphic variants and generalized variants (a.k.a. GADTs). This article
assumes some familiarity with Redux and ReasonML.

## Your first variant

To create a variant in ReasonML the syntax is simple:

```
type catOrDog = Cat | Dog;
```

This defines a variant CatOrDog with two **constructors**, meaning we can construct
the variant by expressing `let cat = Cat`. Using variants in code is simple:

```
let feed = catOrDog =>
    switch(catOrDog) {
    | Cat => feedCatFood
    | Dog => feedDogFood
    }
```

Switch statements are like the case or if/else constructs in other languages. A
variant can also take constructor arguments:

```
type age = int
type name = string
type nameTag = string
type account =
    | Facebook(name,age)
    | Github(name, nameTag)
```

Constructor arguments are like function arguments. These can also be matched on
in a switch statement:

```
let greeting =
  switch (myAccount) {
  | Facebook(name, age) => "Hi " ++ name ++ ", you're " ++ string_of_int(age) ++ "-year-old."
  | Github(name,nameTag) => "Hello " ++ name ++ " a.k.a. " ++ nameTag ++ "."
  };
```

Variants can also be parameterized over variables, so for example an error
handling type:

```
type compute('a) =
    | Successful('a)
    | Failed

// Somewhere else
let getUser = id =>
    Js.Promise.(Fetch.fetch('/api/user/' ++ id)
        |> then_(user => Successful(user)
        |> catch(_err => failed)
```

### Use case: Redux reducers

A reducer is a function that takes some state and an action. Actions are a
description of a change request. Thus they are perfect candidate for variants.
For example this todo application:

```
type actions =
    | AddTodo
    | ToggleTodo
    | RemoveTodo


// Later on
let reducer = (model, action) =>
    switch(action) {
    | AddTodo => ...
    ...
    }
```

Now the compiler will force us to handle every possible action that can be
performed by the view, great!

## Polymorphic Variants, variants sliced up

Variants must always tie to some type, it's impossible to create a `let cat = Cat` unless a `type catOrSomething = Cat...` is declared beforehand.
Polymorphic variants do not have this constraint, they can exist independently
from types. The syntax for polymorphic variants compared to normal variants are
as follows:

```
// Normal variant
type rgb = Red | Green | Blue
// Polymorphic variant
type rgb = [`Red | `Green | `Blue]
```

However these polymorphic variants do not need to be declared beforehand.

```
let number = (i: int) => `Int(i) /* returns [> `Int(int)] /*
```

Note the `>` symbol in the code above, this is explored later. The polymorphic
variants can also be extended:

```
type rgb = [`Red | `Green | `Blue]
type colors = [rgb | `Purple | `Green]
```

Polymorphic variants use a global namespace.

### Upper and lower bounds for polymorphic variants

Notice the `>` symbol in the code above, it indicates the _lower bound_ of the
type. In other words, what the type can contain at least to work. On the other
end, there is `<` to indicate what the type must contain at most to work. If the
reducer declared earlier was instead a polymorphic variant:

```
// Redeclared action as polymorphic variant
// type action = [`AddTodo | `ToggleTodo | `RemoveTodo]
// is not neccesary
let reducer = (state, action) =>
    switch(action) {
    | `AddTodo => ...
    | `ToggleTodo => ...
    | `RemoveTodo => ...
    }
```

ReasonML would infer the type of action to be `` [< `AddTodo | `ToggleTodo | `RemoveTodo] ``.
This makes sense, reducer can not handle an action `` `UpdateTodo ``.

### Use case: Localization

At first, variants seem like a fantastic way to handle localization. The naive
solution is to create two variants, one for language and one for the phrase. So
this could look something like:

```
// Language.re
type language = En | Fr
type phrase = HelloComponentGreeting | OtherComponentWithGoodbye
let translate = (language, phrase) =>
    switch((language,phrase)) {
    | (En, HelloComponentGreeting) => "Hello"
    | (En, OtherComponentWithGoodbye) => "Goodbye"
    | (Fr, HelloComponentGreeting) => "Bonjour"
    | (Fr, OtherComponentWithGoodbye) => "Au revoir"
    }
// Hello.re
let component = (lang) =>
    <h1>Language
        .translate
        (lang, Language.HelloComponentGreeting)
    </h1>
```

In an application, this would mean that every single phrase would have to be
inserted into a massive switch case. Also every single component in the system
would be dependent on a file Language.re. By using polymorphic variants,
components and their translations can be developed in an individual file:

```
//Language.re
type lang = En | Fr
let rec translate = (lang, phrase) =>
    switch((lang, phrase)) {
    | (En, `Greeting) => "Hello"
    | (Fr, `Greeting) => "Bonjour"
    | (En, `Goodbye) => "Bye"
    | (En, `Offline) => "You are offline"
    | (Fr, `Goodbye) => "Au revoir"
    // Default to english if phrase not found
    | (Fr, unsupported) => translate(En, unsupported)
    }

//Hello.re
let component = (~translate: [< `Greeting] => string) =>
    <h1>translate(`Greeting)</h1>


// App.re
let component = (state) =>
    Hello
        .component(~translate=
        Language.translate(state.languge))
```

This still has all the same guarantees as before. `translate` is now instead
taken as a function argument and needs to be able to handle `` [< `Greeting] ``
and return a string. This enables developing components in isolation without
having to import the translate function. If the component contains a phrase not
in the translate function it causes compile errors. This means that component
writers can work in isolation and then wire up the cases afterwards.

Another great example for polymorphic variants is [composable error
handling](https://keleshev.com/composable-error-handling-in-ocaml).

## Generalized variants

Last in the variants of variants is the generalized variant, also known as
generalized algebraic data type, GADT, first-class phantom type, guarded
recursive datatype, or equality-qualified type. Lots of variants on that name.

A generalized variant is a variant which can specify a hidden return type.

```
type boolOrInt('a) =
    | Bool: boolOrInt(bool)
    | Int: boolOrInt(int)
```

`boolOrInt('a)` is a variant with two constructors but with a twist. Notice that
the variant has a return type, `boolOrInt('a)`. The return type can be used to
create a function that depending on the variant produces a different type.

```
let evaluate = (boolOrInt: boolOrInt('a)): 'a =>
    switch(boolOrInt) {
    | Bool => true
    | Int => 5
    }
```

If you try to compile this you will run into errors. The variable needs to be
quantified, which is a topic for another day. For now just change the function.

```
let evaluate: type a. boolOrInt(a) => a =
    switch(boolOrInt) {
    | Bool => true
    | Int => 5
    }
```

Notice that the return type is the same as the type in the `boolOrInt`
constructor's return type. So if you attempt to declare

```
let evaluateWithError: type a. boolOrInt(a) => a =
    switch(boolOrInt) {
    | Bool => "I'm a string"
    | Int => 5
    }
```

the compiler will generate an error. The return type of the function **must** be
the same as the return type of `boolOrInt`. `evaluateWithError` makes the
mistake that it attempts to return a string when `boolOrInt` declared that for
the constructor Bool, the type is a boolean. The type signature states that
`boolOrInt(a) => a`, so for the constructor `Bool`, `boolOrInt(bool)` and for
the constructor `Int`, `boolOrInt(int)`. `evaluateWithError` has a case with
`boolOrInt(bool) => string`, which the compiler complains about.

`boolOrInt` is not particularly exciting. Let's do something more interesting.

### Use case: Inversion of control

When creating large scale software it can be dangerous to rely on one
dependency. For example if the software developed relies on Firebase and the
requirements change so now it has to rely on Postgres then all code needs to be
rewritten. Inversion of control mitigates this by hiding dependency details.
This is normally done with interfaces. So for example

```
type operations =
    { getAll: list(todo),
      delete: string => unit,
      update: string => todo => unit
    }

let firebaseOps: operations = ...

let mockedOps: operations

```

This also makes it easy to run the system locally by using a mocked version.
With generalized variants, this approach is even more powerful:

```
type operations('a) =
    | GetAll: operations(list(todo))
    | Delete(string): operations(unit)
    | Update(string, todo): operations(unit)
```

So we have encoded the operations possible by the database, let's create an
evaluator for this:

```
let evaluate = type a. operations(a) => a = ops =>
    switch(ops) {
    | GetAll => Firebase.fetchTodos
    | Delete(id) => Firebase.deleteTodo(id)
    | Update(id, todo) => Firbase.update(id,todo)
```

So far this has yielded much. However after updating or deleting a todo, the
application should also fetch the new list. So let's encode that

```
type operations('a) =
    | GetAll: operations(list(todo))
    | Delete(string): operations(unit)
    | Update(string, todo): operations(unit)
    | Sequence(operations(unit),operations('b): operations('b)

let rec evaluate = type a. operations(a) => a = ops =>
    switch(ops) {
    | GetAll => Firebase.fetchTodos
    | Delete(id) => Firebase.deleteTodo(id)
    | Update(id, todo) => Firbase.update(id,todo)
    | Sequence(op1, op2) =>
        { evaluate(op1);
          // op1 returns a unit so we don't care about that
          evaluate(op2);
         }
```

Notice that evaluate is now recursive. If two operations are sequenced we want
to run evaluate on each one of them. Now we can describe our effects with
functions so that after each effect the todolist gets updated as well.

```
// Effect.re
let getAll = GetAll
let delete = id => Sequence(Delete(id), getAll)
let update = (string,todo) = Sequence(Update(string,todo), getAll)
let evaluate = type a. operations(a) => a = ops =>
    switch(ops) {
    | GetAll => Firebase.fetchTodos
    | Delete(id) => Firebase.deleteTodo(id)
    | Update(id, todo) => Firbase.update(id,todo)
    | Sequence(op1,op2) {
        evaluate(op1)
        evaluate(op2)
        }
```

This approach has advantages over the standard dependency injection approach for
testing and maintenance, which will be explored in other articles.
