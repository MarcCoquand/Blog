---
date: 2019-07-14
title: SOLID, Single Responsibility Principle
author: Marc Coquand
---

Robert C. Martin introduced these five design principles called SOLID to create
maintainable software. These principles were introduced for Object Oriented
programming but they turn out to be useful for functional programming as well.
In fact, in functional programming applying these principles can turn out to be
even more powerful. The five principles are:

- S — Single Responsibility Principle
- O - Open/closed Principle
- L — Liskov substitution Principle
- I - Interface Segregation Principle
- D - Dependency Inversion Principle

This will be a five part series where I translate each of the concepts to
functional programming, starting with S for Single Responsibility Principle.

Single Responsibility Principle (SRP) states that every module or class should
have responsibility over a single part of the functionality provided by the
software. This roughly translates to that each class should do one thing. _"Do
one thing well"_ and Unix principle are other names for this.

### File structure should be flat and formed around types

In Functional programming, software is a composition of functions, that take
some input and spit out some output. So the principle states that we should keep
our functions small and reusable.

Single Responsibility Principle also addresses file structure concerns. In
Object-oriented programming, each file has one class. Notice that it says every
_module_ should have responsibility over a single part of functionality. In
functional programming you have two constructs, data and functions on that data.
So it follows that every module should have responsibility of a data type and
the functions on that data.

Ever wondered how you should structure your files and folders? It turns out that
the answer is to not have structure at all and keep file structure flat.
Dependencies form a graph but a file system forms a tree! This means that
whatever structure you try to adopt, unless your dependencies are actually a
tree, will never work.

Instead, what you should do is simple. Have a type and the functions that take
that type as an argument. So a file `User` should contain one type t, probably a
record, and then the operations on that user. This can be encoding/decoding,
calculating location and posting that user to a database.

In summary, Single Responsibility Principle states that every part should do one
thing. In functional programming this means having a module structure around
types and to break down functions into small parts. This leads to less messy
code and to functions that are naturally easy to test.

### Lazy evaluation helps writing code that is SSP
