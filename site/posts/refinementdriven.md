---
date: 2019-05-05
title: Refinement-driven application development
author: Marc Coquand
---

Over the last months I have been working on a method for building applications.
There is probably someone who has done this somewhere else but as far I know no
one has done this so far. I coin the term refinement-driven development to allow
us to be buzzword-friendly. Based around constructing software via
stepwise-refinements, the method using functional programming constructs,
dependency injection and property-based testing. A refinement-driven approach
sets out to achieve the following:

1. All forms of testing should be straightforward, including unit, integration and
   E2E-testing. Testing should be human-first approach, meaning in practice that
   we can go from user stories to writing tests for those user stories.
2. Architecture should allow us to easily add new environment. This approach
   allows us to create a web application and then port it to mobile. In fact
   using this approach to build a web application the last thing we would
   implement is the html itself.
3. Strike a balance between maintainability, being bug-free and budget
   constrains. This method lies somewhere between current industry standards and
   formal methods/formal verification in it's rigor. The method gives us
   flexibility to be fast in certain areas but adds room to create further
   assurance in places where critical.
4. The method should be agile and responsive to new requirements.
5. It should allow for easier testing of concurrent systems and microservices.
6. It should be as simple as possible.
7. Following it should naturally lead to SOLID-principles.
8. It should be optimization friendly, as well as ensuring that more verification
   can be done later it also ensures that performance optimizations can be done.
9. It is somewhat language agnostic. Requirement from the language is supporting
   functional programming constructs and property-based testing. You need to
   have sum types or variants or discriminated unions, records and higher-order
   functions. Languages I know that support this are Typescript, ReasonML,
   Haskell, Purescript and F#. Having the same language for both front-end
   and back-end makes this method even more powerful.

The approach is close to what we find in formal methods, specifically
refinements hence the name. The core idea is combining a formal method approach
to software development with property-based testing to ensure our refinements
are consistent with our simpler model without having to write difficult, error
prone, proofs.

Property-based testing is a new form of testing where we generate data for our
functions and ensure that properties hold. The simplest example is that we can
ensure that `reverse(reverse(list)) === list`. The property-based check will
generate lists to try to find the simplest counter-example.

Property-based testing is not perfect however. Finding invariants to test is not
obvious for the uninitiated. Property-based testing gets even harder if you have
not built your code around it from the start, like unit testing.

A core strength of property-based testing is that we can use it to test
environments in isolation. That is we can test that two microservices work in
isolation for bugs that would before only show up when doing integration tests.
Doing so allows us to use a mocked environment for testing the microservices
together.

This all thanks to the idea of refinements. A refinement is defined as follows.
For any specification _P_ (our problem), _P_ is _refined_ by a specification _S_, if and
only if, _P_ is satisfied whenever _S_ is satisfied. Formally this means that
`forall m. P <== S`. What does this mean in English? Well it means that, for
example, if we have a mocked database and a real database, if every property for
the mocked and real one is the same for our model we can swap one out for the
other and be sure that the rest of our program behaves the same. This might
sound obvious but it has powerful implications for the rest of our software. It
means that:

1. We can test walking through the application with simple function composition
   and be sure that if that works it will also work for our mobile application
   and web application.
2. We can test for concurrency and parallelism bugs dependent from the
   environment. We test individual parts of the system for these bugs by using
   our model and property-based testing. Afterwards we can test the integration
   of these parts by using the mocked environment.
3. We can develop our entire application independent of any actual
   implementation. We can create it without caring about if we should use React
   or Vue. Or if we should use Postgresql or Mongodb. When the software is done
   we plug in whatever environment we want. Should we pick the wrong
   one we can swap it out later, thanks to inversion of control.

### Elm architecture as the foundation of our application

The law of refinement is actually quite simple but powerful in practice. It
states

Property-based testing means to test that invariants hold. What is an invariant?
It is a condition that is true regardless of what data we feed a function. A
variant is the opposite, something that varies which is usually data.
