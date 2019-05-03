---
date: 2019-05-03
title: My Criticism of Elm
author: Marc Coquand
---

People who know me in real life know that I always rave about Elm and how
incredible the language is. But, alas in software engineering there is no such
thing as a silver bullet. Elm does have it's drawbacks and I am going to list
some of them here which can pose a problem.

## Testing Elm is actually not that easy

There are parts of the software that Elm's type system can not guarantee are
error free and where regressions can sneak in. For example if our program
interacts with a REST api and the endpoints change, the program would stop
working. In Elm, there is no way to write any test to check for this. The reason
is because Elm is a pure language that does not execute any side effects. Elm
has no way of writing integration tests. To write integration tests in Elm there
must be a way to execute commands and ensure that right they return the correct
response. This is also integral to writing property-based tests for state
machines, where we model the side effects with a pure implementation and compare
the effectful model to the pure model and ensure they function the same way for
any data.

Allowing us to execute the side effects would also allow for rapid End2End tests
as then we could feed update an initial model and a set of commands and ensure
that at the end we get the correct resulting model. This would make quality
assurance a lot easier and allow us to verify that the program follows the
specification.

Almost all projects I have looked at in Elm have little to no tests. In [Richard
Feldman's SPA example](https://github.com/rtfeldman/elm-spa-example) we find
that he only tests one module in the entire software. The mantra seems to be
that it is overkill to test Elm systems. Perhaps there is a case to be made that
integration tests in Elm are not worth their costs. In my experience any
bug you find while manually testing is trivial to fix and would not have shown
up in any unit test. I suspect that other people consider the same thing so they
do not bother. I find this is a big red flag.

## Sometimes you actually want to encapsulate state

If you look at a lot of published elm sites you will find that a lot of them use
almost no animations. While I can not access the main service of
[www.noredink.com](www.noredink.com), all the interactive parts had no
transitions or animations. It turns out that animating in Elm is a tedious
endeavour. For each transition in the animation it must be sent to the global
update function and our global model must also keep track of the transition
itself. This adds a lot of noise and boilerplate to our code. In a big projects,
do we want the entire system to concern itself with animations of a button?
There can be a lot of animations and transitions, making our update function and
Msg type massive and span across files. This is a place where techniques from
OOP can come in handy. OOP is good at a modeling stateful system that transition
gracefully between internal positions. Good examples of this is a button
animation going from untouched to hovered. It will not affect the main state at
all. Another example is the evolution of a database cursor, where it moves from
not being in the middle of a transaction to moving to one that is. That is,
sometimes model a resource as being managed rather than a value being
transformed.

## No communication on what will change

When a new version of Elm comes out, all packages need to explicitly be updated
to the latest version, even if they do not need modifications. In the transition
from Elm 0.18 to Elm 0.19 they removed user defined infix operators. While I
applaud this change, these deprecations were not communicated unless you
explicitly asked in the Slack channel. Speaking of Slack channel

## All communication goes via the Slack channel

The official Slack channel is where most Elm people hang out. This means that
common questions can not be found on Stackoverflow but
must instead be asked in the channel. This is horribly inefficient and results
in people asking the same questions over and over again. It also makes it seem
like the community is dead with little activity happening in the other channels.
