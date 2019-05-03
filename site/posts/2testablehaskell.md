---
date: 2019-02-24
title: Haskell is Not that Easy to Test
author: Marc Coquand
---

One of the common arguments for using Haskell is that Haskell is pure and thus
easy to test. Purity in functions means that a function, given the same
arguments, returns the same value. Purity gives us a lot benefits. Unit tests
are trivial, we can be sure nothing funky is going to happen (like launching
nukes). Haskellers also argue that Haskell is entirely pure. Now I ask you, dear
reader, what is pure about the function below.

```
main :: IO String
main =
    do  line <- getLine
        putStrLn line
```

Is the function easy to write an automated unit test for? Nope. IO monads causes
problems down the line because it allows creating imperative programs and thus
we have gained nothing from any other language. Say we are creating an
authentication system using Json Web Tokens. We want to generate the token, set
an expiration date of this token based on one week from now. What we might
create is something like token function below.

```
token :: Key User -> IO (Maybe WebToken)
token user =
    do  currentTime <- Time.Posix.getPOSIXTime
        let oneWeek = 604800000
        let expirationDate = currentTime + oneWeek
        let maybeToken = encode $ Token user expirationDate
        return maybeToken
```

Now this function I wrote a while back. It looked fine at the time but notice
that, regardless of Haskell is pure or not, we have a function that is actually
a bit tricky to unit test. If I were to write it today I would write it like
this:

```
class MonadTime m where
    getTime :: m POSIXTime

instance MonadTime IO where
    getTime = Time.Posix.getPOSIXTime

instance MonadTime ((->) POSIXTime) where
    -- Allows us to call functions with the constraint MonadTime
    -- with an extra argument containing a mock value.
    getTime = id

addWeek :: POSIXTime -> POSIXTime
addWeek currentTime =
    currentTime + oneWeek
    where
        oneWeek = 604800000

token :: MonadTime m => Key User -> m (Maybe WebToken)
token user =
    do  currentTime <- getTime
        let expirationDate = addWeek currentTime
        let maybeToken = encode $ Token user expirationDate
        return maybeToken
```

With this definition, unit tests becomes trivial. Now there are other posts that
explains writing better Haskell code
[here](https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html). My
point with this post is to clarify that testing is still not free in Haskell, it
requires rigor and research and the uninitiated can write untestable code. It is
also to clarify that regardless if Haskell is pure or not we still do not get
the benefits of purity everywhere in our code. In fact we could write our
Haskell code in one massive main function and use `return` on pure functions and
we would have gained no benefits whatsoever from normal code. This means that we
still need to introduce some discipline and rigor to our coworkers that are not
familiar with Haskell. The new solution, while testable, is a lot more complex
and requires understanding more advanced Haskell concepts like MTL.
