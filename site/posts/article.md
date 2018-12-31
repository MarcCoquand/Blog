---
title: A Haskell Rant
author: Marc Coquand
date: 2018-08-28
---

Let me first start of with stating that I love a lot of things in Haskell. The
declarative and functional style added with purity makes for an amazing and
beautiful language. Learning Haskell was also an enlightening experience with a
lot of aha moments. A lot of what I have learnt I will be able to use in other
languages. However with all of that said there is an excessive amount of
complexity, followed by poor explanations and so much to learn that it becomes
too much to be worth it all.

So I enrolled in a course at university for functional programming . There we
learnt everything from the basics up to type classes, Functor, Applicative,
Monad and ended at State transformers and parsers. All was good. The concepts
were not so hard and allowed me to write some clean and elegant code. Now that I
felt I had learnt Haskell I wanted to use all my newly acquired knowledge to
create something cool with it. I decided I was going to make a web app. Suddenly
I was going to be introduced to the world that is real life Haskell.

``` 
{-# language OverloadedStrings #-}
module Main where

import SitePipe

indexContext :: [Value] -> Value
indexContext posts = 
    object 
        [ "posts" .= posts
        , "url" .= ("/index.html" :: String)
        ]

-- | All the static assets can just be copied over from our site's source
staticAssets :: SiteM ()
staticAssets = 
    copyFiles [ "css/" ]

main :: IO ()
main = 
    site $ 
        do  posts <- resourceLoader markdownReader ["posts/*.md"]

            staticAssets
            writeTemplate "templates/index.html" [indexContext posts]
            writeTemplate "templates/post.html" posts
```

Jesus that is overwhelming. Turns out that there is something called language
extensions and there is a ton of them that you have to learn if you want to read
others code because they are used everywhere. I had never seen them up to this
point. After learning all 9 of the ones above I still had not learnt all that I
would need to learn to use this library. This is just the baseline to start to
understand this library. And many of these extensions do not have more than
maybe an article and some blog post as documentation. Some of the extensions are
there to fix issues with the language itself. Such OverloadedStrings to fix the
poor default implementation of String as a list of Chars. So I thought after
learning the nine extensions I listed above that I would be ready to start to
use the library. Turns out that it was not enough because then you have
extensions like TemplateHaskell and Quasiquoters which is kinda Haskell’s answer
to Lisp macros and allows you to do metaprogramming. This adds an entire new
layer of syntax and makes your code way harder to debug if you manage to
generate expressions that do not compile. Suddenly one of the main advantages of
Haskell which is it’s super useful compiler is gone. Template Haskell adds a lot
of voodoo magic making it hard to know what is going on.

---

Some of these extensions just add some esoteric stuff to make your code look
cooler. For example LambdaCase allows you to write:
