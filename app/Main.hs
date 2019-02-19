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
    copyFiles [ "css/", "js/", "res/" ]

main :: IO ()
main = 
    site $ 
        do  posts <- resourceLoader markdownReader ["posts/*.md"]

            staticAssets
            writeTemplate "templates/index.html" [indexContext posts]
            writeTemplate "templates/post.html" posts
