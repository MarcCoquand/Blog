{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.List              (sort)
import           GHC.Generics
import           SitePipe

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

data Post = MakePost
    { url      :: String
    , filepath :: String
    , content  :: String
    , date     :: String
    , author   :: String
    , title    :: String
    }
    deriving (Generic, Eq)
instance ToJSON Post
instance FromJSON Post
instance Ord Post where
    compare a b =
        if (date a) > (date b) then
            LT
        else
            GT

-- | Just let it crash if it's not successful.
fromSuccess :: Result a -> a
fromSuccess (Success a) = a

sortPosts :: [Value] -> [Value]
sortPosts =
    map toJSON . sort . map (fromSuccess . fromJSON @Post)

main :: IO ()
main =
    site $
        do  unsortedPosts <- resourceLoader markdownReader ["posts/*.md"]

            let posts =
                    sortPosts unsortedPosts

            staticAssets
            writeTemplate "templates/index.html" [indexContext posts]
            writeTemplate "templates/post.html" posts
