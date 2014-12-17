{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid((<>))

import Hakyll

main :: IO ()
main = hakyll $ do
   match "images/*" $ do
      route idRoute
      compile copyFileCompiler

   match "css/*" $ do
      route idRoute
      compile compressCssCompiler

   match "posts/*" $ do
      route $ setExtension "html"
      compile $   pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html" postContext
              >>= loadAndApplyTemplate "templates/default.html" postContext
              >>= relativizeUrls

   create ["index.html"] $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         makeItem "" >>= loadAndApplyTemplate "templates/index.html" (indexContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (indexContext posts)
                     >>= relativizeUrls

   match "templates/*" $ compile templateCompiler

indexContext :: [Item String] -> Context String
indexContext posts =  listField "posts" postContext (return posts)
                   <> constField "title" "Stuff"
                   <> defaultContext

postContext :: Context String
postContext =  dateField "date" "%B %e, %Y"
            <> defaultContext
