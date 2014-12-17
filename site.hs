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

   match (fromList ["about.markdown", "contact.markdown"]) $ do
      route $ setExtension "html"
      compile $   pandocCompiler
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

   match "posts/*" $ do
      route $ setExtension "html"
      compile $   pandocCompiler
              >>= loadAndApplyTemplate "templates/post.html" postContext
              >>= loadAndApplyTemplate "templates/default.html" postContext
              >>= relativizeUrls

   create ["archive.html"] $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         makeItem "" >>= loadAndApplyTemplate "templates/archive.html" (archiveContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (archiveContext posts)
                     >>= relativizeUrls

   match "index.html" $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         getResourceBody >>= applyAsTemplate (indexContext posts)
                         >>= loadAndApplyTemplate "templates/default.html" (indexContext posts)
                         >>= relativizeUrls

   match "templates/*" $ compile templateCompiler

indexContext :: [Item String] -> Context String
indexContext posts =  listField "posts" postContext (return posts)
                   <> constField "title" "Home"
                   <> defaultContext

archiveContext :: [Item String] -> Context String
archiveContext posts =  listField "posts" postContext (return posts)
                     <> constField "title" "Archives"
                     <> defaultContext

postContext :: Context String
postContext =  dateField "date" "%B %e, %Y"
            <> defaultContext
