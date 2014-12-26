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

   match "drafts/*" $ do
      route $ setExtension "html"
      compile $   pandocCompiler
              >>= loadAndApplyTemplate "templates/draft.html" draftContext
              >>= loadAndApplyTemplate "templates/default.html" draftContext
              >>= relativizeUrls

   create ["index.html"] $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll "posts/*"
         makeItem "" >>= loadAndApplyTemplate "templates/index.html" (indexContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (indexContext posts)
                     >>= relativizeUrls

   create ["drafts.html"] $ do
      route idRoute
      compile $ do
         posts <- loadAll "drafts/*"
         makeItem "" >>= loadAndApplyTemplate "templates/drafts.html" (draftsContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (draftsContext posts)
                     >>= relativizeUrls

   match "templates/*" $ compile templateCompiler

draftsContext :: [Item String] -> Context String
draftsContext posts =  listField "posts" draftContext (return posts)
                    <> constField "title" "Drafts"
                    <> defaultContext

indexContext :: [Item String] -> Context String
indexContext posts =  listField "posts" postContext (return posts)
                   <> constField "title" "Stuff"
                   <> defaultContext

postContext :: Context String
postContext =  dateField "date" "%B %e, %Y"
            <> defaultContext

draftContext :: Context String
draftContext = defaultContext
