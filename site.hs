{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative

import Control.Exception(SomeException, catch)
import Data.Monoid((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath((</>), dropExtension)

import Text.Pandoc(Block(..), Pandoc, WriterOptions(..))
import Text.Pandoc.Walk(walk)

import Hakyll

main :: IO ()
main = hakyll $ do
   match "robots.txt" $ do
      route idRoute
      compile copyFileCompiler

   match "images/*" $ do
      route idRoute
      compile copyFileCompiler

   match "css/*" $ do
      route idRoute
      compile compressCssCompiler

   match "javascript/*" $ do
      route idRoute
      compile copyFileCompiler

   match "posts/**/*" extraPostFilesRules
   match "posts/*" $ postRules "templates/post.html" postContext
   match "drafts/**/*" extraPostFilesRules
   match "drafts/*.markdown" $ postRules "templates/draft.html" draftContext

   create ["index.html"] $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll "posts/*.markdown"
         makeItem "" >>= loadAndApplyTemplate "templates/index.html" (indexContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (indexContext posts)
                     >>= relativizeUrls

   create ["drafts.html"] $ do
      route idRoute
      compile $ do
         posts <- loadAll "drafts/*.markdown"
         makeItem "" >>= loadAndApplyTemplate "templates/drafts.html" (draftsContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (draftsContext posts)
                     >>= relativizeUrls

   match "templates/*" $ compile templateCompiler

tryWithDefault :: IO a -> a -> IO a
tryWithDefault m d = catch m (\(_ :: SomeException) -> return d)

extraPostFilesRules :: Rules ()
extraPostFilesRules = do
   route idRoute
   compile $ do
      path <- drop 2 <$> getResourceFilePath -- drop leading "./"
      contents <- unsafeCompiler $ (Text.unpack <$> Text.readFile path) `tryWithDefault` ""
      return $ Item (fromFilePath path) contents

postRules :: Identifier -> Context String -> Rules ()
postRules templateIdent context = do
   route $ setExtension "html"
   compile $ do
      extrafiles <- getExtraPostFiles
      pandocCompilerWithTransform
         defaultHakyllReaderOptions
         (defaultHakyllWriterOptions { writerHighlight = False })
         (pandocIncludeFilter extrafiles)
         >>= loadAndApplyTemplate templateIdent context
         >>= loadAndApplyTemplate "templates/default.html" context
         >>= relativizeUrls

getExtraPostFiles :: Compiler [(String, String)]
getExtraPostFiles = do
   path <- (drop 2 . dropExtension) <$> getResourceFilePath -- drops leading "./" and an extension
   map (fromItem path) <$> loadAll (fromGlob $ path </> "*")
 where fromItem :: String -> Item String -> (String, String)
       -- drop leading directories up to a post name and also drop following "/"
       fromItem path (Item ident body) = (drop (length path + 1) $ toFilePath ident, body)

-- From http://johnmacfarlane.net/pandoc/scripting.html#include-files
pandocIncludeFilter :: [(String, String)] -> Pandoc -> Pandoc
pandocIncludeFilter extrafiles = walk go
 where go :: Block -> Block
       go cb@(CodeBlock props@(_, _, namevals) _) =
          maybe cb (CodeBlock props) $
             flip lookup extrafiles =<< lookup "include" namevals
       go b = b

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
