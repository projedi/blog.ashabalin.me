{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative

import Control.Exception(SomeException, catch)
import Data.Monoid((<>))
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import System.FilePath((</>), dropExtension, splitFileName)

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
      -- TODO: Should use compressCssCompiler but there is a bug:
      -- https://github.com/jaspervdj/hakyll/issues/323
      compile copyFileCompiler

   match "javascript/*" $ do
      route idRoute
      compile copyFileCompiler

   handlePosts "Posts" "posts" "index.html"
   handlePosts "Drafts" "drafts" "drafts/index.html"

   match "templates/*" $ compile templateCompiler

tryWithDefault :: IO a -> a -> IO a
tryWithDefault m d = catch m (\(_ :: SomeException) -> return d)

handlePosts :: String -> FilePath -> Identifier -> Rules ()
handlePosts title postDir indexIdent = do
   match (fromGlob $ postDir </> "**/*") $ do
      route idRoute
      compile $ do
         path <- drop 2 <$> getResourceFilePath -- drop leading "./"
         contents <- unsafeCompiler $ (Text.unpack <$> Text.readFile path) `tryWithDefault` ""
         return $ Item (fromFilePath path) contents
   match (fromGlob $ postDir </> "*.markdown") $ do
      route $ customRoute $ (</> "index.html") . dropExtension . toFilePath
      compile $ do
         extrafiles <- getExtraPostFiles
         pandocCompilerWithTransform
            defaultHakyllReaderOptions
            (defaultHakyllWriterOptions { writerHighlight = False })
            (pandocIncludeFilter extrafiles)
            >>= loadAndApplyTemplate "templates/post.html" postContext
            >>= loadAndApplyTemplate "templates/default.html" postContext
            >>= relativizeUrls
   create [indexIdent] $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll (fromGlob $ postDir ++ "/*.markdown")
         makeItem "" >>= loadAndApplyTemplate "templates/posts.html" (postListContext title posts)
                     >>= loadAndApplyTemplate "templates/default.html" (postListContext title posts)
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

postListContext :: String -> [Item String] -> Context String
postListContext title posts
   =  listField "posts" postContext (return posts)
   <> constField "title" title
   <> defaultContext

postContext :: Context String
postContext =  dateField "date" "%B %e, %Y"
            <> field "url" stripIndex
            <> defaultContext

stripIndex :: Item a -> Compiler String
stripIndex = fmap (maybe empty (toUrl . go)) . getRoute . itemIdentifier
 where go :: FilePath -> String
       go name =
          case splitFileName name of
             (dir, "index.html") -> dir
             _ -> name
