{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative
import Control.Monad

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

   handlePosts "Posts" "posts" ["index.html", "posts/index.html"] "post-context"
   handlePosts "Drafts" "drafts" ["drafts/index.html"] "draft-context"

   match "templates/*" $ compile templateCompiler

tryWithDefault :: IO a -> a -> IO a
tryWithDefault m d = catch m (\(_ :: SomeException) -> return d)

extraTextPostFiles :: String -> [Pattern]
extraTextPostFiles postDir = map (\ext -> fromGlob $ postDir </> "**" </> ("*." ++ ext))
   [ "agda", "idr" ]

extraMiscPostFiles :: String -> [Pattern]
extraMiscPostFiles postDir = map (\ext -> fromGlob $ postDir </> "**" </> ("*." ++ ext))
   [ ]

handlePosts :: String -> FilePath -> [Identifier] -> String -> Rules ()
handlePosts title postDir indexIdents bodyClasses = do
   forM_ (extraTextPostFiles postDir) $ \pat ->
      match pat $ do
         route idRoute
         compile $ do
            path <- drop 2 <$> getResourceFilePath -- drop leading "./"
            contents <- unsafeCompiler $ (Text.unpack <$> Text.readFile path) `tryWithDefault` ""
            return $ Item (fromFilePath path) contents
   forM_ (extraMiscPostFiles postDir) $ \pat ->
      match pat $ do
         route idRoute
         compile copyFileCompiler
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
   create indexIdents $ do
      route idRoute
      compile $ do
         posts <- recentFirst =<< loadAll (fromGlob $ postDir ++ "/*.markdown")
         makeItem "" >>= loadAndApplyTemplate "templates/posts.html" (postListContext posts)
                     >>= loadAndApplyTemplate "templates/default.html" (postListContext posts)
                     >>= relativizeUrls
 where postListContext :: [Item String] -> Context String
       postListContext posts =  listField "posts" postContext (return posts)
                             <> constField "title" title
                             <> constField "body_classes" bodyClasses
                             <> defaultContext
       postContext :: Context String
       postContext =  dateField "date" "%B %e, %Y"
                   <> field "url" stripIndex
                   <> constField "body_classes" bodyClasses
                   <> defaultContext

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

stripIndex :: Item a -> Compiler String
stripIndex = fmap (maybe empty (toUrl . go)) . getRoute . itemIdentifier
 where go :: FilePath -> String
       go name =
          case splitFileName name of
             (dir, "index.html") -> dir
             _ -> name
