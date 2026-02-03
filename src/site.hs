{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Monad (forM_, (<=<))
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.IO.Handle (BufferMode (NoBuffering), Handle, hSetBuffering)
import Hakyll
import News (loadNewsEntries, newsEntryContext)
import System.Process (runInteractiveCommand)
import Text.Pandoc.Builder (setMeta)
import Text.Pandoc.Definition (Block (..), Inline (..), MathType (..), Pandoc)
import Text.Pandoc.Highlighting (Style, pygments, styleToCss)
import Text.Pandoc.Options (WriterOptions (..))
import Text.Pandoc.Walk (walk, walkM)

-- GitHub Pages expects the website to be in `/docs`.
myConfig :: Configuration
myConfig = defaultConfiguration {destinationDirectory = "docs"}

main :: IO ()
main = hakyllWith myConfig $ do
  match "images/*" $ do
    route idRoute
    compile copyFileCompiler

  match "styles/main.scss" $ do
    route $ setExtension "css"
    compile compressScssCompiler

  forM_ ["styles/*.css", "styles/fonts/*"] $ \f -> match f $ do
    route idRoute
    compile copyFileCompiler

  match "templates/*" $ compile templateBodyCompiler

  match "news.md" $ do
    compile getResourceBody

  -- Citations
  match "bib/american-statistical-association.csl" $ compile cslCompiler
  match "bib/bibliography.bib" $ compile biblioCompiler

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      myPandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" defaultContext
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  -- Syntax highlighting
  create ["styles/syntax.css"] $ do
    route idRoute
    compile $ do
      makeItem $ styleToCss pandocCodeStyle

  match "index.md" $ do
    route $ setExtension "html"
    compile $ do
      -- Load the latest 3 posts to show on the homepage.
      posts <- fmap (take 3) . recentFirst =<< loadAll "posts/*"
      newsEntries <- loadNewsEntries "news.md" 3
      newsItems <- mapM makeItem newsEntries
      let newsCtx = listField "recentNews" newsEntryContext (return newsItems)
          postsCtx = listField "recentPosts" defaultContext (return posts)
          ctx = newsCtx <> postsCtx <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= renderPandoc
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls

  create ["atom.xml"] $ do
    route idRoute
    compile $ do
      posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
      renderAtom myFeedConfiguration (bodyField "description" <> defaultContext) posts

pandocCodeStyle :: Style
pandocCodeStyle = pygments

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
  pandocItemCompilerWithTransformM
    defaultHakyllReaderOptions
    defaultHakyllWriterOptions { writerHighlightStyle = Just pandocCodeStyle }
    ( traverse hlKaTeX
        <=< processBib
    )

-- Compiler for SCSS.
-- Requires the `sass` command line tool.
-- Source : https://github.com/jjduhamel/blog/blob/master/site.hs
compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
  fmap (fmap compressCss) $
    getResourceString
      >>= withItemBody
        ( unixFilter
            "sass"
            [ "--stdin",
              "--style",
              "compressed",
              "--load-path",
              "./styles"
            ]
        )

-- KaTeX compiler for Pandoc from https://tony-zorman.com/posts/katex-with-hakyll.html
hlKaTeX :: Pandoc -> Compiler Pandoc
hlKaTeX pandoc = recompilingUnsafeCompiler do
  (hin, hout, _, _) <- runInteractiveCommand "deno run scripts/math.ts"
  hSetBuffering hin NoBuffering
  hSetBuffering hout NoBuffering

  (`walkM` pandoc) \case
    Math mathType (T.unwords . T.lines . T.strip -> text) -> do
      let math :: Text =
            foldl'
              (\str (repl, with) -> T.replace repl with str)
              case mathType of
                DisplayMath {-s-} -> ":DISPLAY " <> text
                InlineMath {-s-} -> text
              macros
      T.hPutStrLn hin math
      RawInline "html" <$> getResponse hout
    block -> pure block
  where
    -- KaTeX might sent the input back as multiple lines if it involves a
    -- matrix of coordinates. The big assumption here is that it does so only
    -- when matrices—or other such constructs—are involved, and not when it
    -- sends back "normal" HTML.
    getResponse :: Handle -> IO Text
    getResponse handle = go ""
      where
        go :: Text -> IO Text
        go !str = do
          more <- (str <>) <$> T.hGetLine handle
          if ">" `T.isSuffixOf` more -- end of HTML snippet
            then pure more
            else go more

    macros :: [(Text, Text)]
    macros =
      [ ("≔", "\\mathrel{\\vcenter{:}}="),
        ("\\defeq", "\\mathrel{\\vcenter{:}}="),
        ("\\cat", "\\mathcal"),
        ("\\kVect", "\\mathsf{Vect}_{\\mathtt{k}}")
      ]

processBib :: Item Pandoc -> Compiler (Item Pandoc)
processBib pandoc = do
  csl <- load @CSL "bib/american-statistical-association.csl"
  bib <- load @Biblio "bib/bibliography.bib"
  -- Link citations.
  p <- withItemBody (pure . setMeta "link-citations" True) pandoc
  fmap insertRefHeading <$> processPandocBiblio csl bib p
  where
    -- Insert a heading for the citations.
    insertRefHeading :: Pandoc -> Pandoc
    insertRefHeading = walk $ concatMap \case
      d@(Div ("refs", _, _) _) -> [Header 2 ("references", [], []) [Str "References"], d]
      block -> [block]

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Sahel Iqbal · Blog"
    , feedDescription = "A blog about various technical topics that interest me."
    , feedAuthorName  = "Sahel Iqbal"
    , feedAuthorEmail = "sahel13miqba@proton.me"
    , feedRoot        = "https://sahel13.github.io/"
    }
