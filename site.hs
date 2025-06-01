{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
import Control.Monad (forM_)
import Hakyll
import Data.List (foldl')
import Data.Text    qualified as T
import Data.Text (Text)
import Data.Text.IO qualified as T
import GHC.IO.Handle (BufferMode (NoBuffering), Handle, hSetBuffering)
import System.Process (runInteractiveCommand)
import Text.Pandoc.Definition (Block (..), Inline (..), MathType (..), Pandoc)
import Text.Pandoc.Walk (walk, walkM)

-- GitHub Pages expects the website to be in `/docs`.
myConfig :: Configuration
myConfig = defaultConfiguration { destinationDirectory = "docs" }

main :: IO ()
main = hakyllWith myConfig $ do
    forM_ ["images/*", "files/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match "styles/main.scss" $ do
        route $ setExtension "css"
        compile compressScssCompiler

    forM_ ["styles/*.css", "styles/fonts/*"] $ \f -> match f $ do
        route   idRoute
        compile copyFileCompiler

    match "index.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
    pandocCompilerWithTransformM
        defaultHakyllReaderOptions
        defaultHakyllWriterOptions
        hlKaTeX

-- Compiler for SCSS.
-- Requires the `sass` command line tool.
-- Source : https://github.com/jjduhamel/blog/blob/master/site.hs
compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
    fmap (fmap compressCss) $
        getResourceString
        >>= withItemBody (unixFilter "sass" [ "--stdin"
                                            , "--style", "compressed"
                                            , "--load-path", "./styles"
                                            ])

-- KaTeX compiler for Pandoc from https://tony-zorman.com/posts/katex-with-hakyll.html
hlKaTeX :: Pandoc -> Compiler Pandoc
hlKaTeX pandoc = recompilingUnsafeCompiler do
  (hin, hout, _, _) <- runInteractiveCommand "deno run scripts/math.ts"
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering

  (`walkM` pandoc) \case
    Math mathType (T.unwords . T.lines . T.strip -> text) -> do
      let math :: Text
            = foldl' (\str (repl, with) -> T.replace repl with str)
                     case mathType of
                       DisplayMath{-s-} -> ":DISPLAY " <> text
                       InlineMath{-s-}  ->                text
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
      if ">" `T.isSuffixOf` more  -- end of HTML snippet
      then pure more
      else go   more

  macros :: [(Text, Text)]
  macros =
    [ ("≔"       , "\\mathrel{\\vcenter{:}}=")
    , ("\\defeq" , "\\mathrel{\\vcenter{:}}=")
    , ("\\to"    , "\\longrightarrow")
    , ("\\mapsto", "\\longmapsto")
    , ("\\cat"   , "\\mathcal")
    , ("\\kVect" , "\\mathsf{Vect}_{\\mathtt{k}}")
    ]

