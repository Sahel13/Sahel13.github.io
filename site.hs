{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (forM_)
import Hakyll

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

    match "index.md" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

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
