{-# LANGUAGE OverloadedStrings #-}

-- | Parse a simple news list from Markdown for the homepage.
--
-- Expected format (newest first):
--
-- > - 2026-01: Released a new library ...
-- > - **2025-09**: Accepted a paper ...
module News
  ( NewsEntry (..),
    loadNewsEntries,
    newsEntryContext,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Hakyll
  ( Compiler,
    Context,
    Identifier,
    Item,
    defaultHakyllReaderOptions,
    defaultHakyllWriterOptions,
    field,
    itemBody,
    load,
    readPandocWith,
  )
import Text.Pandoc (runPure, writeMarkdown)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..), nullMeta)
import Text.Pandoc.Shared (stringify)

-- | Parsed news entry with a displayable date and body.
data NewsEntry = NewsEntry
  { newsDate :: String,
    newsBody :: String
  }

-- | Hakyll context fields: @date@ and @body@.
newsEntryContext :: Context NewsEntry
newsEntryContext =
  field "date" (return . newsDate . itemBody)
    <> field "body" (return . newsBody . itemBody)

-- | Load the news file, parse the first bullet list, and return the newest @limit@.
loadNewsEntries :: Identifier -> Int -> Compiler [NewsEntry]
loadNewsEntries identifier limit = do
  newsSource <- load identifier
  newsPandoc <- readPandocWith defaultHakyllReaderOptions newsSource
  case extractNewsEntries (itemBody newsPandoc) of
    Left err -> fail err
    Right entries -> pure (take limit entries)

extractNewsEntries :: Pandoc -> Either String [NewsEntry]
extractNewsEntries (Pandoc _ blocks) =
  case [items | BulletList items <- blocks] of
    (items : _) -> traverse parseNewsItem items
    [] -> Right []

-- | Parse a single bullet list item into a date + body.
parseNewsItem :: [Block] -> Either String NewsEntry
parseNewsItem blocks = case blocks of
  (Plain inlines : rest) -> parseNewsInlines Plain inlines rest
  (Para inlines : rest) -> parseNewsInlines Para inlines rest
  _ -> Left "News item must start with a plain paragraph."

-- | Split the first line into a date and the remainder, then render body blocks.
parseNewsInlines :: ([Inline] -> Block) -> [Inline] -> [Block] -> Either String NewsEntry
parseNewsInlines mkBlock inlines restBlocks = do
  (dateText, bodyInlines) <- parseNewsDate inlines
  let leadingBlocks = if null bodyInlines then [] else [mkBlock bodyInlines]
      bodyBlocks = leadingBlocks <> restBlocks
  body <- blocksToMarkdown bodyBlocks
  pure $ NewsEntry dateText body

-- | Extract a date from bold text or from leading text before a colon.
parseNewsDate :: [Inline] -> Either String (String, [Inline])
parseNewsDate inlines = case inlines of
  (Strong dateInlines : rest) ->
    let dateText = T.unpack (stringify dateInlines)
        remaining = dropWhile isNewsSeparator rest
     in Right (dateText, remaining)
  _ -> case splitOnColon inlines of
    Just (dateInlines, rest) ->
      let dateText = T.unpack (stringify dateInlines)
          remaining = dropWhile isNewsSeparator rest
       in Right (dateText, remaining)
    Nothing ->
      Left "News item must start with a date followed by a ':' (e.g. 2026-01: ...)."

-- | Split inline content on the first ':' found in a text segment.
splitOnColon :: [Inline] -> Maybe ([Inline], [Inline])
splitOnColon = go []
  where
    go _ [] = Nothing
    go acc (inline : rest) = case inline of
      Str txt ->
        let (before, after) = T.breakOn ":" txt
         in if T.null after
              then go (acc <> [inline]) rest
              else
                let leftPart = if T.null before then acc else acc <> [Str before]
                    remainingText = T.drop 1 after
                    rightPart = (if T.null remainingText then [] else [Str remainingText]) <> rest
                 in Just (leftPart, rightPart)
      _ -> go (acc <> [inline]) rest

-- | Skip separator tokens between date and body.
isNewsSeparator :: Inline -> Bool
isNewsSeparator inline = case inline of
  Str ":" -> True
  Str "—" -> True
  Str "-" -> True
  Space -> True
  SoftBreak -> True
  LineBreak -> True
  _ -> False

-- | Render blocks back to Markdown for template insertion.
blocksToMarkdown :: [Block] -> Either String String
blocksToMarkdown blocks =
  case runPure $ writeMarkdown defaultHakyllWriterOptions (Pandoc nullMeta blocks) of
    Left err -> Left (show err)
    Right body -> Right (T.unpack (T.strip body))
