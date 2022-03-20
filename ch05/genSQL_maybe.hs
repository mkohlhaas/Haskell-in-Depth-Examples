{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Foldable
import Data.Maybe
import Control.Monad.Writer
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type SQL = Text
type LineNumber = Int




genInsert :: Text -> Text -> SQL
genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processLine :: (LineNumber, Text) -> Maybe SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = pure $ genInsert s1 s2
processLine _ = pure mempty

genSQL :: Text -> Maybe SQL
genSQL txt = T.concat <$> traverse processLine (zip [1..] $ T.lines txt)

testData :: Text
testData = "Pen:Bob\nGlass:Mary:10\nPencil:Alice\nBook:Bob\nBottle"

testGenSQL :: IO ()
testGenSQL = do
  TIO.putStr $ fromJust $ genSQL testData





main :: IO ()
main = testGenSQL
