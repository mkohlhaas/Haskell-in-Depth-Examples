{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

import Data.Foldable
import Control.Monad.Writer
import Data.Text(Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO


type SQL = Text
type LineNumber = Int




genInsert :: Text -> Text -> SQL
genInsert s1 s2 = "INSERT INTO items VALUES ('" <> s1 <> "','" <> s2 <> "');\n"

processLine :: (LineNumber, Text) -> SQL
processLine (_, T.splitOn ":" -> [s1, s2]) = genInsert s1 s2


genSQL :: Text -> SQL
genSQL txt = T.concat $ fmap processLine (zip [1..] $ T.lines txt)

testData :: Text
testData = "Pen:Bob\nPencil:Alice\nBook:Bob"

testGenSQL :: IO ()
testGenSQL = do
  TIO.putStr $ genSQL testData





main :: IO ()
main = testGenSQL
