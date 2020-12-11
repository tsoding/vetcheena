module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Maybe
import System.Directory

newtype Bow = Bow
  { bowToMap :: M.Map T.Text Int
  } deriving (Show, Read)

wordToBow :: T.Text -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . T.words

emptyBow = Bow M.empty

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = emptyBow

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: T.Text -> Bow -> Float
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
    where n = fromMaybe 0 $ M.lookup w $ bowToMap bow

bowFromFile :: FilePath -> IO Bow
bowFromFile filePath =
    textToBow <$> T.readFile filePath

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold bows

spamBow :: IO Bow
spamBow = bowFromFolder "./data/spam/"

hamBow :: IO Bow
hamBow = bowFromFolder "./data/ham/"

main :: IO ()
main = putStrLn "Hello, Haskell!"
