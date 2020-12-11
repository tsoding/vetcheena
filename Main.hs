module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Maybe
import System.Directory
import Data.Char
import Text.Printf
import Control.Monad
import Data.List
import Data.Function

newtype Word' =
  Word' T.Text
  deriving (Show, Read, Eq, Ord)

mkWord :: T.Text -> Word'
mkWord = Word' . T.toUpper

wordToText :: Word' -> T.Text
wordToText (Word' t) = t

newtype Bow = Bow
  { bowToMap :: M.Map Word' Int
  } deriving (Show, Read)

summaryBow :: Bow -> IO ()
summaryBow (Bow bow) = do
  forM_ (sortBy (compare `on` snd) $ M.toList bow) $ \(w, f) ->
    printf "%s -> %d\n" (wordToText w) f

normalizeTextToWords :: T.Text -> [Word']
normalizeTextToWords =
  map mkWord .
  T.words .
  T.map
    (\x ->
       if isAlphaNum x
         then x
         else ' ')

wordToBow :: Word' -> Bow
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow
textToBow = foldMap wordToBow . normalizeTextToWords

emptyBow = Bow M.empty

instance Semigroup Bow where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid Bow where
  mempty = emptyBow

wordsCount :: Bow -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: Word' -> Bow -> Float
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
spamBow = bowFromFolder "./data/train/spam/"

hamBow :: IO Bow
hamBow = bowFromFolder "./data/train/ham/"

wordProbabilitySpam :: Word' -> IO Float
wordProbabilitySpam w = do
  pws <- wordProbability w <$> spamBow
  phs <- wordProbability w <$> hamBow
  let ps = pws + phs
  return $ if ps == 0.0 then 0.0 else pws / (pws + phs)

textProbabilitySpam :: T.Text -> IO Float
textProbabilitySpam text = do
  let ws = normalizeTextToWords text
  ps <- mapM wordProbabilitySpam ws
  let ips = map (\p -> 1.0 - p) ps
  let pp = product ps
  return (pp / (pp + product ips))

main :: IO ()
main = putStrLn "Hello, Haskell!"
