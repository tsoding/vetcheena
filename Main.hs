{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
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

type Freq = Int
type Prob = Double

newtype Bow a = Bow
  { bowToMap :: M.Map Word' a
  } deriving (Show, Read)

summaryBow :: Bow Freq -> IO ()
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

wordToBow :: Word' -> Bow Freq
wordToBow w = Bow $ M.fromList [(w, 1)]

textToBow :: T.Text -> Bow Freq
textToBow = foldMap wordToBow . normalizeTextToWords

emptyBow = Bow M.empty

instance Semigroup (Bow Freq) where
  Bow bow1 <> Bow bow2 = Bow $ M.unionWith (+) bow1 bow2

instance Monoid (Bow Freq) where
  mempty = emptyBow

wordsCount :: Bow Freq -> Int
wordsCount (Bow bow) = sum $ map snd $ M.toList bow

wordProbability :: Word' -> Bow Prob -> Prob
wordProbability w bow = fromMaybe 0 $ M.lookup w $ bowToMap bow

freqToProb :: Bow Freq -> Bow Prob
freqToProb bow = Bow $ M.map (\x -> fromIntegral x / n) $ bowToMap bow
  where
    n = fromIntegral $ wordsCount bow

readFileIfPossbile :: FilePath -> IO (Maybe T.Text)
readFileIfPossbile filePath = do
  bytes <- B.readFile filePath
  return $ case T.decodeUtf8' bytes of
    Right text -> Just text
    Left _ -> Nothing

bowFromFile :: FilePath -> IO (Maybe (Bow Freq))
bowFromFile filePath = do
  contents <- readFileIfPossbile filePath
  return (textToBow <$> contents)

bowFromFolder :: FilePath -> IO (Bow Freq)
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold $ mapMaybe id bows

data SpamModel = SpamModel
  { spamBow :: !(Bow Prob)
  , hamBow :: !(Bow Prob)
  }

spamModel :: IO SpamModel
spamModel = do
  spam <- loadBowCsv "./data/spam.csv"
  ham <- loadBowCsv "./data/ham.csv"
  return $ SpamModel spam ham

seenWord :: Word' -> SpamModel -> Bool
seenWord w (SpamModel (Bow spamBow) (Bow hamBow)) = isJust sm || isJust hm
  where
    sm = M.lookup w spamBow
    hm = M.lookup w hamBow

wordProbabilitySpam :: SpamModel -> Word' -> Maybe Double
wordProbabilitySpam sm@(SpamModel spamBow hamBow) w
  | seenWord w sm =
    let pws = wordProbability w spamBow
        phs = wordProbability w hamBow
        ps = pws + phs
     in Just (pws / (pws + phs))
  | otherwise = Nothing

textProbabilitySpam :: SpamModel -> T.Text -> Double
textProbabilitySpam sm text = (pp / (pp + product ips))
  where
    ws = normalizeTextToWords text
    ps = mapMaybe (wordProbabilitySpam sm) ws
    ips = map (\p -> 1.0 - p) ps
    pp = product ps

classifyText :: SpamModel -> T.Text -> Double
classifyText sm text = textProbabilitySpam sm text

classifyFile :: SpamModel -> FilePath -> IO Double
classifyFile sm filePath = classifyText sm <$> T.readFile filePath

classifyFolder :: SpamModel -> FilePath -> IO ()
classifyFolder sm folderPath = do
  fileNames <- listDirectory folderPath
  forM_ fileNames $ \fileName -> do
    let filePath = folderPath <> "/" <> fileName
    stats <- classifyFile sm filePath
    printf "%s -> %s\n" filePath (show stats)

dumpBowCsv :: Show a => Bow a -> FilePath -> IO ()
dumpBowCsv bow filePath =
  writeFile filePath $
  unlines $
  map (\(Word' word, value) -> printf "%s,%s" word (show value)) $
  M.toList $
  bowToMap bow

loadBowCsv :: Read a => FilePath -> IO (Bow a)
loadBowCsv filePath =
  Bow .
  M.fromList .
  map
    (\line ->
       let [word, value] = T.splitOn "," line
        in (Word' word, read $ T.unpack value)) .
  T.lines <$>
  T.readFile filePath

train :: IO ()
train = do
  putStrLn "Training HAM..."
  ham <- freqToProb <$> bowFromFolder "./data/train/ham/"
  dumpBowCsv ham "./data/ham.csv"
  putStrLn "Training SPAM..."
  spam <- freqToProb <$> bowFromFolder "./data/train/spam/"
  dumpBowCsv spam "./data/spam.csv"

main :: IO ()
main = do
  sm <- spamModel
  putStrLn "Absolute HAM:"
  classifyFolder sm "./data/validate/ham/"
  putStrLn ""
  putStrLn "Absolute SPAM:"
  classifyFolder sm "./data/validate/spam/"
