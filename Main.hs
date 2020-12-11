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

wordProbability :: Word' -> Bow -> Double
wordProbability w bow = fromIntegral n / fromIntegral (wordsCount bow)
    where n = fromMaybe 0 $ M.lookup w $ bowToMap bow

readFileIfPossbile :: FilePath -> IO (Maybe T.Text)
readFileIfPossbile filePath = do
  bytes <- B.readFile filePath
  return $ case T.decodeUtf8' bytes of
    Right text -> Just text
    Left _ -> Nothing

bowFromFile :: FilePath -> IO (Maybe Bow)
bowFromFile filePath = do
  contents <- readFileIfPossbile filePath
  return (textToBow <$> contents)

bowFromFolder :: FilePath -> IO Bow
bowFromFolder folderPath = do
  fileNames <- listDirectory folderPath
  bows <- mapM (bowFromFile . (folderPath <>)) fileNames
  return $ fold $ mapMaybe id bows

data SpamModel = SpamModel
  { spamBow :: Bow
  , hamBow :: Bow
  }

spamModel :: IO SpamModel
spamModel = do
  spam <- bowFromFolder "./data/train/spam/"
  ham <- bowFromFolder "./data/train/ham/"
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

wordProbabilityHam :: SpamModel -> Word' -> Maybe Double
wordProbabilityHam sm@(SpamModel spamBow hamBow) w
  | seenWord w sm =
    let pws = wordProbability w spamBow
        phs = wordProbability w hamBow
        ps = pws + phs
     in Just (phs / (phs + pws))
  | otherwise = Nothing

textProbabilitySpam :: SpamModel -> T.Text -> Double
textProbabilitySpam sm text = (pp / (pp + product ips))
  where
    ws = normalizeTextToWords text
    ps = mapMaybe (wordProbabilitySpam sm) ws
    ips = map (\p -> 1.0 - p) ps
    pp = product ps

textProbabilityHam :: SpamModel -> T.Text -> Double
textProbabilityHam sm text = (pp / (pp + product ips))
  where
    ws = normalizeTextToWords text
    ps = mapMaybe (wordProbabilityHam sm) ws
    ips = map (\p -> 1.0 - p) ps
    pp = product ps

classifyText :: SpamModel -> T.Text -> (Double, Double)
classifyText sm text = (textProbabilitySpam sm text, textProbabilityHam sm text)

classifyFile :: SpamModel -> FilePath -> IO (Double, Double)
classifyFile sm filePath = classifyText sm <$> T.readFile filePath

classifyFolder :: SpamModel -> FilePath -> IO ()
classifyFolder sm folderPath = do
  fileNames <- listDirectory folderPath
  forM_ fileNames $ \fileName -> do
    let filePath = folderPath <> "/" <> fileName
    stats <- classifyFile sm filePath
    printf "%s -> %s\n" filePath (show stats)

main :: IO ()
main = putStrLn "Hello, Haskell!"
