{-# LANGUAGE OverloadedStrings #-}

module Data.Wavefront where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr)
import Data.Void (Void)
import Linear.V2
import Linear.V3
import Linear.V4
import Text.Megaparsec (Parsec, (<|>))
import Text.Megaparsec qualified as P
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L

type Parser = Parsec Void ByteString

sc :: Parser ()
sc = L.space B.space1 (L.skipLineComment "#") (L.skipBlockComment "{=" "=}")

float :: Parser Double
float = L.signed sc L.float

vertexParser :: Parser (V3 Double)
vertexParser = do
  _ <- B.string "v "
  vec <-
    V3
      <$> (float >>= (pure . realToFrac))
      <*> (B.string " " *> float >>= (pure . realToFrac))
      <*> (B.string " " *> float >>= (pure . realToFrac))
  w <- P.option 1.0 (B.string " " *> float >>= (pure . realToFrac))
  pure $ (/ w) <$> vec

normalParser :: Parser (V3 Double)
normalParser = do
  _ <- B.string "vn"
  V3
    <$> (B.string " " *> float >>= (pure . realToFrac))
    <*> (B.string " " *> float >>= (pure . realToFrac))
    <*> (B.string " " *> float >>= (pure . realToFrac))

uvParser :: Parser (V2 Double)
uvParser = do
  _ <- B.string "vt"
  (V3 u v w) <-
    V3
      <$> (B.string " " *> float >>= (pure . realToFrac))
      <*> P.option 0.0 (B.string " " *> float >>= (pure . realToFrac))
      <*> P.option 0.0 (B.string " " *> float >>= (pure . realToFrac))
  -- if u > 1.0 || v > 1.0
  --   then fail "Texture coordinates out of bounds"
  --   else
  pure $ V2 u v

data FaceRef = FaceRef
  { vcIndex :: Int
  , vtIndex :: Maybe Int
  , vnIndex :: Maybe Int
  }
  deriving Show

type Face = [FaceRef]

faceRefParser :: Parser FaceRef
faceRefParser = do
  v <- L.decimal
  mt <- P.optional (B.string "/" *> P.optional L.decimal)
  mn <- case mt of
    Nothing -> pure Nothing -- v
    Just Nothing -> P.optional (B.string "/" *> L.decimal) -- v//
    Just (Just _vt) -> P.optional (B.string "/" *> L.decimal) -- v/vt or v/vt/vn
  pure $ FaceRef v (mt >>= id) mn

faceParser :: Parser Face
faceParser =
  B.string "f " *> P.some (faceRefParser <* sc)

data Object = Object
  { name :: ByteString
  , verticies :: [V3 Double]
  , normals :: [V3 Double]
  , uvs :: [V2 Double]
  , faces :: [Face]
  }
  deriving Show

smoothingParser :: Parser ()
smoothingParser = do
  _ <- B.string "s "
  _ <- P.takeWhileP (Just "s smoothing group") (/= 10)
  pure ()

data ObjEntry
  = EntryV (V3 Double)
  | EntryN (V3 Double)
  | EntryUV (V2 Double)
  | EntryF Face

entryParser =
  EntryV
    <$> vertexParser <|> EntryN
    <$> normalParser <|> EntryUV
    <$> uvParser <|> EntryF
    <$> faceParser

partitionEntries
  :: [ObjEntry] -> ([V3 Double], [V3 Double], [V2 Double], [Face])
partitionEntries = foldr go ([], [], [], [])
 where
  go e (vs, ns, uvs, fs) = case e of
    EntryV v -> (v : vs, ns, uvs, fs)
    EntryN n -> (vs, n : ns, uvs, fs)
    EntryUV u -> (vs, ns, u : uvs, fs)
    EntryF f -> (vs, ns, uvs, f : fs)

objectParser :: Parser Object
objectParser = do
  name <- B.string "o " *> P.takeWhileP (Just "object name") (/= 10)
  sc
  entries <- P.many (sc *> entryParser)
  let (vs, ns, uvs, fs) = partitionEntries entries
  pure (Object name vs ns uvs fs)

parseWavefront :: Parser [Object]
parseWavefront = sc *> P.many objectParser

loadWavefront :: FilePath -> IO (Either String Object)
loadWavefront file = do
  f <- BS.readFile file
  let a = first P.errorBundlePretty $ P.runParser (sc *> objectParser) file f
  pure a

runParser p =
  either (error . P.errorBundlePretty) id
    . P.runParser p "<stdin>"
