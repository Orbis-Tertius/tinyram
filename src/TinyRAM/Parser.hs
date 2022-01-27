module TinyRAM.Parser (firstLine, instruction) where

import           Control.Monad               (void)

import           Text.Parsec

import           TinyRAM.Types.RegisterCount (RegisterCount (..))
import           TinyRAM.Types.WordSize      (WordSize (..))


type Parser = Parsec String ()

-- Basic helpers
number :: Parser String
number =  many1 digit

float :: Parser Double
float = read <$> parser  where
   parser = (++) <$> number <*> (option "" $ (:) <$> char '.'  <*> number )

int :: Parser Int
int = read <$> number

-- ; TinyRAM V=1.00 W=W K=K
-- Parse the first line of TinyRAM program.
firstLine :: Parser (WordSize, RegisterCount)
firstLine = do
  void $ string ";"
  void $ many space
  void $ string "TinyRAM"
  void $ many space
  void $ string "V="
  void $ float
  void $ many space
  void $ string "W="
  wValue <- int
  void $ many space
  void $ string "K="
  kValue <- int
  return (WordSize wValue, RegisterCount kValue)

-- Parse each Instruction followed by a comment.
instruction :: Parser (Maybe (String, [String]))
instruction = do
  void $ many space
  void $ optionMaybe $ try $ manyTill anyChar (try (string ":"))
  instr <- optionMaybe $ do
    void $ many space
    nm <- try $ manyTill anyChar space
    rgs <-
      (try $ sepBy1 (many space *> many (noneOf ";,")) (string ",")) -- Without comments
      <|> (try $ sepBy1 (manyTill anyChar (try $ space <|> char ';')) (string ",")) -- With Comments
    return $ (nm, rgs)
  void $ many space
  void $ optionMaybe $ do
    void $ many anyChar
  return instr
