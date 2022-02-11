module TinyRAM.Parser (firstLine, instruction) where

import           Control.Monad                     (void)

import           Text.Parsec

import           TinyRAM.Types.ImmediateOrRegister (ImmediateOrRegister (..))
import           TinyRAM.Types.Instruction         (Instruction (..))
import           TinyRAM.Types.Operations          (Operations (..))
import           TinyRAM.Types.Register            (Register (..))
import           TinyRAM.Types.RegisterCount       (RegisterCount (..))
import           TinyRAM.Types.Word                (Word (..))
import           TinyRAM.Types.WordSize            (WordSize (..))

import           TinyRAM.Operations


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
instruction :: Parser (Maybe Instruction)
instruction = do
  void $ many space
  void $ optionMaybe $ try $ manyTill anyChar (try (string ":"))
  mInstr <- optionMaybe $ do
    void $ many space
    nm <- try $ manyTill anyChar space
    rgs <-
      (try $ sepBy1 (many space *> many (noneOf ";,")) (string ",")) -- Without comments
      <|> (try $ sepBy1 (manyTill anyChar (try $ space <|> char ';')) (string ",")) -- With Comments
    return $ (getOpCode $ readOpCode nm, rgs)
  case mInstr of
    Just (op, rgts) -> do
      void $ many space
      void $ optionMaybe $ do
        void $ many anyChar
      let (ri, rj, a) = getRegisters rgts (op == getOpCode STORE)
      return $ Just $ Instruction op a ri rj
    _ -> return Nothing

getRegisters :: [String] -> Bool -> (Register, Register, ImmediateOrRegister)
getRegisters (r1:r2:r3: []) _ = (getRegister r1, getRegister r2, getImmediateOrRegister r3)
getRegisters (r1:r2:[]) True = (getRegister r2, 0, getImmediateOrRegister r1)
getRegisters (r1:r2:[]) False = (getRegister r1, 0, getImmediateOrRegister r2)
getRegisters (r1: []) _ = (0, 0, getImmediateOrRegister r1)
getRegisters _ _ = (0, 0, IsImmediate 0)

getRegister :: String -> Register
getRegister ('r': x) = Register $ read x
getRegister _        = 0

getImmediateOrRegister :: String -> ImmediateOrRegister
getImmediateOrRegister ('r': x) = IsRegister $ Register $ read x
getImmediateOrRegister x        = IsImmediate $ Word $ read x
