{-# LANGUAGE RecordWildCards #-}

import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Prelude hiding (LT,GT)
import Data.Void
import Control.Monad (void, when)
import Control.Monad.State.Strict
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map.Strict as M

data Instruction = Instruction{var::String, op::Op, varCond::String, cmp::(Int -> Int -> Bool), valCond::Int}-- deriving (Show)

data Op = Inc Int | Dec Int deriving (Show, Eq)
data Cmp = E | LT | GT | LTE | GTE | NE deriving (Show)

-- Ex: b inc 5 if a > 1

type Parser = Parsec Void String

parseInst :: Parser Instruction
parseInst = do
  var <- many letterChar
  op <- space *> parseOp <* space
  varCond <- string "if "*> many letterChar <* space
  cmp <- parseCmp <* space
  valCond <- fromInteger <$> signedInteger
  return Instruction{..}

sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

parseCmp :: Parser (a->a->Bool)
parseCmp = parseE <|> parseNE <|> parseLTE <|> parseGTE <|> parseLT <|> parseGT
  where
    parseE = string "==" >> return (==)
    parseLT = string "<" >> return (<)
    parseGT = string ">" >> return (>)
    parseNE = string "!=" >> return (/=)
    parseLTE = string "<=" >> return (<=)
    parseGTE = string ">=" >> return (>=)

parseOp = do
  op <- many letterChar <* space
  num <- fromInteger <$> signedInteger
  return $ readOp op num
  
readOp "inc" n = Inc n
readOp "dec" n = Dec n
readOp _ _ = error "Wrong input for inc/dec"

signedInteger :: Parser Integer
signedInteger = L.signed sc L.decimal

type Computer = State (M.Map String Int)

evalCmp :: String -> (a->a->Bool) -> Int -> Computer Bool
evalCmp var cmp val = do
  varVal <- gets (M.findWithDefault 0 var)
  return $ cmp varVal val

evalOp :: String -> Op -> Computer ()
evalOp var op = do
  varVal <- gets (M.findWithDefault 0 var)
  max' <- gets (M.findWithDefault minBound "Max allocated")
  let newVal = getOp' op varVal
  modify (M.insert var newVal)
  modify (M.insert "Max allocated" (max max' newVal))

getOp' :: Op -> Int -> Int
getOp' (Inc a) b = b + a
getOp' (Dec a) b = b - a

interpret Instruction{..} = do
  cond <- evalCmp varCond cmp valCond
  when (cond) (evalOp var op)

getCmp :: Cmp -> (Int -> Int -> Bool)
getCmp E = (==)
getCmp NE = (/=)
getCmp LT = (<)
getCmp GT = (>)
getCmp LTE = (<=)
getCmp GTE = (>=)  

parseLine :: String -> Instruction
parseLine l = fromJust $ parseMaybe parseInst l

main = do
  insts <- map parseLine . lines <$> getContents
  let result = execState (mapM interpret insts) M.empty
  print . maximum . M.elems $ M.delete "Max allocated" result
  print (result M.! "Max allocated")
