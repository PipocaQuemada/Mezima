module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as IO

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Text.ParserCombinators.Parsec hiding (spaces)

import Data.IORef

import Control.Monad.Except

main :: IO ()
main = do
  file <- readFile "samples/add.mzm"
  print $ parse parseExpr "scheme" file
  print . parse parseExpr "scheme" $ map reverseParens file
  putStrLn $ map reverseParens file
  
reverseParens '(' = ')' 
reverseParens ')' = '(' 
reverseParens x = x 

----------------------
-- Expr
----------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Bool Bool
  deriving (Eq)

instance Show LispVal where
  show (Atom a) = a
  show (List vals) = "(" ++ L.intercalate " " (show <$> vals) ++ ")"
  show (DottedList vals last) = "(" ++ L.intercalate " " (show <$> vals) ++ " . " ++ show last ++ ")"
  show (String s) = '"' : s ++ ['"']
  show (Bool True) = "#כן" 
  show (Bool False) = "#לֹא" 

----------------------
-- Parsing
----------------------

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

hebrewLetter :: Parser Char
hebrewLetter = oneOf "אבגדהוזחטיכלמנסעפצקרשתךםןףץ"


parseString :: Parser LispVal
parseString = do
                char '\''
                x <- many (noneOf "\'")
                char '\''
                return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
              first <- hebrewLetter <|> symbol
              rest <- many (hebrewLetter <|> digit <|> symbol)
              let atom = first:rest
              return $ case atom of 
                         "#כן" -> Bool True
                         "#לֹא" -> Bool False
                         _    -> Atom atom

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '׳'
    x <- parseExpr
    return $ List [Atom "גרשּּ", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


------------------------------------------------------
-- errors
------------------------------------------------------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError



------------------------------------------------------
-- eval
------------------------------------------------------

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Bool _) = return val
eval (List [Atom "גרשּּ", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [
  ("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("מודולוס", numericBinop mod),
  ("תוצאה", numericBinop quot),
  ("שארית", numericBinop rem)
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("חוט=?", strBoolBinop (==)),
  ("חוט<?", strBoolBinop (<)),
  ("חוט>?", strBoolBinop (>)),
  ("חוט<=?", strBoolBinop (<=)),
  ("חוט>=?", strBoolBinop (>=)),
]

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                      right <- unpacker $ args !! 1
                                      return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . Number . foldl1 op



unpackNum :: LispVal -> ThrowsError Integer
unpackNum (List [Atom "גרשּּ", Atom x]) = gemmatria x
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum



gematriaMap :: M.Map Char Int
gematriaMap = M.fromList [
    ('א', 1), ('ב', 2), ('ג', 3), ('ד', 4), ('ה', 5), ('ו', 6), ('ז', 7), ('ח', 8),
    ('ט', 9), ('י', 10), ('כ', 20), ('ל', 30), ('מ', 40), ('נ', 50), ('ס', 60), ('ע', 70),
    ('פ', 80), ('צ', 90), ('ק', 100), ('ר', 200), ('ש', 300), ('ת', 400)
  ]


gemmatria x = sum $ map valueOf x
  where valueOf x = fromMaybe 0 (M.lookup x gematriaMap)

