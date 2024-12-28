module Main where

import System.IO
import System.Environment

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe

import Text.ParserCombinators.Parsec hiding (spaces)

import Data.IORef

import Control.Monad.Except

main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runFile $ args !! 0
               _ -> putStrLn "Program takes only 0 or 1 argument"

runFile filename = do
  file <- readFile filename 
  evalAndPrint . stripRTLOverride $ file
  

stripRTLOverride ('\x202e':xs) = xs
stripRTLOverride xs = xs

----------------------
-- Expr
----------------------

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | String String
             | Bool Bool
  deriving (Eq)

disp (Atom a) = a
disp (List [Atom "גרש", x]) = '׳' : show x 
disp (List vals) = "(" ++ L.intercalate " " (show <$> vals) ++ ")"
disp (DottedList vals last) = "(" ++ L.intercalate " " (show <$> vals) ++ " . " ++ show last ++ ")"
disp (String s) = '"' : s ++ ['"']
disp (Bool True) = "#כן" 
disp (Bool False) = "#לֹא" 

instance (Show LispVal) where
  show = disp

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

-- For some reason, the  macbook Hebrew - Qwerty keyboard source gives you reversed parens.
-- They display correctly in vim in rtl mode, but show reversed in bidirectional text.
-- So, just try parsing both with normal parens and reversed parens to make things easier.
readExpr :: String -> Either LispError LispVal
readExpr input = case fallback (parse parseExpr "mezima" input) (parse parseExpr "mezima" $ fmap reverseParens input) of
     Left err -> throwError $ Parser err
     Right val -> return val

fallback :: Either a b -> Either a b -> Either a b
fallback (Left b) f = f
fallback (Right a) f = Right a

reverseParens '(' = ')' 
reverseParens ')' = '(' 
reverseParens x = x 

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
                                       ++ " args; found values " ++ show found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

trapError action = catchError action (return . show)

extractValue :: Either b a -> a
extractValue (Right val) = val


------------------------------------------------------
-- eval
------------------------------------------------------

eval :: LispVal -> Either LispError LispVal
eval val@(String _) = return val
eval val@(Bool _) = return val
eval (List [Atom "אם", pred, t, f]) = eval pred >>= \x -> if x == Bool False then eval f else eval t
eval (List [Atom "גרשּּ", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> Either LispError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives = [
    ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("מודולוס", numericBinop mod),
    ("תוצאה", numericBinop quot),
    ("שארית", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("כנס", cons),
    ("כאר", car),
    ("כדר", cdr),
    ("שווה", eqv),
    ("חוט=?", strBoolBinop (==)),
    ("חוט<?", strBoolBinop (<)),
    ("חוט>?", strBoolBinop (>)),
    ("חוט<=?", strBoolBinop (<=)),
    ("חוט>=?", strBoolBinop (>=))
  ]

car :: [LispVal] -> Either LispError LispVal
car [List (x : xs)]         = return x
car [DottedList (x : xs) _] = return x
car [badArg]                = throwError $ TypeMismatch "pair" badArg
car badArgList              = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> Either LispError LispVal
cdr [List (x : xs)]         = return $ List xs
cdr [DottedList [_] x]      = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg]                = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> Either LispError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> Either LispError LispVal
eqv [(Bool arg1), (Bool arg2)]             = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)]         = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)]             = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)]             = return $ Bool $ (length arg1 == length arg2) && 
                                                             (all eqvPair $ zip arg1 arg2)
     where eqvPair (x1, x2) = case eqv [x1, x2] of
                                Left err -> False
                                Right (Bool val) -> val
eqv [_, _]                                 = return $ Bool False
eqv badArgList                             = throwError $ NumArgs 2 badArgList

boolBinop :: (LispVal -> Either LispError a) -> (a -> a -> Bool) -> [LispVal] -> Either LispError LispVal
boolBinop unpacker op args = if length args /= 2 
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop  = boolBinop unpackNum
strBoolBinop  = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> Either LispError String
unpackStr (String s) = return s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> Either LispError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
numericBinop op           []  = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params        = mapM unpackNum params >>= return . number . foldl1 op

number x = List [Atom "גרשּּ", Atom $ convertToHebrew x]

unpackNum :: LispVal -> Either LispError Integer
unpackNum (List [Atom "גרשּּ", Atom x]) = return $ gemmatria x
unpackNum (List [n]) = unpackNum n
unpackNum (Atom x) = return $ gemmatria x
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

gematriaMap :: M.Map Char Integer
gematriaMap = M.fromList [
    ('א', 1), ('ב', 2), ('ג', 3), ('ד', 4), ('ה', 5), ('ו', 6), ('ז', 7), ('ח', 8),
    ('ט', 9), ('י', 10), ('כ', 20), ('ל', 30), ('מ', 40), ('נ', 50), ('ס', 60), ('ע', 70),
    ('פ', 80), ('צ', 90), ('ק', 100), ('ר', 200), ('ש', 300), ('ת', 400)
  ]

gemmatria x = sum $ map valueOf x
  where valueOf x = fromMaybe 0 (M.lookup x gematriaMap)

-- A mapping of the Hebrew numeral system
hebrewNumerals :: M.Map Integer String
hebrewNumerals = M.fromList
  [ 
    (900, "קתת")      -- 300 = Shin
  , (800, "תת")      -- 300 = Shin
  , (700, "שת")      -- 300 = Shin
  , (600, "רת")      -- 300 = Shin
  , (500, "קת")      -- 300 = Shin
  , (400, "ת")      -- 400 = Tav
  , (300, "ש")      -- 300 = Shin
  , (200, "ר")      -- 200 = Resh
  , (100, "ק")      -- 100 = Qof
  , (90, "צ")       -- 90 = Tsade
  , (80, "פ")       -- 80 = Pe
  , (70, "ע")       -- 70 = Ayin
  , (60, "ס")       -- 60 = Samekh
  , (50, "נ")       -- 50 = Nun
  , (40, "מ")       -- 40 = Mem
  , (30, "ל")       -- 30 = Lamed
  , (20, "כ")       -- 20 = Kaf
  , (10, "י")       -- 10 = Yod
  , (9, "ט")        -- 9 = Tet
  , (8, "ח")        -- 8 = Chet
  , (7, "ז")        -- 7 = Zayin
  , (6, "ו")        -- 6 = Vav
  , (5, "ה")        -- 5 = Hey
  , (4, "ד")        -- 4 = Daled
  , (3, "ג")        -- 3 = Gimel
  , (2, "ב")        -- 2 = Bet
  , (1, "א")        -- 1 = Aleph
  ]

-- Function to convert a number to its Hebrew numeral
convertToHebrew :: Integer -> String
convertToHebrew 0 = ""  -- No numeral for 0, you can add it if you like
convertToHebrew n = L.intercalate "׳" . reverse . fmap numberChunkToHebrew . chunks $ n
  where
    chunks = L.unfoldr (\x ->if x > 0 then Just (x `mod` 1000, x `div` 1000) else Nothing)
 
-- convert a number from 1 - 999 into a hebrew numeral
numberChunkToHebrew 0 = ""
numberChunkToHebrew n =  join . catMaybes $ [numeral hundreds, numeral tens, numeral ones]
  where 
   ones = n `mod` 10
   tens = (n `mod` 100) - ones
   hundreds = n - ones - tens
   numeral x = M.lookup x hebrewNumerals
    
------------------------------------------------------
-- repl
------------------------------------------------------

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr =  evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
   result <- prompt
   if pred result 
      then return ()
      else action result >> until_ pred prompt action


runRepl :: IO ()
runRepl = until_ (== "לצאת") (readPrompt "מזמה>>> ") evalAndPrint


-- examples 

add1 = "(+ ׳א ׳א)"

