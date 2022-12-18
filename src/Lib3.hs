{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant if" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Text.Read
import Data.Char
import Types ( Document (DNull, DList, DInteger, DMap, DString),FromDocument, fromDocument)
import Lib1 (State(..),emptyState)
import Lib2 (getTabs)
import Data.Aeson (Array)
import Data.Aeson.Encoding (list, string)
import GHC.Generics (D)
import Data.List (stripPrefix)
import Data.Vector.Internal.Check (check)
import Test.QuickCheck(quickCheck)


-- IMPLEMENT
-- Parses a document from yaml

parseDocument :: String -> Either String Document
parseDocument str = (fst) <$> (dropTitle str)

dropTitle :: String -> Either String (Document, String)
dropTitle input = do
  let str = convertString input [] 0 0
  case take 4 str of 
    "---\n" -> case take 6 str of
             "---\n-\n" -> do 
               (x, y) <- many (drop 3 str) 0 fromSecond
               return (DList x, y)
             _ -> startParse (drop 4 str ) 0
    _ -> startParse str 0

startParse :: String -> Int -> Either String (Document, String)
startParse str lvl = dataCheck (parseComplexType str lvl) (parseBaseType str)

dataCheck :: Either String (a, String) -> Either String (a, String) -> Either String (a, String)
dataCheck parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

parseComplexType :: String -> Int -> Either String (Document, String)
parseComplexType str _ | (getSpaces str 0) == (length str) = Left "Empty string"
parseComplexType str lvl = do
    let spaces = getSpaces str 0
    case take 2 str of
        "\n " -> parseComplexType (drop 1 str) 0
        "-\n" -> case lvl == 0 of
                 True  -> parseDList str 0
                 False -> parseComplexType (drop 2 str) 0
        "  " ->  
            case last (head (words str)) of
            ':' -> parseDMap (drop spaces str) spaces []
            _   -> 
                  if spaces > lvl
                  then parseDList (drop spaces str) spaces
                  else (
                    if spaces == lvl 
                    then parseDList (drop spaces str) spaces 
                    else parseDList (drop (spaces + 2) str) spaces)
        "{}" -> parseDMap str spaces []
        _    -> case last (head (words str)) of  
                ':' -> parseDMap str lvl [] 
                _   -> parseDList str lvl

parseDMap :: String -> Int -> [(String, Document)] -> Either String (Document, String)
parseDMap str _ _ | head (words str) == "{}" = Right (DMap [], drop 2 str)
parseDMap str lvl list = do
  (x1, y1) <- readFirst str
  case compare (getSpaces y1 0) 0 of
    EQ -> do 
        (x2, y2) <- startParse (drop 1 y1) lvl
        listM <- addElem ((remove'' x1), x2) list y2
        let next = drop (getNewLines y2 0) y2
        case next of
            "" -> Right (DMap (fst listM), next)
            _  -> case last (head (words next)) of
                --':' -> parseDMap (drop (getSpaces next 0) next) lvl (fst listM)
                ':' -> if lvl == (getSpaces next 0)
                    then (parseDMap (drop (getSpaces next 0) next) lvl (fst listM))
                    else (
                        if lvl < (getSpaces next 0)
                        then (parseDMap (drop (getSpaces next 0) next) (getSpaces next 0) (fst listM))
                        else (return (DMap (fst listM), snd listM)))
                _   -> return (DMap (fst listM), snd listM)

    _ -> do   
        (x2, y2) <- parseBaseType (drop 1 y1)
        listM <- addElem ((remove'' x1), x2) list y2
        let next = drop (getNewLines y2 0) y2
        case next of
            "" -> Right (DMap (fst listM), next)
            _  -> case last (head (words next)) of
               --':' -> parseDMap (drop (getSpaces next 0) next) lvl (fst listM)
                ':' -> if lvl == (getSpaces next 0)
                    then (parseDMap (drop (getSpaces next 0) next) lvl (fst listM))
                    else (
                        if lvl < (getSpaces next 0)
                        then (parseDMap (drop (getSpaces next 0) next) (getSpaces next 0) (fst listM))
                        else (return (DMap (fst listM), snd listM)))
                _   -> return (DMap (fst listM), snd listM)

addElem :: a -> [a] -> String -> Either String ([a], String)
addElem tuple list left = Right ((addToListEnd tuple list), left)  

readFirst :: String -> Either String (String, String)
readFirst str = do
  let s = reverse (head (words str)) 
      l = length s
  return (reverse (drop 1 s), drop l str)


parseDList :: String -> Int -> Either String (Document, String)
parseDList str lvl | head (words str) == "[]" = Right (DList [], drop 2 str)
parseDList str lvl = do
    (_, x1) <- parseChar '-' str
    case isSpace (head x1) of 
      False -> Left "Its negative number"
      True -> do
        (y, x2) <- optional (drop 1 x1) lvl elems
        (l, x3) <- dataCheck (parseChar '-' x2) (parseChar '\n' x2)
        case y of
          Just a  -> case l of
              ' ' -> return (DList a, x3)
              _ -> return (DList a, l:x3)
          Nothing -> return (DList [], x3)

optional :: String -> Int -> (String -> Int -> Either String (a, String)) -> Either String (Maybe a, String)
optional str lvl parser =
    case parser str lvl of
        Left e -> Right (Nothing, str)
        Right (i, r) -> Right (Just i, r)

elems :: String -> Int -> Either String ([Document], String)
elems str lvl = do 
    (i, r) <- dataCheck (multiple str lvl) (single str lvl)
    return (i, r)

single :: String -> Int -> Either String ([Document], String)
single str lvl = do
  (i, r) <- startParse str lvl
  return ([i], r)

multiple :: String -> Int -> Either String ([Document], String)
multiple str lvl = do
  (i1, r1) <- startParse str lvl
  (i2, r2) <- many r1 lvl fromSecond
  return (i1:i2, r2)
    
many :: String -> Int -> (String -> Int -> Either String (a, String)) -> Either String ([a], String)
many str lvl parser = many' str []
    where
        many' s [] =
             case parser s lvl of
                Left e -> Left e
                Right (i, r) -> many' r [i]
        many' s acc =
            case parser s lvl of
                Left e -> Right (reverse acc, s)
                Right (i, r) -> many' r (i:acc)

fromSecond :: String -> Int -> Either String (Document, String)
fromSecond str lvl | str == "\n" = Left "End of the file"
                   | str == "" = Left "End of the file"
                   | last (head (words str)) == ':' = Left "DMap type variable found"
fromSecond str lvl = do
    (x, r1) <- dataCheck (parseChar '-' str) (parseChar '\n' str) 
    case takeWhile isSpace r1 of 
        " " -> startParse (drop 1 r1) lvl
        _ ->    
            case stripPrefix "-\n" r1 of
                Just a -> 
                    if lvl == (getSpaces a 0)
                    then (Left "List closes")
                    else (if lvl > (getSpaces a 0) 
                          then(Left "List closes")
                          else(do
                          (i, r2) <- startParse a 0
                          return (i, r2)) )
                Nothing -> 
                    if getSpaces (drop (getNewLines r1 0) r1) 0 == lvl
                    then ( if (lvl == 0) && (last (head (tail (words r1))) == ':')
                           then (do
                            (i, r2) <- startParse (drop (lvl + 2) r1) (lvl + 2)
                            return (i, r2))
                           else (
                             if (head (words r1)) == "-" && (last (head (tail (words r1))) == ':') 
                             then (do
                              (i, r2) <- startParse (drop (lvl + 2) r1) (lvl + 2)
                              return (i, r2))
                             else (do
                              (i, r2) <- startParse (drop (lvl + 2) r1) lvl
                              return (i, r2))))
                            
                    else ( 
                        if getSpaces (drop (getNewLines r1 0) r1) 0 < lvl
                        then (
                            Left "List closes")
                        else ( do
                        (i, r2) <- startParse r1 lvl
                        return (i, r2)))

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Right (' ', "")
parseChar ch (x:xs) | ch == x = Right (x, xs)
                    | otherwise = Left $ ch :" expected"

parseBaseType :: String -> Either String (Document, String)
parseBaseType str | (getSpaces str 0) == (length str) = Right (DString str, "")
                  | head (words str) == "''" = Right (DString "", drop 2 str)
                  | take 2 str == "{}" = Right (DMap [], drop 2 str)
                  | take 2 str == "[]" = Right (DList [], drop 2 str)
parseBaseType str = do
    let firstElem = head (words str)
    case firstElem of
        "-" -> parseBaseType (drop 2 str)
        "null" -> Right (DNull, drop 4 str)
        _ -> case readMaybeInt firstElem of
            Just a -> case readMaybeInt (head (lines str)) of
                        Just a -> case isDigit (last (head (lines str))) of
                                    True -> case isDigit (head (head (lines str))) of
                                              True -> (parseDInteger str)
                                              False -> case (head (head (lines str))) of
                                                        '-' -> parseDInteger str
                                                        _   -> parseDString str
                                    False -> parseDString str
                        Nothing -> parseDString str
            Nothing -> parseDString str

parseDInteger :: String -> Either String (Document, String)
parseDInteger str = (\(i, r) -> (DInteger i, r)) <$> parseInteger str

parseInteger :: String -> Either String (Int, String)
parseInteger ('-':xs) = do
    let prefix = takeWhile isDigit xs
    case prefix of
      [] -> Left "Error: Empty integer"
      _ -> Right (read ('-':prefix), drop (length prefix + 1) ('-':xs))
parseInteger str = do
    let prefix = takeWhile isDigit str
    case prefix of
      [] -> Left "Error: Empty integer"
      _ -> Right (read prefix, drop (length prefix) str)

parseDString :: String -> Either String (Document, String)
parseDString str = (\(i, r) -> (DString i, r)) <$> parseString str

parseString :: String -> Either String (String, String)
parseString [] = return ("", "")
parseString str = do
    let prefix = takeWhile (/='\n') str
    case charToString (head prefix) of 
      "'" -> case charToString (last prefix) of
          "'" -> do
                  let prefix = takeWhile (/='\n') str
                  return ((dropFirstAndLast prefix), drop (length prefix) str)
          _   -> do
                  let prefix = takeWhile (/='\n') str
                  return (prefix, drop (length prefix) str)
      _ -> do
        return (prefix, drop (length prefix) str)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

getSpaces :: String -> Int -> Int
getSpaces [] spaces = spaces
getSpaces (h:t) spaces
    | h == ' ' = getSpaces t (spaces + 1)
    | otherwise = spaces

getNewLines :: String -> Int -> Int
getNewLines [] newLines = newLines
getNewLines (h:t) newLines
    | h == '\n' = getNewLines t (newLines + 1)
    | otherwise = newLines

addToListEnd :: a -> [a] -> [a]
addToListEnd a xs = xs ++ [a]

charToString :: Char -> String
charToString c = [c]

dropFirstAndLast :: String -> String
dropFirstAndLast str = drop 1 (take ((length str)-1) str)

countDashes:: String -> Int -> Int
countDashes [] dashes = dashes
countDashes str dashes
    | take 2 str == "- " = countDashes (drop 2 str) (dashes + 1)
    | take 2 str == "  " = countDashes (drop 2 str) dashes
    | otherwise = dashes 

remove'' :: String -> String 
remove'' [] = []
remove'' str = 
  case charToString (head str) of
    "'" -> case charToString (last str) of
             "'" -> dropFirstAndLast str
             _   -> str              
    _   -> str

convertString :: String -> String -> Int -> Int -> String
convertString [] str2 lvl spaces = 
  case take 4 str2 of
    "---\n" -> str2
    _       -> "---\n" ++ str2
convertString (h:t) str2 lvl spaces = do
  case take 4 (h:t) of
    "---\n" -> convertString (drop 3 t) str2 lvl spaces
    _ ->
      case h of 
      '-' -> case take 2 t of 
             " -" -> case take 3 t of
                     " - " -> case lvl > 0 of
                              False -> convertString (drop 1 t) (str2 ++ (getTabs lvl) ++ "-\n") (lvl + 1) spaces
                              True -> convertString (drop 1 t) (str2 ++ (getTabs (spaces `div` 2)) ++ (getTabs lvl) ++ "-\n") (lvl + 1) spaces 
                     _ -> case lvl > 0 of
                          True -> convertString t (str2 ++ (getTabs (spaces `div` 2)) ++ (addToListEnd h (getTabs lvl))) 0 spaces
                          False -> convertString t (str2 ++ (addToListEnd h (getTabs lvl))) 0 spaces
             _    -> case (head t) of
                     '\n' -> convertString t (addToListEnd h str2) 0 0
                     _ -> case isDigit (head t) of
                          False -> case lvl > 0 of
                                   True ->  convertString (drop 1 t) (str2 ++ (getTabs lvl) ++ (getTabs (spaces `div` 2)) ++ "- ") 0 0
                                   False ->  convertString (drop 1 t) (str2 ++ (getTabs lvl) ++ "- ") 0 0
                          True -> convertString t (addToListEnd h str2) 0 0    
      _   -> case h of
             '\n' -> convertString t (addToListEnd h str2) 0 (getSpaces t 0)
             ' ' -> convertString t (addToListEnd h str2) lvl spaces
             _   -> convertString t (addToListEnd h str2) 0 0
   
 
-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart State
  deriving (Show, Eq)

instance FromDocument GameStart where
  fromDocument doc =case makeCords doc ([0],[0]) of
    Left str -> Left str
    Right cords -> case makeTuple cords 'x' [] of
        Left str -> Left str
        Right tuple -> Right (GameStart (Cord (tuple)))


-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
--gameStart :: State -> GameStart -> State
--gameStart (State l) d = State $ ("Game started: " ++ show d) : l
gameStart :: State -> GameStart -> State
gameStart l (GameStart state) =state


makeCords :: Document -> ([Int],[Int]) -> Either String ([Int],[Int])
makeCords (DMap ((str,doc):t)) (col,row) =
    case str of
        "number_of_hints" -> (makeCords  (DMap t)  (col,row))
        "occupied_cols"-> do
            cols <- makeList doc []
            Right (cols,row)
        "occupied_rows"-> do
            rows <- makeList doc []
            makeCords  (DMap t) (col,rows)
        "game_setup_id"->  (makeCords  (DMap t)  (col,row))
        _ -> Left str
makeCords _ _ = Left "Error: Wrong parameters (makeCords)"

makeList  :: Document ->[Int]-> Either String [Int]
makeList (DList [DInteger last]) list  = Right (last:list)
makeList (DList (h:t)) list = do
    int <- parser_DInteger h
    makeList (DList (t)) (int:list)
makeList _ _ = Left "Error: Wrong parameters (makeList)"

makeTuple  :: ([Int],[Int])->Char->[(Int,Int,Char)]-> Either String [(Int,Int,Char)]
makeTuple ([],[]) char tuple = Right(tuple)
makeTuple ((hC:tC),(hR:tR)) char tuple = makeTuple (tC,tR) char ((hC,hR,char):tuple)
makeTuple kazkas _ _ = Left (show kazkas)


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint State
  deriving (Show, Eq)

instance FromDocument Hint where
    -- fromDocument :: Document -> Either String Hint
    fromDocument doc = case hint_ (emptyState) doc of
        Left str -> Left str
        Right state -> Right ( Hint state)
    fromDocument _ = Left "Hint data invalid."

hint :: State -> Hint -> State
hint (Cord list) (Hint (Cord doc)) = Cord (doc++list)
    

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
--hint :: State -> Hint -> State
--hint (State l) h = State $ ("Hint " ++ show h) : l
hint_ :: State -> Document -> Either String State
hint_ l h = (hintf h l (0,0,'x'))
hint_ _ _ =  Left "Error: Wrong parameters"

hintf :: Document -> State ->(Int,Int,Char)-> Either String State
hintf (DNull) (Cord state2) (x,y,c) = Right (Cord state2)
hintf (DMap ((str,doc):t)) (Cord state2) (x,y,c) =
    case str of
        "coords"-> hintf doc (Cord state2) (x,y,c)
        "tail"-> hintf doc (Cord state2) (x,y,c)
        "head"->
            case hintf doc (Cord state2) (x,y,c) of
                Left _ -> Left "Error: Wrong second parameter"
                Right state -> hintf (DMap t) (state) (x,y,c)
        "col"-> do
            case (parser_DInteger doc) of
                Left _ -> Left "Error: Wrong first parameter"
                Right int -> hintf (DMap t) (Cord state2) (int ,y,'o')
        "row"->
            case (parser_DInteger doc) of
                Left _ -> Left "Error: Wrong first parameter"
                Right int -> Right (Cord ((x,int,c): state2))
        _ -> Left "Error: Wrong string"
hintf _ _ _ = Left "Error: Wrong parameters"


parser_DInteger :: Document -> Either String Int
parser_DInteger (DInteger a) = Right a
parser_DInteger _ = Left "Error: It's not DInteger"