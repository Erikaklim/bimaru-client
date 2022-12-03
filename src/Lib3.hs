{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Use first" #-}
module Lib3(hint, gameStart, parseDocument, GameStart, Hint) where

import Text.Read
import Data.Char
import Types ( Document (DNull, DList, DInteger, DMap, DString) )
import Lib1 (State(..))
import Data.Aeson (Array)
import Data.Aeson.Encoding (list, string)
import GHC.Generics (D)
import Data.List (stripPrefix)
import Data.Vector.Internal.Check (check)

-- IMPLEMENT
-- Parses a document from yaml

parseDocument :: String -> Either String Document
parseDocument str = (fst) <$> (dropTitle str)
--parseDocument _ = Left "Given value is not a String"

dropTitle :: String -> Either String (Document, String)
dropTitle str = startParse (drop 4 str ) 0 

startParse :: String -> Int -> Either String (Document, String)
startParse str lvl =
    case take 2 str of
        "-\n" -> startParse (drop 2 str) (lvl - 1)
        _ -> dataCheck (parseComplexType str lvl) (parseBaseType str)

dataCheck :: Either String (a, String) -> Either String (a, String) -> Either String (a, String)
dataCheck parser1 parser2 =
    case parser1 of
        Right a -> Right a
        Left _ -> parser2

parseComplexType :: String -> Int -> Either String (Document, String)
parseComplexType str lvl = 
    case head str of
        ' ' -> parseComplexType (drop 2 str) (lvl + 1)
        _ ->  parseDList str lvl
  

parseDMap :: String -> Either String (Document, String)
parseDMap str = Left "Not implemented"

parseDList :: String -> Int -> Either String (Document, String)
parseDList str lvl = do
    (_, x1) <- parseChar '-' str
    case lvl of 
        1 -> Left "its the same list level"
        _ ->  do 
            (y, x2) <- optional (drop 1 x1) (lvl + 1) elems  
            (_, x3) <- parseChar '\n' x2
            case y of
                Just a  -> return (DList a, x3)
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
    case r1 of
      "\n" -> return ([i1], r1)
      _    -> do 
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
fromSecond str lvl = do
    (_, r1) <- parseChar '\n' str
    case r1 of
      "" -> Left "end"
      _ -> do
        (i, r2) <- startParse r1 lvl
        return (i, r2)

parseChar :: Char -> String -> Either String (Char, String)
parseChar ch [] = Left $ "Empty input: '" ++ [ch] ++ "' expected"
parseChar ch (x:xs) | ch == x = Right (x, xs)
                    | otherwise = Left $ ch :" expected"

parseBaseType :: String -> Either String (Document, String)
parseBaseType str = do
    let firstElem = head (words str)
    case firstElem of
        "-" -> parseBaseType (drop 2 str)
        "null" -> Right (DNull, drop 4 str)
        _ -> case readMaybeInt firstElem of
            Just a ->  (parseDInteger str)
            Nothing -> (parseDString str)

parseDInteger :: String -> Either String (Document, String)
parseDInteger str = (\(i, r) -> (DInteger i, r)) <$> parseInteger str

parseInteger :: String -> Either String (Int, String)
parseInteger str =
    let
        prefix = takeWhile isDigit str
    in
        case prefix of
            [] -> Left "Empty integer"
            _ -> Right (read prefix, drop (length prefix) str)

parseDString :: String -> Either String (Document, String)
parseDString str = (\(i, r) -> (DString i, r)) <$> parseString str

parseString :: String -> Either String (String, String)
parseString str = 
    let 
        prefix = takeWhile (/='\n') str
    in
        case prefix of 
            [] -> Left "Empty string"
            _  -> Right (prefix, drop (length prefix) str)

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

charToString :: Char -> String
charToString c = [c]

addToListEnd :: a -> [a] -> [a]
addToListEnd a xs = xs ++ [a]

addListToListEnd :: [a] -> [a] -> [a]
addListToListEnd a xs = xs ++ a

takeOneString :: String -> String -> (String, String)
takeOneString ('\n':t) list = (list, t)
takeOneString (h:t) list = takeOneString t (addToListEnd h list)

-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data GameStart = GameStart deriving Show

-- This adds game data to initial state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
--gameStart :: State -> GameStart -> State
--gameStart (State l) d = State $ ("Game started: " ++ show d) : l
gameStart :: State -> Document -> Either String State
gameStart (Cord state) doc = do
    cords <- makeCords doc ([0],[0])
    tuple <- makeTuple cords 'x' []
    Right(Cord (tuple++state))
gameStart _ _ = Left "Error: Wrong parameters (gameStart)"

makeCords :: Document -> ([Int],[Int]) -> Either String ([Int],[Int])
makeCords (DMap ((str,doc):t)) (col,row) =
    case str of
        "number_of_hints" -> (makeCords  (DMap t)  (col,row))
        "occupied_cols"-> do
            cols <- makeList doc []
            makeCords  (DMap t) (cols,row)
        "occupied_rows"-> do
            rows <- makeList doc []
            Right (col,rows)
        _ -> Left "Error: Wrong string"
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
makeTuple _ _ _ = Left "Error: Wrong parameters (makeTuple)"


-- IMPLEMENT
-- Change right hand side as you wish
-- You will have to create an instance of FromDocument
data Hint = Hint deriving Show

-- Adds hint data to the game state
-- Errors are not reported since GameStart is already totally valid adt
-- containing all fields needed
--hint :: State -> Hint -> State
--hint (State l) h = State $ ("Hint " ++ show h) : l
hint :: State -> Document -> Either String State
hint l h = (hintf h l (0,0,'x'))
hint _ _ =  Left "Error: Wrong parameters"

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
