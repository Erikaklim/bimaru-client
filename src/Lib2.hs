{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# HLINT ignore "Use if" #-}
module Lib2(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint, renderDocument
) where

import System.IO
import Text.Read
import Types
    ( Document(DInteger, DString, DList, DNull, DMap), Coord(..), Check(..), ToDocument(..))

import Data.Char
import Lib1(State(..))
instance ToDocument Check where
  toDocument (Check c) = DMap [("coords", DList $ map toDocument c)]

instance ToDocument Coord where
    toDocument (Coord c r) = DMap [("col", DInteger c), ("row", DInteger r)] 


-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
-- IMPLEMENT

-- This is very initial state of your program
emptyState :: State
emptyState  = (Cord [])

-- IMPLEMENT
-- This adds game data to initial state 

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

render :: State->String
render (Cord st)= makeSpace (render_ (sort st []) (0,0,10,10) "") ""

-- IMPLEMENT
-- Make check from current state
mkCheck :: State -> Check
mkCheck (Cord st) = 
    let miss = findElements_byChar st '@' []
        good = findElements_byChar st '!' []
        coord = fromListToCoord (miss++good) []
    in ( Check {coords = coord})

fromListToCoord :: [(Int, Int, Char)] -> [Coord] ->[Coord]
fromListToCoord ((x,y,c):t) coords = fromListToCoord t ((Coord  {col = x, row = y}): coords)
fromListToCoord end rez = rez

findElements_byChar :: [(Int, Int, Char)]->(Char)->[(Int, Int, Char)]->[(Int, Int, Char)]
findElements_byChar ((c,r,char):t) looking rez
    | char==looking =findElements_byChar t  looking ((c,r,char):rez)
    | otherwise = findElements_byChar t  looking (rez)
findElements_byChar end looking rez = rez

-- IMPLEMENT
-- Toggle state's value
-- Receive raw user input tokens
toggle :: State -> [String] -> State
toggle l t = toggle_ l t

toggle_ :: State -> [String]->State
toggle_ (Cord st) (hh:ht:tt) = 
    let colInt = takeInt hh
        rowInt = takeInt ht
        char = findElement st (colInt,rowInt,'x')
    in toggle_ (Cord ((colInt,rowInt,char):st)) tt
toggle_ a b= a

findElement :: [(Int, Int, Char)]->(Int, Int, Char)->Char
findElement (h:t) looking 
    | looking==h =  '!'
    | otherwise = findElement t looking
findElement end max ='@'

takeInt :: String -> Int
takeInt str = read str :: Int

-- IMPLEMENT
-- Adds hint data to the game state
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
parser_DInteger a = Left "Error: It's not DInteger" 


sortList :: [(Int, Int, Char)] -> [(Int, Int, Char)]
sortList (z:zs) = if (head zs) < z then (z:zs) else (z:(sortList zs))

findMax :: [(Int, Int, Char)]->(Int, Int, Char)->(Int, Int, Char)
findMax (h:t) max 
    | h>max =  findMax t h
    | otherwise = findMax t max
findMax end max =max

sort :: [(Int, Int, Char)]->[(Int, Int, Char)]->[(Int, Int, Char)]
sort (h:t) newList =
    let max = findMax (h:t) h
        removedList= remove_element (h:t) [] max
    in sort removedList (max:newList) 
sort [] newList = newList

remove_element:: [(Int, Int, Char)]-> [(Int, Int, Char)]-> (Int, Int, Char)-> [(Int, Int, Char)]
remove_element (h:t) newList value 
    | h==value = newList++t
    | otherwise = remove_element t (h:newList) value


render_ :: [(Int,Int,Char)]-> (Int,Int,Int,Int)->String->String
render_ ((objx,objy,objc):t) (x,y,width,hight) rez
    | y>width = render_ ((objx,objy,objc):t) (x+1,0,width,hight) ('|':rez)
    | x>hight = error (show rez)
    | objx == x && objy == y && objc == 'x' =  render_ t (x,y+1,width,hight) ('=':rez)
    | objx == x && objy == y =  render_ t (x,y+1,width,hight) (objc:rez) 
    | objy > y || objx > x   = render_ ((objx,objy,objc):t) (x,y+1,width,hight) ('=':rez)
    | otherwise = render_ t (x,y,width,hight) rez
render_ [] (x,y,width,hight) rez 
    | y>width = render_ [] (x+1,0,width,hight) ('|':rez)
    | x>hight = rez
    | otherwise =render_ [] (x,y+1,width,hight) ('=':rez) 
    
makeSpace ::  String -> String -> String
makeSpace (h : t) rez =
  case h of
    '|' -> makeSpace t ('\n' : rez)
    _ -> makeSpace t (h : rez)
makeSpace "" rez = rez



renderDocument :: Document -> String
renderDocument d = "---\n" ++ renderDocumentf d

renderDocumentf :: Document -> String
renderDocumentf (DMap []) = "{}"
renderDocumentf (DList []) = "[]"
renderDocumentf (DMap a) = (renderDMap (DMap a) 0)
renderDocumentf (DList a) = (renderDList (DList a) 0)
renderDocumentf doc = baseTypes doc 

baseTypes :: Document -> String
baseTypes doc = 
    case doc of
    DNull -> "null"
    DInteger a -> show a
    DString [] -> "''"
    DString str -> 
      case readMaybeInt str of
        Just a -> case (getSpaces str 0) == 0 of
                    True -> case (getSpaces (reverse str) 0) == 0 of 
                              True -> "'" ++ str ++ "'"
                              False -> "'" ++ str ++ "'"
                    False -> "'" ++ str ++ "'"
        Nothing -> case head str of
                  ' ' -> "'" ++ str ++ "'"
                  _   -> case last str of
                      ' ' -> "'" ++ str ++ "'"
                      _   -> str

getTabs :: Int -> String
getTabs n 
    | n < 1 = ""
    | otherwise = "  " ++ getTabs (n-1)

renderDMap :: Document -> Int -> String
renderDMap (DMap []) _ = ""
renderDMap (DMap ((str, (DMap [])):t)) n = (getTabs n ++ str ++ ": {}\n") ++ renderDMap (DMap t) n 
renderDMap (DMap ((str, (DList [])):t)) n = (getTabs n ++ str ++ ": []\n") ++ renderDMap (DMap t) n 
renderDMap (DMap ((str, (DList h)):t)) n = (getTabs n ++ str ++ ":\n") ++ (renderDList (DList h) (n+1)) ++ renderDMap (DMap t) n 
renderDMap (DMap ((str, (DMap h)):t)) n = (getTabs n ++ str ++ ":\n") ++ (renderDMap (DMap h) (n+1)) ++ renderDMap (DMap t) n
renderDMap (DMap ((str, doc):t)) n = (getTabs n ++ str ++ ": " ++ baseTypes doc ++ "\n") ++ renderDMap (DMap t) n

renderDList :: Document -> Int -> String
renderDList (DList []) _ = ""
renderDList (DList ((DList []): t)) n = (getTabs n ++ "- []\n") ++ renderDList (DList t) n 
renderDList (DList ((DMap []): t)) n = (getTabs n ++ "- {}\n") ++ renderDList (DList t) n 
renderDList (DList ((DList h): t)) n = (getTabs n ++ "-\n") ++ (renderDList (DList h) (n+1)) ++ renderDList (DList t) n 
renderDList (DList ((DMap h): t)) n = (getTabs n ++ "-\n") ++ (renderDMap (DMap h) (n+1)) ++ renderDList (DList t) n
renderDList (DList (h:t)) n = (getTabs n ++ "- " ++ baseTypes h ++ "\n") ++ renderDList (DList t) n

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe

getSpaces :: String -> Int -> Int
getSpaces [] spaces = spaces
getSpaces (h:t) spaces
    | h == ' ' = getSpaces t (spaces + 1)
    | otherwise = spaces