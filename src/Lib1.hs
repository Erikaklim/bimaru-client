{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Lib1(
    State(..), emptyState, gameStart, render, mkCheck, toggle, hint
) where

import Types

-- This is a state of your game.
-- It must contain all values you might need during a game:
-- number of occupied rows/cols, hints, occupied cells,..
-- You can change the right hand side as you wish but please
-- keep the type name as is
data State =
  Cord [(Int,Int,Char)]
  | Test [String]
  deriving (Show, Eq)
-- IMPLEMENT
-- This is very initial state of your program
emptyState :: State
emptyState  = (Cord [])

-- IMPLEMENT
-- This adds game data to initial state 
gameStart :: State -> Document -> State
gameStart (Cord state) doc =
    let cords = makeCords doc ([0],[0]) 
        tuple = makeTuple_ (cords) 0 []
    in Cord (tuple++state)
makeCords :: Document -> ([Int],[Int]) -> ([Int],[Int])
makeCords (DMap ((str,doc):t)) (col,row) =
    case str of
        "number_of_hints"-> makeCords  (DMap t) (col,row)
        "occupied_cols"-> makeCords  (DMap t) ((makeList doc []),row)
        "occupied_rows"-> (col,(makeList doc []))
        _->  error (show str)

makeList  :: Document ->[Int]->[Int]
makeList (DList [DInteger last]) list= reverse (last:list)
makeList (DList (h:t)) list = makeList (DList (t)) ((parser_DInteger h):list)
makeList dox a = error (show "Bad parrametres")

makeTuple  :: ([Int],[Int])->Char->[(Int,Int,Char)]->[(Int,Int,Char)]
makeTuple ((hC:tC),(hR:tR)) char tuple = makeTuple (tC,tR) char ((hC,hR,char):tuple)
makeTuple ([],[]) char tuple = tuple
makeTuple e1 e2 e3 = error "Bad parametres"

makeTuple_  :: ([Int],[Int])->Int->[(Int,Int,Char)]->[(Int,Int,Char)]
makeTuple_ ((hC:tC),(hR:tR)) int tuple = makeTuple_ (tC,tR) (int+1) ((-1,int,(intToChar (hR))):(int,10,(intToChar (hC))):tuple)
makeTuple_ ([],[]) int tuple = tuple
makeTuple_ e1 e2 e3 = error "Bad parametres"

intToChar :: Int -> Char
intToChar x 
    | x == 0 = '0'
    | x == 1 = '1'
    | x == 2 = '2'
    | x == 3 = '3'
    | x == 4 = '4'
    | x == 5 = '5'
    | x == 6 = '6'
    | x == 7 = '7'
    | x == 8 = '8'
    | x == 9 = '9'
    | x > 9 = error ("Intas netelpa i chara")  -- (intToChar (x `div` 10)  ++ intToChar (mod x 10 ) )

render :: State->String
render (Cord st)= makeSpace (render_ (sort st []) (-1,0,10,9) "") "" ++ show(st)


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
toggle (Cord st) t =
  let new_ST= remove_elements_byChar st [] '@'
  in toggle_ (Cord st) t

toggle_ :: State -> [String]->State
toggle_ (Cord st) (hh:ht:tt) = 
    let colInt = takeInt hh
        rowInt = takeInt ht
        char = findElement_ st (colInt,rowInt,'x') (colInt,rowInt,'@') (colInt,rowInt,'!')
        newList = remove_element_byCord st [] (colInt,rowInt) 
    in case char of
      '_'->toggle_ (Cord (st)) tt
      '='->toggle_ (Cord (newList)) tt
      _->toggle_ (Cord ((colInt,rowInt,char):newList)) tt
toggle_ a b= a


findElement_ :: [(Int, Int, Char)]->(Int, Int, Char)->(Int, Int, Char)->(Int, Int, Char)->Char
findElement_ (h:t) looking looking2 looking3
    | looking3==h =  '_'-- jei sauktukas removint nereikia
    | looking2==h =  '='
    | looking==h =  '!'
    | otherwise = findElement_ t looking looking2 looking3
findElement_ end _ _ _='@'


takeInt :: String -> Int
takeInt str = read str :: Int
-- IMPLEMENT
-- Adds hint data to the game state
hint :: State -> Document -> State
hint l h = hintf h l (0,0,'x')

hintf :: Document -> State ->(Int,Int,Char)-> State
hintf (DNull) (Cord state2) (x,y,c)= (Cord state2)
hintf (DMap ((str,doc):t)) (Cord state2) (x,y,c)=
    case str of
        "coords"-> hintf doc (Cord state2) (x,y,c)
        "tail"-> hintf doc (Cord state2) (x,y,c)
        "head"-> hintf (DMap t) (hintf doc (Cord state2) (x,y,c)) (x,y,c)
        "col"-> hintf (DMap t) (Cord state2) (x,(parser_DInteger doc),'o')
        "row"->  Cord (((parser_DInteger doc),y,c): state2)

parser_DInteger :: Document -> Int
parser_DInteger (DInteger a) = a
parser_DInteger a = error "its not DInteger"


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
remove_element [] newList value = newList

remove_elements_byChar:: [(Int, Int, Char)]-> [(Int, Int, Char)]-> Char-> [(Int, Int, Char)]
remove_elements_byChar [] newList value = newList
remove_elements_byChar ((col,row,char_):t) newList value 
    | char_==value = remove_elements_byChar t (newList) value
    | otherwise = remove_elements_byChar t ((col,row,char_):newList) value

remove_element_byCord:: [(Int, Int, Char)]-> [(Int, Int, Char)]-> (Int, Int)-> [(Int, Int, Char)]
remove_element_byCord ((col,row,char_):t) newList (c,r) 
    | c==col && r==row = remove_element_byCord t (newList) (c,r)
    | otherwise = remove_element_byCord t ((col,row,char_):newList) (c,r)
remove_element_byCord [] newList value = newList

render_ :: [(Int,Int,Char)]-> (Int,Int,Int,Int)->String->String
render_ ((objx,objy,objc):t) (x,y,width,hight) rez
    | y>width = render_ ((objx,objy,objc):t) (x+1,0,width,hight) ('|':' ':rez)
    | x>hight = error (show rez)
    | objx == x && objy == y && objc == 'x' =  render_ t (x,y+1,width,hight) ('=':' ':rez)
    | objx == x && objy == y =  render_ t (x,y+1,width,hight) (objc:' ':rez) 
    | objy > y || objx > x   = render_ ((objx,objy,objc):t) (x,y+1,width,hight) ('=':' ':rez)
    | otherwise = render_ t (x,y,width,hight) rez
render_ [] (x,y,width,hight) rez 
    | y>width = render_ [] (x+1,0,width,hight) ('|':' ':rez)
    | x>hight = rez
    | otherwise =render_ [] (x,y+1,width,hight) ('=':' ':rez) 
    
makeSpace ::  String -> String -> String
makeSpace (h : t) rez =
  case h of
    '|' -> makeSpace t ('\n' :'\n' : rez)
    _ -> makeSpace t (h : rez)
makeSpace "" rez = rez