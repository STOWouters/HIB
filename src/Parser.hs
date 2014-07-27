{-
 - Parser.hs
 - Functional parser for Battleship, based upon the description on Graham
 - Hutton's 'Programming in Haskell', Cambridge University Press, 2007.
 -
 - Copyright (C) 2014 Stijn Wouters
 -
 - This program is free software: you can redistribute it and/or modify
 - it under the terms of the GNU General Public License as published by
 - the Free Software Foundation, either version 3 of the License, or
 - (at your option) any later version.
 -
 - This program is distributed in the hope that it will be useful,
 - but WITHOUT ANY WARRANTY; without even the implied warranty of
 - MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 - GNU General Public License for more details.
 -
 - You should have received a copy of the GNU General Public License
 - along with this program.  If not, see <http://www.gnu.org/licenses/>.
 -
 - Last modified: 27 July 2014.
 - By: Stijn Wouters.
 -}
module Parser where

import qualified Point
import qualified Board
import qualified Ship
import Data.Char (isSpace, isDigit)

{-
 - The implementation of the parser is slightly different from the parser
 - described on Graham Hutton's book, since this won't work anymore on the
 - modern Haskell version. Graham Hutton has already published an updated
 - version of his Parser Library, which can be found at
 - http://www.cs.nott.ac.uk/~gmh/bib.html#pearl
 -
 - I've based upon this library to implement a parser specific for the
 - Battleship game.
 -}

-- The parser type
newtype Parser a    =   Parser ( String -> Either String (a,String) )

instance Monad Parser where
    return v    =   Parser $ \input -> Right (v,input)
    p >>= f     =   Parser $ \input -> case parse p input of
                                Left msg        -> Left msg
                                Right (v,out)   -> parse (f v) out

{-
 - > parse (return 1) "abc"
 - Right (1, "abc")
 -}
parse               ::  Parser a -> String -> Either String (a,String)
parse (Parser p)    =   p

{-
 - > parse (failure "Failed to parse.") "abc"
 - Left "Failed to parse."
 -}
failure     ::  String -> Parser a
failure msg =   Parser $ \_ -> Left msg

{-
 - > parse item "abc"
 - Right ('a', "bc")
 - > parse item ""
 - Left "Missing one or more items to parse"
 -}
item    ::  Parser Char
item    =   Parser $ \input -> case input of
                        ""      -> Left "Missing one or more items to parse"
                        (x:xs)  -> Right (x,xs)

{-
 - > parse (satisfy isDigit "is not a digit") "1abc"
 - Right ('1', "abc")
 - > parse (satisfy isDigit "is not a digit") "abc"
 - Left "Parse error on 'a': is not a digit"
 -}
satisfy             ::  (Char -> Bool) -> String -> Parser Char
satisfy check msg   =   do
                            c <- item
                            if check c
                                then return c
                                else failure $ concat ["Parse error on ", (show c), ": ", msg]

-- Choice operator (read +++ as 'or else').
(+++)   ::  Parser a -> Parser a -> Parser a
p +++ q =   Parser $ \input -> case parse p input of
                        Left _          -> parse q input    -- failed to parse p
                        Right result    -> Right result     -- p succeed

-- Check for zero, one, or more occurences satisfying a given condition ('star'
-- comes from the UNIX regex * operator, which has the same meaning).
star    ::  Parser a -> Parser [a]
star p  =   plus p +++ return []

-- Check for one or more occurences satisfying a given condition ('plus' comes
-- from the UNIX regex + operator, which has the same meaning).
plus    ::  Parser a -> Parser [a]
plus p  =   do
                x   <- p
                xs  <- star p
                return (x:xs)

-- Parse spaces, but don't do anything with that.
space   ::  Parser ()
space   =   do
                star $ satisfy isSpace "is not a space"
                return ()

-- Digit parser.
digit   ::  Parser Char
digit   =   satisfy isDigit "is not a digit"

-- x-coordinate parser.
x_point ::  Parser Int
x_point =   do
                xs <- plus digit
                let n = read xs
                if elem n x_range
                    then return n
                    else failure $ concat [(show n), " not in ", (show x_range)]
                        where
                            x_range = [0..Board.width-1]

-- y-coordinate parser.
y_point ::  Parser Int
y_point =   do
                xs <- plus digit
                let n = read xs
                if elem n y_range
                    then return n
                    else failure $ concat [(show n), " not in ", (show y_range)]
                        where
                            y_range = [0..Board.height-1]

-- Token parser, ignore space before and after the token.
token   ::  Parser a -> Parser a
token p =   do
                space   -- space before token, no matter if fail
                v <- p
                space   -- space after token, no matter if parse fail
                return v

{-
 - > parse (symbol 'a') "  a  b c"
 - Right ('a', "b c")
 - > parse (symbol 'a') "  b c "
 - Left "Parse error on 'b': expects symbol 'a'"
 -}
symbol      ::  Char -> Parser Char
symbol s    =   token $ satisfy (== s) $ "expects symbol " ++ (show s)

{-
 - > parse point "( 0 , 0 )"
 - Right ((0,0), "")
 - > parse point "(a,0)"
 - Left "Parse error on 'a': is not a digit."
 - > parse point "(0,1000)"
 - Left "1000 not in [0,1,2,3,4,5,6,7,8,9]" (assuming y_range = [0..9])
 - > parse point "(0,0"
 - Left "Missing one or more items to parse"
 -}
point   ::  Parser Point.Point
point   =   do
                symbol '('
                x <- x_point
                symbol ','
                y <- y_point
                symbol ')'
                return (x, y)

-- Points parser (with the amount of points as argument).
points      ::  Integer -> Parser [Point.Point]
points 0    =   return []
points 1    =   do
                    p   <- point    -- don't include a ';' on the end
                    ps  <- points 0
                    return $ p:ps
points n    =   do
                    p   <- point
                    symbol ';'
                    ps  <- points $ n-1
                    return $ p:ps

-- Parse ship with a certain length.
ship        ::  Integer -> Parser Ship.Ship
ship length =   do
                    ship <- points length
                    if Ship.isValidShip ship
                        then return ship
                        else failure $ (show ship) ++ " is not a valid ship"
