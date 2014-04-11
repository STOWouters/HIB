{-
 - Board
 - Functions and declarations for the board of Battleship.
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
 - Last modified: 11 April 2014.
 - By: Stijn Wouters.
 -}
module Board where

import qualified Point

-- Tells whether a target is a hit, miss, or unknown (has not been shot)
data Target =   Hit | Miss | Unknown
                deriving (Eq)

instance Show Target where
    show Hit        = "x"
    show Miss       = "o"
    show Unknown    = "-"

-- The width of the board. Don't use `Integer` since some functions doesn't
-- support `Integer` argument while `Int` does.
width   ::  Int
width   =   10

-- The height of the board. Don't use `Integer` since some functions doesn't
-- support `Integer` argument while `Int` does.
height  ::  Int
height  =   10

-- The player's board, so she/he can track down all her/his shots
type Board  =   [Target]

-- Display the board
display     ::  Board -> IO ()
display []  =   return ()
display b   =   do
                    let parts = splitAt width b
                    putStrLn $ concat $ map show $ fst parts
                    display $ snd parts
                    -- Split the board into two parts: the first row (so the
                    -- first `width` cells) and the remaining cells. Print the
                    -- first row and go recursively further with the rest (tail
                    -- recursion).

-- Update the board with a new target on the given coordinate.
mark        ::  Point.Point -> Target -> Board -> Board
mark p t b  =   concat [fst parts, [t], tail . snd $ parts]
                where
                    parts = splitAt ( (snd p) * width + fst p ) b
                -- Split the board into two parts: the first contains cells
                -- preceding the marked cell (whose value has to be updated),
                -- while the second contains the rest. I use matrix arithmetic
                -- as teached by prof. Vangheluwe to get the correct index to
                -- split the list (because I don't like nested lists for matrix
                -- representation, and this one is much eleganter and easier to
                -- handle).
