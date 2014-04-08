{--
 - Battleship.hs
 - Interactive Battleship game written in Haskell
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
 - Last modified: 08 April 2014.
 - By: Stijn Wouters.
 -}
module Battleship where

import Assets

-- The width of the board
width   ::  Integer
width   =   10

-- The height of the board
height  ::  Integer
height  =   10

-- List of all hits and misses
type Hits   =   [Point]
type Misses =   [Point]

-- The player's board, consisting of a set of hits and misses
type Board  =   (Hits, Misses)

-- Check whether the point is a hit or a miss (or none of them)
hit         ::  Point -> Hits -> Bool
hit p hits  =   elem p hits

miss        ::  Point -> Misses -> Bool
miss p miss =   elem p miss

-- Get the corresponding symbol (for outputting the board)
symbol                  ::  Point -> Board -> Char
symbol p board          |   (hit p . fst) board   = 'x'   -- a hit!
                        |   (miss p . snd) board  = 'o'   -- a miss!
                        |   otherwise             = '-'   -- not tried

-- Display the whole board
display         ::  Board -> IO()
display board   =   (io_exec . concat) [[putChar (symbol (x,y) board) | x <- [0..width-1]] ++ [putChar '\n'] | y <- [0..height-1]]
{-
 - This one above looks hard to understand, isn't it? So, let's break this
 - beautiful one down for you:
 -
 - (1)  Use list comprehension for printing one row (on index y - just
 -      suppose the y is already given)
 -
 -      `[putChar (symbol (x,y) board) | x <- [0..width-1]]`
 -
 - (2)  Put a new line after the row (so, use concatenation operator after (1))
 -
 -      `(1) ++ [putChar '\n']`
 -
 - (3)  Repeat (1) and (2) for all y coordinates (so use a list comprehension)
 -
 -      `[(2) | y <- [0..height-1]]`
 -
 - (4)  Since we've just generated a list of list of IO operations [ [IO a] ],
 -      we must concatenate them so you can use the io_exec (which needs an
 -      argument of type [IO a] and not [ [IO a] ])
 -
 -      `(io_exec . concat) (3)`
 -}
