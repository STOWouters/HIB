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
 - Last modified: 09 April 2014.
 - By: Stijn Wouters.
 -}
module Battleship where

import Assets
import System.IO (hFlush, stdout)   -- fixes IO buffering issue

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

-- Get specific data from the board (abstraction from the data model, makes the
-- design less vulnerable for changes in the data model).
getHits     ::  Board -> Hits
getHits b   =   fst b

getMisses   ::  Board -> Misses
getMisses b =   snd b

-- The player data model
type Player =   (String, Board)

-- Get specific data from the player (another abstraction from the data model,
-- makes the design less vulnerable for changes in the data model).
getName     ::  Player -> String
getName p   =   fst p

getBoard    ::  Player -> Board
getBoard p  =   snd p

-- All the participants of the Battleship game
type Players    =   (Player, Player)

{-
 - There's no need for abstraction from the Players type, since the Battleship
 - is always played with two players
 -}

-- Check whether the point is a hit or a miss (or none of them)
isHit           ::  Point -> Hits -> Bool
isHit p hits    =   elem p hits

isMiss          ::  Point -> Misses -> Bool
isMiss p miss   =   elem p miss

-- Get the corresponding symbol (for outputting the board)
symbol      ::  Point -> Board -> Char
symbol p b  |   isHit p $ getHits b     = 'x'   -- a hit!
            |   isMiss p $ getMisses b  = 'o'   -- a miss!
            |   otherwise               = '-'   -- not tried

-- Display the whole board
display         ::  Board -> IO()
display board   =   io_exec . concat $ [[putChar (symbol (x,y) board) | x <- [0..width-1]] ++ [putChar '\n'] | y <- [0..height-1]]
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
 -      `io_exec . concat $ (3)`
 -}

-- Implements the game loop (where players can shoot to each others)
gameloop            ::  Players -> IO ()
gameloop players    =   do
                            return ();

-- Play the game
play    ::  IO ()
play    =   do
                let initial_board = ([],[]);

                -- First, initialize the players
                putStr "1: Enter yer name > ";
                hFlush stdout;
                name1 <- getLine;
                let player1 = (name1, initial_board);
                putStrLn $ "Ahoy cap'tain " ++ name1 ++ "!";
                hFlush stdout;

                putStr "2: Enter yer name > ";
                hFlush stdout;
                name2 <- getLine;
                let player2 = (name2, initial_board);
                putStrLn $ "Ahoy cap'tain " ++ name2 ++ "!";
                hFlush stdout;

                let players = (player1, player2);

                -- start gameloop
                gameloop players;
                return ()
