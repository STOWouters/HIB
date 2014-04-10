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
 - Last modified: 10 April 2014.
 - By: Stijn Wouters.
 -}
module Battleship where

import Assets
import System.IO (hFlush, stdout)   -- fixes IO buffering issue

-- Check whether the point is a hit or a miss (or none of them)
target      ::  Point -> Board -> Target
target p b  |   elem p $ getHits b      = Hit
            |   elem p $ getMisses b    = Miss
            |   otherwise               = Unknown

-- Get the corresponding symbol (for outputting the board)
symbol      ::  Point -> Board -> Char
symbol p b  =   case target p b of
                    Hit         -> 'x'  -- A hit!
                    Miss        -> 'o'  -- A miss!
                    Unknown     -> '-'  -- not tried that point

-- Display the whole board
display         ::  Board -> IO()
display board   =   io_exec . concat $ [[putChar $ symbol (x,y) board | x <- [0..width-1]] ++ [putChar '\n'] | y <- [0..height-1]]
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

                -- First, initialize the players and its fleet

                -- parse name of player 1
                putStr "1: Enter yer name > ";
                hFlush stdout;
                name <- getLine;
                putStrLn $ "Ahoy cap'tain " ++ name ++ "!";
                hFlush stdout;

                -- parse fleet of player 1
                putStr $ name ++ ": Enter yer ship of length 2 > ";
                hFlush stdout;
                line <- getLine;
                let ship2 = [];
                putStr $ name ++ ": Enter yer ship of length 3 > ";
                hFlush stdout;
                line <- getLine;
                let ship3 = [];
                putStr $ name ++ ": Enter yer ship of length 4 > ";
                hFlush stdout;
                line <- getLine;
                let ship4 = [];
                putStr $ name ++ ": Enter yer ship of length 5 > ";
                hFlush stdout;
                line <- getLine;
                let ship5 = [];

                let fleet = [ship2, ship3, ship4, ship5];

                -- generate player 1
                let player1 = (name, initial_board, fleet);

                -- parse name of player 2
                putStr "2: Enter yer name > ";
                hFlush stdout;
                name <- getLine;
                putStrLn $ "Ahoy cap'tain " ++ name ++ "!";
                hFlush stdout;

                -- parse fleet of player 1
                putStr $ name ++ ": Enter yer ship of length 2 > ";
                hFlush stdout;
                line <- getLine;
                let ship2 = [];
                putStr $ name ++ ": Enter yer ship of length 3 > ";
                hFlush stdout;
                line <- getLine;
                let ship3 = [];
                putStr $ name ++ ": Enter yer ship of length 4 > ";
                hFlush stdout;
                line <- getLine;
                let ship4 = [];
                putStr $ name ++ ": Enter yer ship of length 5 > ";
                hFlush stdout;
                line <- getLine;
                let ship5 = [];

                let fleet = [ship2, ship3, ship4, ship5];

                -- generate player 2
                let player2 = (name, initial_board, fleet);

                -- now we have all the players
                let players = (player1, player2);

                -- start gameloop
                gameloop players;
                return ()
