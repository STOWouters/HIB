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
import qualified Parser

-- Check whether the point is a hit or a miss (or none of them)
target      ::  Point -> Board -> Target
target p b  |   elem p $ getHits b      = Hit
            |   elem p $ getMisses b    = Miss
            |   otherwise               = Unknown

-- Get the orientation of a ship
orientation     ::  Ship -> Orientation
orientation s   |   null s                                  = None
                |   (fst . head $ s) == (fst . last $ s)    = Vertical
                |   (snd . head $ s) == (snd . last $ s)    = Horizontal
                |   otherwise                               = None

-- Check the ship coordinates: either the x or y coordinate should be
-- incrementing or decrementing.
-- TODO

-- Get the corresponding symbol (for outputting the board)
symbol      ::  Point -> Board -> Char
symbol p b  =   case target p b of
                    Hit         -> 'x'  -- A hit!
                    Miss        -> 'o'  -- A miss!
                    Unknown     -> '-'  -- not tried that point

-- Shoot at a fleet
shoot       ::  Point -> Fleet -> Target
shoot p f   =   case elem p $ concat f of
                    True    -> Hit
                    False   -> Miss

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

-- Prompt for player name
prompt_name     ::  String -> IO String
prompt_name who =   do
                        putStr $ who ++ ": Enter yer name > ";
                        hFlush stdout;
                        name <- getLine;
                        putStrLn $ "Ahoy cap'tain " ++ name ++ "!";
                        hFlush stdout;
                        return name;

prompt_ship         ::  String -> Integer -> IO Ship
prompt_ship who n   =   do
                            putStr $ who ++ ": Enter yer ship of length " ++ (show n) ++ " > ";
                            hFlush stdout;
                            line <- getLine;
                            let result = Parser.parse (Parser.points n) line;

                            if null result then
                                do
                                    putStrLn "Arrgh, syntax error!";
                                    hFlush stdout;
                                    prompt_ship who n;  -- use tail recursion
                            else
                                do
                                    let ship = fst $ result!!0;
                                    putStrLn "Yo-ho-ho!";
                                    hFlush stdout;
                                    return ship;

-- Implements the game loop (where players can shoot to each others)
gameloop            ::  Players -> IO ()
gameloop players    =   do
                            return ();

-- Play the game
play    ::  IO ()
play    =   do
                let initial_board = ([],[]);

                -- parse name of players
                name1 <- prompt_name "1";
                name2 <- prompt_name "2";

                -- parse fleet of player 1
                ship2 <- prompt_ship name1 2;
                ship3 <- prompt_ship name1 3;
                ship4 <- prompt_ship name1 4;
                ship5 <- prompt_ship name1 5;

                let fleet1 = [ship2, ship3, ship4, ship5];

                -- parse fleet of player 2
                ship2 <- prompt_ship name2 2;
                ship3 <- prompt_ship name2 3;
                ship4 <- prompt_ship name2 4;
                ship5 <- prompt_ship name2 5;

                let fleet2 = [ship2, ship3, ship4, ship5];

                -- generate players
                let player1 = (name1, initial_board, fleet1);
                let player2 = (name2, initial_board, fleet2);

                let players = (player1, player2);

                -- start gameloop
                gameloop players;
                return ()
