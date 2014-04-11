{-
 - Battleship.hs
 - The interactive Battleship game.
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
module Battleship where

import qualified Point
import qualified Board
import qualified Ship
import qualified Player
import qualified Parser

-- Some IO operations doesn't appear in the correct order, this is actually an
-- IO buffering issue, importing this and execute `hFlush stdout` after every
-- putStr[Ln] should fix that.
import System.IO (hFlush, stdout)

-- Prompt for player name.
prompt_name         ::  String -> IO String
prompt_name who     =   do
                            putStr $ who ++ ": (name) > "
                            hFlush stdout
                            name <- getLine
                            return name

-- Prompt for ship coordinates, you have to pass how many points should be
-- parsed, and the fleet created so far. It will keep prompting untill the ship
-- has valid coordinates.
prompt_ship         ::  String -> Integer -> Ship.Fleet -> IO Ship.Ship
prompt_ship who n f =   do
                            putStr $ who ++ ": (ship [" ++ (show n) ++ "]) > "
                            hFlush stdout
                            line <- getLine
                            let result = Parser.parse (Parser.points n) line

                            if null result then do
                                -- failed to parse, try again
                                putStrLn "Syntax Error."
                                hFlush stdout
                                prompt_ship who n f
                            else do
                                let ship = fst $ result!!0

                                if Ship.overlap ship f then do
                                    putStrLn "Overlaps with the fleet so far."
                                    hFlush stdout
                                    prompt_ship who n f
                                else do
                                    return ship

-- Prompt for the coordinates.
prompt_point        ::  String -> IO Point.Point
prompt_point who    =   do
                            putStr $ who ++ ": (point) > "
                            hFlush stdout
                            line <- getLine
                            let result = Parser.parse Parser.point line

                            if null result then do
                                -- failed to parse the point, try again
                                putStrLn "Syntax Error."
                                hFlush stdout
                                prompt_point who
                            else do
                                let point = fst $ result!!0
                                return point

-- The shootloop, where the player can shoot.
shootloop           ::  (Player.Player, Player.Player) -> Int -> IO ()
shootloop (p1,p2) 0 =   do
                            let fleet = Player.getFleet p2

                            -- If the opponent has no fleet, then stop the
                            -- game and display both boards. Otherwise, go
                            -- further by just switching the players.
                            if null fleet then do
                                let name1 = Player.getName p1
                                let name2 = Player.getName p2

                                putStrLn $ name1 ++ " wins."
                                hFlush stdout

                                putStrLn $ "Board of " ++ name1 ++ ":"
                                hFlush stdout
                                Board.display $ Player.getBoard p1

                                putStrLn $ "Board of " ++ name2 ++ ":"
                                hFlush stdout
                                Board.display $ Player.getBoard p2
                                return ()
                            else do
                                shootloop (p2,p1) $ length fleet

shootloop (p1,p2) n =   do
                            let fleet = Player.getFleet p2  -- fleet of opponent

                            -- If the opponent has no fleet, then stop the game
                            -- and display both boards. Otherwise, start
                            -- shooting.
                            if null fleet then do
                                let name1 = Player.getName p1
                                let name2 = Player.getName p2

                                putStrLn $ name1 ++ " wins."
                                hFlush stdout

                                putStrLn $ "Board of " ++ name1 ++ ":"
                                hFlush stdout
                                Board.display $ Player.getBoard p1

                                putStrLn $ "Board of " ++ name2 ++ ":"
                                hFlush stdout
                                Board.display $ Player.getBoard p2
                                return ()
                            else do
                                let name = Player.getName p1    -- name of current player
                                let board = Player.getBoard p1  -- board of current player

                                -- Display current board and prompt for a
                                -- coordinate to shoot at.
                                Board.display board
                                point <- prompt_point name

                                if Ship.hit point fleet then do
                                    let new_board = Board.mark point Board.Hit board

                                    -- Just a hit, or sank a whole ship?
                                    let l_before = length fleet
                                    let new_fleet = Ship.clean . Ship.eleminate point $ fleet
                                    let l_after = length new_fleet

                                    if l_after < l_before then do
                                        putStrLn "A ship has been sank!"
                                        hFlush stdout
                                    else do
                                        putStrLn "Hit!"
                                        hFlush stdout

                                    -- update current board and opponents fleet
                                    let new_p1 = Player.updateBoard p1 new_board
                                    let new_p2 = Player.updateFleet p2 new_fleet
                                    shootloop (new_p1, new_p2) $ n-1
                                else do
                                    putStrLn "Miss!"
                                    hFlush stdout

                                    -- update current board
                                    let new_board = Board.mark point Board.Miss board
                                    let new_p1 = Player.updateBoard p1 new_board
                                    shootloop (new_p1, p2) $ n-1

-- Play the game.
play    ::  IO ()
play    =   do
                -- start with an empty board
                let board = replicate (Board.width * Board.height) Board.Unknown

                -- parse name of players
                name1 <- prompt_name "1"
                name2 <- prompt_name "2"

                -- parse fleet of player 1
                let fleet1 = []

                ship2 <- prompt_ship name1 2 fleet1
                let fleet1 = [ship2]
                ship3 <- prompt_ship name1 3 fleet1
                let fleet1 = [ship2, ship3]
                ship4 <- prompt_ship name1 4 fleet1
                let fleet1 = [ship2, ship3, ship4]
                ship5 <- prompt_ship name1 5 fleet1
                let fleet1 = [ship2, ship3, ship4, ship5]

                -- parse fleet of player 2
                let fleet2 = []

                ship2 <- prompt_ship name2 2 fleet2
                let fleet2 = [ship2]
                ship3 <- prompt_ship name2 3 fleet2
                let fleet2 = [ship2, ship3]
                ship4 <- prompt_ship name2 4 fleet2
                let fleet2 = [ship2, ship3, ship4]
                ship5 <- prompt_ship name2 5 fleet2
                let fleet2 = [ship2, ship3, ship4, ship5]

                -- generate players
                let player1 = (name1, board, fleet1)
                let player2 = (name2, board, fleet2)

                -- start shootloop
                shootloop (player1, player2) $ length fleet1
                return ()
