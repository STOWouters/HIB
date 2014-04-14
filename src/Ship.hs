{-
 - Ship
 - Functions and declarations for ships in the Battleship game.
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
 - Last modified: 14 April 2014.
 - By: Stijn Wouters.
 -}
module Ship where

import qualified Point
import qualified Board

-- Ship represented as a list of points.
type Ship   =   [Point.Point]

-- Check whether a ship is valid
isValidShip_H           ::  Ship -> Bool
isValidShip_H []        =   True    -- empty ship
isValidShip_H [_]       =   True    -- ship with one coordinate
isValidShip_H (x:xs)    |   x Point.>+ head xs  = isValidShip_H xs
                        |   otherwise           = False

isValidShip_V           ::  Ship -> Bool
isValidShip_V []        =   True    -- empty ship
isValidShip_V [_]       =   True    -- ship with one coordinate
isValidShip_V (x:xs)    |   x Point.^+ head xs  = isValidShip_V xs
                        |   otherwise           = False

isValidShip             ::  Ship -> Bool
isValidShip []          =   True    -- empty ship
isValidShip [_]         =   True    -- ship with one coordinate
isValidShip s           |   (fst . head) s == (fst . last) s    = isValidShip_V s   -- guess it's a vertical ship
                        |   (snd . head) s == (snd . last) s    = isValidShip_H s   -- guess it's a horizontal ship
                        |   otherwise                           = False             -- no

-- Fleet represented as a list of ships.
type Fleet  =   [Ship]

-- Shoot at a fleet and see whether it's a hit or a miss
hit     ::  Point.Point -> Fleet -> Bool
hit p f =   elem p $ concat f

-- Eleminate a point from a fleet.
eleminate       ::  Point.Point -> Fleet -> Fleet
eleminate p f   =   [ [ point | point <- ship, p /= point ] | ship <- f]

-- Clean fleet from empty ships (i.e. the ship has been sunk).
clean   ::  Fleet -> Fleet
clean f =   [ ship | ship <- f, not $ null ship ]

-- Check on overlap with other ships in the fleet.
overlap     ::  Ship -> Fleet -> Bool
overlap s f =   any (==True) [elem p $ concat f | p <- s]

-- Display the fleet on a board.
display     ::  Fleet -> IO ()
display f   =   putStrLn $ concat [ (concat [ if elem (x,y) points then "% " else "~ " | x <- [0..Board.width-1] ]) ++ ['\n'] | y <- [0..Board.height-1] ]
                           where points = concat f
                -- WOW! Let's break this beautiful one down for better understanding:
                --
                --      (1) Iterate over each column (assuming the y is already
                --          given), now you have a row:
                --
                --          `(concat [ if elem (x,y) points then '%' else '~' | x <- [0..Board.width-1] ]) ++ ['\n']`
                --
                --      (2) Actually, y is not given, so let's modify that:
                --
                --          `[ (1) | y <- [0..Board.height-1] ]`
                --
                --      (3) But `putStrLn` doesn't work with a list in a list,
                --          so concatenate them so you'll get a list of Char's
                --          (thus String):
                --
                --          `putStrLn $ concat (2)`
                --
                -- Alright!
