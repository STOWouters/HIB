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
 - Last modified: 11 April 2014.
 - By: Stijn Wouters.
 -}
module Ship where

import qualified Point
import qualified Board

-- Ship represented as a list of points.
type Ship   =   [Point.Point]

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

-- Check on overlap with other ships in the fleet
overlap     ::  Ship -> Fleet -> Bool
overlap s f =   any (==True) [elem p $ concat f | p <- s]