{-
 - Player
 - Functions and declarations for players.
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
module Player where

import qualified Board
import qualified Ship

-- The player data model.
type Player =   (String, Board.Board, Ship.Fleet)

-- Get specific data from the player (as an abstraction from the datamodel, to
-- make the design less vulnerable for changes in the data model).
getName             ::  Player -> String
getName (n,_,_)     =   n

getBoard            ::  Player -> Board.Board
getBoard (_,b,_)    =   b

getFleet            ::  Player -> Ship.Fleet
getFleet (_,_,f)    =   f

-- Update the datamodel of players.
updateBoard             ::  Player -> Board.Board -> Player
updateBoard (n,_,f) b   =   (n,b,f)

updateFleet             ::  Player -> Ship.Fleet -> Player
updateFleet (n,b,_) f   =   (n,b,f)
