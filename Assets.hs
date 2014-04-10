{--
 - Assets.hs
 - Some global assets for the Battleships game, this does NOT contains any
 - actual implementation of the HIB, but rather some helper functions and
 - data/types.
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
module Assets where

-- Point representation
type Point  =   (Integer, Integer)

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

-- The ship (represented by a list of points)
type Ship   =   [Point]

-- The fleet (represented by a list of ships)
type Fleet  =   [Ship]

-- The player data model
type Player =   (String, Board, Fleet)

-- Get specific data from the player (another abstraction from the data model,
-- makes the design less vulnerable for changes in the data model).
getName             ::  Player -> String
getName (n,_,_)     =   n

getBoard            ::  Player -> Board
getBoard (_,b,_)    =   b

getFleet            ::  Player -> Fleet
getFleet (_,_,f)    =   f

-- All the participants of the Battleship game
type Players    =   (Player, Player)

{-
 - There's no need for abstraction from the Players type, since the Battleship
 - is always played with two players
 -}

-- Possible values for a target
data Target =   Hit | Miss | Unknown
                deriving Eq

-- Execute a sequence of IO operations
io_exec         ::  [IO a] -> IO()
io_exec []      =   return ()
io_exec (x:xs)  =   do
                        x;
                        io_exec xs;
