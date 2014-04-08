{--
 - Assets.hs
 - Some global assets for the Battleships game.
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
module Assets where

-- Point representation
type Point  =   (Integer, Integer)

-- Execute a sequence of IO operations
io_exec         ::  [IO a] -> IO()
io_exec []      =   return ()
io_exec (x:xs)  =   do
                        x;
                        io_exec xs;
