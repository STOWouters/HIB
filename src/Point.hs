{-
 - Point
 - Functions and declarations for points.
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
 - Last modified: 12 April 2014.
 - By: Stijn Wouters.
 -}
module Point where

-- Point representation, Don't use `Integer` since some functions doesn't
-- support `Integer` argument while `Int` does.
type Point  =   (Int, Int)

-- Check whether a point is a precedor of the other point.
(>+)                ::  Point -> Point -> Bool
(x0,y0) >+ (x1,y1)  =   x1 == x0 + 1 && y1 == y0

(^+)                ::  Point -> Point -> Bool
(x0,y0) ^+ (x1,y1)  =   x1 == x0 && y1 == y0 + 1
