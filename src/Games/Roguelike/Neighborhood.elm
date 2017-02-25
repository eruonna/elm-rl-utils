module Games.Roguelike.Neighborhood exposing
  ( Neighborhood
  , get
  , localMap
  )

{-| A representation of two-dimensional maps centered relative to a particular
tile.

# Type
@docs Neighborhood

# Functions
@docs get, localMap

-}

import Games.Roguelike.Map as Map exposing (Map, Location)

{-| A `Neighborhood` is a two-dimensional map which is queried relative to a
''center'' location.
-}
type alias Neighborhood t = { map : Map t, center : Location }

{-| Get a value using a relative offset from the center.
-}
get : (Int, Int) -> Neighborhood t -> Maybe t
get (dx, dy) nbhd =
  let (x,y) = nbhd.center
  in Map.get (x+dx,y+dy) nbhd.map

{-| Update a map by applying a `Neighborhood` function at each tile.
-}
localMap : (Neighborhood t -> u) -> Map t -> Map u
localMap fn map = Map.map (\ l _ -> fn (Neighborhood map l)) map
