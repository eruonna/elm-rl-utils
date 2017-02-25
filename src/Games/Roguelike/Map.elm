module Games.Roguelike.Map exposing
  ( Location
  , Map
  , get
  , set
  , update
  , fromList
  , map
  , foldl
  , union
  , merge
  )

{-| This module provides utilities for working with two-dimensional maps of
tiles.

# Aliases
@docs Location, Map

# Functions
@docs get, set, update, fromList, map, foldl, union, merge

-}

import Dict exposing (Dict)
import Maybe exposing (Maybe)

{-| Represents a location on the map. This is a pair rather than a record
because we use it as keys in a `Dict`.
-}
type alias Location = (Int, Int)

{-| A two-dimensional map with tiles of type `t`.
-}
type alias Map t = Dict Location t

{-| Get the tile at a location.
-}
get : Location -> Map t -> Maybe t
get = Dict.get

{-| Set the tile at a location.
-}
set : Location -> t -> Map t -> Map t
set = Dict.insert

{-| Update the tile at a location.
-}
update : Location -> (Maybe t -> Maybe t) -> Map t -> Map t
update = Dict.update

{-| Build a map from an association list.
-}
fromList : List (Location, t) -> Map t
fromList = Dict.fromList

{-| Apply a function to all tiles.
-}
map : (Location -> t -> u) -> Map t -> Map u
map = Dict.map

{-| Fold over tiles.
-}
foldl : (Location -> t -> r -> r) -> r -> Map t -> r
foldl = Dict.foldl

{-| Combine two maps, overwriting locations in the second with tiles from
the first.
-}
union : Map t -> Map t -> Map t
union = Dict.union

{-| Merge two maps.
-}
merge : (Location -> t -> r -> r)
     -> (Location -> t -> u -> r -> r)
     -> (Location -> u -> r -> r)
     -> Map t -> Map u
     -> r -> r
merge = Dict.merge
