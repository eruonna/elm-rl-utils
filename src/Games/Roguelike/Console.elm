module Games.Roguelike.Console
  ( Location
  , Console
  , view
  , draw
  , fromMap ) where

import Dict exposing (Dict)
import Html exposing (Html)
import List
import Maybe

import Games.Roguelike.Tile exposing (Tile, Rect) as Tile

{-| Represents a location on the console. This is a pair rather than a record
because we use it as keys in a `Dict`.
-}
type alias Location = (Int, Int)

{-| Represents a console filled with tiles. The `rect` field is the rectangle
of `Location`s that are considered part of the console, but not all of those
need be present in `dict`.
-}
type alias Console =
  { dict : Dict Location Tile
  , rect : Rect Int
  }

{-| Create a new (empty) console.
-}
new : Rect Int -> Console
new rect = { d = Dict.empty, r = rect }

{-| Convert a `Console` to `Html`. Currently we use a `table` since it is,
arguably, tabular data.
-}
view : Console -> Html
view console =
  table [ tableStyle ]
    [ tbody []
        (List.map (\ r ->
          tr []
            (List.map (\ l -> td [] <| Maybe.withDefault []
                                         Maybe.map (\ t -> [ Tile.view t ])
                                           <| Dict.get l console.dict)
                      <| rectLocs console.rect))) ]

rectLocs : Rect Int -> List.List (Int, Int)
rectLocs { x, y, w, h } =
  List.map (\ a -> List.map (\ b -> (a, b)) [y..y+h-1]) [x..x+w-1]

{-| Draw a tile into a console. Does no bounds checking; tiles added
out-of-bounds will consume space but will not be shown with `view`.
-}
draw : Location -> Tile -> Console -> Console
draw location tile console =
  { console | dict = Dict.insert location tile console.dict }

{-| Build a console from a `Location`-keyed `Dict` using the given rectangle.
The `tileGetter` argument is a function that converts a map entry into a tile.
This ought to be more efficient than iterating over the map and `draw`ing each
tile individually.
-}
fromMap : Dict Location a -> (a -> Tile) -> Console
fromMap map tileGetter = Dict.map t m
