module Games.Roguelike.Console exposing
  ( Console
  , new
  , view
  , draw
  , fromMap
  )

{-| This module provides utilities for manipulating and viewing grids of
tiles.

# Types
@docs Console

# Create
@docs new, fromMap

# Update
@docs draw

# View
@docs view

-}

import Dict exposing (Dict)
import Html exposing (Html, table, tbody, tr, td)
import Html.Attributes exposing (style)
import List
import Maybe

import Games.Roguelike.Tile as Tile
import Games.Roguelike.Tile exposing (Tile, Rect)
import Games.Roguelike.Map as Map exposing (Map, Location)

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
new rect = { dict = Dict.empty, rect = rect }

{-| Convert a `Console` to `Html`. Currently we use a `table` since it is,
arguably, tabular data.
-}
view : Console -> Html a
view console =
  table [ tableStyle ]
    [ tbody [ tableStyle ]
        (List.map (\ r ->
          tr [ tableStyle ]
            (List.map (\ l -> td [ tableStyle ]
                                    <| Maybe.withDefault []
                                    <| Maybe.map (\ t -> [ Tile.view t ])
                                    <| Dict.get l console.dict)
                      r))
               <| rectLocs console.rect) ]

rectLocs : Rect Int -> List (List (Int, Int))
rectLocs { x, y, w, h } =
  List.map (\ b -> List.map (\ a -> (a, b)) (List.range x (x+w-1))) (List.range y (y+h-1))

tableStyle : Html.Attribute a
tableStyle =
  style [ ("padding", "0px")
        , ("margin", "0px")
        , ("border", "0px")
        , ("border-spacing", "0px")
        ]

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
fromMap : Rect Int -> Map a -> (a -> Tile) -> Console
fromMap rect map tileGetter =
  { dict = Dict.map (\ _ -> tileGetter) map
  , rect = rect
  }
