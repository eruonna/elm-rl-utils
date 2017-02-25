module Main exposing (main)

import Debug
import Dict exposing (Dict)
import Json.Decode as Json
import List
import Platform.Cmd as Cmd exposing (Cmd, (!))
import Platform.Sub as Sub exposing (Sub)
import Task exposing (Task)
import Dom
import Html exposing (Html, Attribute)
import Html.Attributes
import Html.Events
import Games.Roguelike.Console as Console exposing (Console)
import Games.Roguelike.Map as Map exposing (Map, Location)
import Games.Roguelike.Tile as Tile exposing (Rect, Tile)

consolas16x16 = Tile.tilesheet "/assets/consolas_unicode_16x16.png" 16 16
emptyTile = consolas16x16 0 0 Nothing
playerTile = consolas16x16 0 2 (Just "@")
floorTile = consolas16x16 14 1 (Just ".")
wallTile = consolas16x16 3 1 (Just "#")

type alias MapTile = { passable : Bool, tile : Tile }
floor = MapTile True floorTile
wall = MapTile False wallTile
rock = MapTile False emptyTile
type alias Player = Location

type alias Model = { map : Map MapTile, player : Player }

onEdge : Rect Int -> Location -> Bool
onEdge rect (x,y) = x == rect.x
                 || y == rect.y
                 || x == rect.x + rect.w - 1
                 || y == rect.y + rect.h - 1

placeRoom : Rect Int -> Map MapTile -> Map MapTile
placeRoom rect map =
  Map.union
    (mapRect rect (\ l -> if onEdge rect l then wall else floor))
    map

newMap : Rect Int -> Map MapTile
newMap rect = mapRect rect (always rock) |> placeRoom (Rect 20 10 20 20)

mapRect : Rect Int -> (Location -> MapTile) -> Map MapTile
mapRect rect tileGetter =
  Map.fromList <|
    List.concatMap (\ x -> List.map (\ y -> ((x,y), tileGetter (x,y)))
                                    (List.range rect.y (rect.y + rect.h - 1)))
                   (List.range rect.x (rect.x + rect.w - 1))

newPlayer = (30, 20)

rect = { x = 0, y = 0, w = 60, h = 40 }

newModel = Model (newMap rect) newPlayer

type Msg =
   KeyPress Int
 | Focus

keyCmds = Dict.fromList
  [ (72, move (-1,0))
  , (74, move (0,1))
  , (75, move (0,-1))
  , (76, move (1,0))
  , (89, move (-1,-1))
  , (85, move (1,-1))
  , (66, move (-1,1))
  , (78, move (1,1))
  ]

canMove : Location -> Model -> Bool
canMove l model = case Dict.get l model.map of
  Nothing -> False
  Just t -> t.passable

move : (Int, Int) -> Model -> Model
move (dx, dy) model =
  let (x, y) = model.player
      newPos = (x+dx, y+dy)
  in if canMove newPos model then { model | player = (x + dx, y + dy) } else model

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = pureUpdate msg model ! []

pureUpdate : Msg -> Model -> Model
pureUpdate msg model = case Debug.log "update" msg of
  KeyPress key -> case Dict.get (Debug.log "keypress" key) keyCmds of
    Nothing -> model
    Just action -> action model
  Focus -> model

onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
  Html.Events.on "keydown" (Json.map tagger Html.Events.keyCode)

viewGame : Model -> Html msg
viewGame {map, player} =
  Console.fromMap rect map .tile 
    |> Console.draw player playerTile
    |> Console.view

view : Model -> Html Msg
view model =
  Html.div [ onKeyPress KeyPress
           , Html.Attributes.tabindex 0
           , Html.Attributes.id "game"
           ]
           [ viewGame model
           ]

main = Html.program { init = newModel ! [ Task.attempt (always Focus) (Dom.focus "game") ]
                    , view = view
                    , update = update
                    , subscriptions = always Sub.none
                    }
