module Games.Roguelike.FOV exposing
  (fov
  )

{-| Utility for computing FOV.

@docs fov

-}

import Dict exposing (Dict)
import List
import Maybe
import Tuple exposing (..)

type alias FOVIn a =
  { passesLight : (Int, Int) -> Bool
  , setVisible : (Int, Int) -> Bool -> a -> a
  , range : Int
  }
type alias FOVAlg a = FOVIn a -> a -> a

getXs : Int -> (Float, Float) -> List Int
getXs y (minSlope, maxSlope) =
  List.range (floor ((toFloat y - 0.5) * minSlope + 0.5))
             (ceiling ((toFloat y + 0.5) * maxSlope + 0.5) - 1)

step : (Int, Bool) -> (Maybe Int, List (Int, Int)) -> (Maybe Int, List (Int, Int))
step (x, p) (m, r) = case (m, p) of
  (Nothing, False) -> (m, r)
  (Nothing, True) -> (Just x, r)
  (Just y, False) -> (Nothing, (x+1, y) :: r)
  (Just y, True) -> (m, r)

finish : Int -> (Maybe Int, List (Int, Int)) -> List (Int, Int)
finish x (m, r) = case m of
  Nothing -> r
  Just y -> (x, y) :: r

getBlocks : ((Int, Int) -> Bool) -> Int -> List Int -> List (Int, Int)
getBlocks passesLight y xs = case xs of
  [] -> [] -- shouldn't happen
  (x :: _) ->
    xs |> List.map (\ x -> (x, passesLight (x, y)))
       |> List.foldr step (Nothing, [])
       |> finish x

toSlopes : Int -> (Float, Float) -> (Int, Int) -> (Float, Float)
toSlopes y (minSlope, maxSlope) (xMin, xMax)
  = (toMinSlope y minSlope (xMin-1), toMaxSlope y maxSlope (xMax+1))

toMaxSlope : Int -> Float -> Int -> Float
toMaxSlope y maxSlope x = min maxSlope ((toFloat x - 0.5) / (toFloat y + 0.5))

toMinSlope : Int -> Float -> Int -> Float
toMinSlope y minSlope x = max minSlope ((toFloat x + 0.5) / (toFloat y - 0.5))

updateSlopes : Int -> (Float, Float) -> List (Int, Int) -> List (Float, Float)
updateSlopes y slopes = List.map (toSlopes y slopes)

row : ((Int, Int) -> Bool)
      -> ((Int, Int) -> Bool -> a -> a)
      -> Int
      -> List (Float, Float)
      -> a
      -> (a, List (Float, Float))
row passesLight setVisible y slopes map =
  let fringe = List.map (\ s -> (s, getXs y s)) slopes
  in ( List.foldr (\ x m -> setVisible (x, y) True m) map (List.concatMap second fringe)
     , List.concatMap (\ (s, xs) -> getBlocks passesLight y xs |> updateSlopes y s) fringe)

octant : FOVAlg a
octant i map =
  List.range 1 i.range
    |> List.foldl (\ y (m, slopes) -> row i.passesLight i.setVisible y (Debug.log "row" slopes) m)
                  (map, [(0,1)])
    |> first
    |> i.setVisible (0,0) True

flipXY : (Int, Int) -> (Int, Int)
flipXY (x, y) = (y, x)

flipX : (Int, Int) -> (Int, Int)
flipX (x, y) = (-x, y)

flipY : (Int, Int) -> (Int, Int)
flipY (x, y) = (x, -y)

trans : ((Int, Int) -> (Int, Int)) 
         -> FOVIn a -> FOVIn a
trans f i = { i | passesLight = i.passesLight << f
                , setVisible = i.setVisible << f }

quadrant : FOVAlg a
quadrant i = octant i >> octant (trans flipXY i)

halfPlane : FOVAlg a
halfPlane i = quadrant i >> quadrant (trans flipY i)

plane : FOVAlg a
plane i = halfPlane i >> halfPlane (trans flipX i)

{-| Compute field of view information.

    fov visible origin range

Here `visible : Dict (Int, Int) Bool` represents which squares can be seen
through; `origin : (Int, Int)` is the point from which the field of view
originates; and `range : Int` is the maximum range of vision. The return type
is `Dict (Int, Int) Bool` which represents which squares are visible. Every
visible square is present in the `Dict` and has the value `True`. Invisible
squares may have value `False` or may be absent from the `Dict`.

As of this writing, the algortithm used is shadow casting. A square is
considered visible if any part of it is visible from the center of the origin
square.
-}
fov : Dict (Int, Int) Bool -> (Int, Int) -> Int -> Dict (Int, Int) Bool
fov map (x, y) range =
  plane  { passesLight = \ (x1, y1) -> Maybe.withDefault False
                                        <| Dict.get (x1+x, y1+y) map
         , setVisible = \ (x1, y1) v -> Dict.insert (x1+x, y1+y) v
         , range = range }
         Dict.empty
