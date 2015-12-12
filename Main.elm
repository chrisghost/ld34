import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (..)


-- MODEL

type alias Model =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , a : Float
  }


ship : Model
ship =
  { x = 0
  , y = 0
  , vx = 0
  , vy = 0
  , a = 0
  }


type alias Keys = { x: Int, y: Int }

-- UPDATE

--getvecship : Model -> String
getvecship ship = (-(sin ship.a), (cos ship.a))

update : (Float, Keys) -> Model -> Model
update (dt, keys) ship =
  let 
    vec = (getvecship ship)
    --_ = log "> " vec
  in 
    ship
      |> rotateShip keys.x dt
      |> thrust keys.y dt
      |> physics dt


rotateShip : Int -> Float -> Model -> Model
rotateShip x dt ship = if x == -1 then
                          { ship | a = ship.a + (dt * 0.1) }
                       else
                          ship

thrust : Int -> Float -> Model -> Model
thrust y dt ship = if y == 1 then
                     let
                       (xt', yt') = (getvecship ship)
                       (xt, yt) = (xt' * 0.3, yt' * 0.3)
                     in
                       {
                         ship |
                                vx = ship.vx + xt,
                                vy = ship.vy + yt
                       }
                   else
                     ship


physics : Float -> Model -> Model
physics dt ship =
  { ship |
      x = ship.x + dt * ship.vx,
      y = ship.y + dt * ship.vy
  }



-- VIEW

drawShip : Model -> Form
drawShip ship = group [
                  circle 20
                    |> filled (rgb 255 0 0)
                  ,
                  rect 10 50
                    |> filled (rgb 0 0 0)
                    |> move (0, -20)
                ]
                  |> rotate ship.a
                  |> move (ship.x, ship.y)

view : (Int, Int) -> Model -> Element
view (w',h') ship =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238),
        drawShip ship,
        toForm (show (getvecship ship))
          |> move (-100, -100),
        toForm (show ship.a)
          |> move (-100, -150)
      ]


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update ship input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
