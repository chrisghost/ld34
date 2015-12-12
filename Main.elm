import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (..)


-- MODEL

type alias Positioned a = { a | x : Float, y : Float }

type alias Ship =
  Positioned {
    vx : Float
  , vy : Float
  , a : Float
  }

type alias Wall = Positioned { w: Int, h: Int }

type alias Model =
  {
    walls : List Wall,
    ship: Ship
  }


model : Model
model = Model [ {x= -100, y=100, w= 500, h= 20 } ] {x=0, y=0, vx=0, vy=0, a=0}
--model = Model [ Wall { -100 100 500 20} ] {x=0, y=0, vx=0, vy=0, a=0}

type alias Keys = { x: Int, y: Int }

-- UPDATE

getvecship : Ship -> (Float, Float)
getvecship ship = (-(sin ship.a), (cos ship.a))

--update : (Float, Keys) -> Model -> Model
update (dt, keys) model =
  let
    vec = getvecship model.ship
    --_ = log "> " vec
    ship' = model.ship
      |> rotateShip keys.x dt
      |> thrust keys.y dt
      |> physics model.ship.vx 0 dt

    walls' = List.map (physics 0 -model.ship.vy dt) model.walls
  in
    { model |
      walls = walls',
      ship = ship'
    }


rotateShip : Int -> Float -> Ship -> Ship
rotateShip x dt ship = if x == -1 then
                          { ship | a = ship.a + (dt * 0.1) }
                       else
                          ship

thrust : Int -> Float -> Ship -> Ship
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

--physics : Float -> Float -> Float -> Positioned {} -> Positioned {}
physics vx vy dt obj = { obj | x = obj.x + dt * vx, y = obj.y + dt * vy }

-- VIEW

drawWall : Wall -> Form
drawWall wall = rect (toFloat wall.w) (toFloat wall.h)
                  |> filled (rgb 0 0 0)
                  |> move (wall.x, wall.y)

drawShip : Ship -> Form
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
view (w',h') model =
  let
    (w,h) = (toFloat w', toFloat h')
  in
    collage w' h'
      ([ rect w h
          |> filled (rgb 174 238 238),
        drawShip model.ship,
        toForm (show (getvecship model.ship))
          |> move (-100, -100),
        toForm (show model.ship.a)
          |> move (-100, -150)
      ] ++ (List.map drawWall model.walls)
    )


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update model input)


input : Signal (Float, Keys)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map2 (,) delta Keyboard.arrows)
