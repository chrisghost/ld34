import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (..)
import Touch


-- MODEL

type alias Positioned a = { a | x : Float, y : Float }

type alias Ship =
  Positioned {
    vx : Float
  , vy : Float
  , a : Float
  , size: Float
  }

type alias Wall = Positioned { w: Float, h: Float }

type GameState = Game | Dead

type alias Model =
  {
    defaultWalls : List Wall,
    walls : List Wall,
    ship: Ship,
    state: GameState
  }

(gameWidth, gameHeight) = (600,1000)

startWalls : List Wall
startWalls = [
               {x= -200, y= 100, w= 400, h= 20 }
             , {x= 200, y= 300, w= 400, h= 20 }
             , {x= -250, y= 300, w= 100, h= 20 }
             ]

defaultWalls : List Wall
defaultWalls = [
                 {x= 300, y= 0, w= 30, h= gameHeight }
               , {x= -300, y= 0, w= 30, h= gameHeight }
               ]

defaultModel : Model
defaultModel = Model defaultWalls startWalls {x= 0, y= -300, vx=0, vy=0, a=0, size=10} Game
--model = Model [ Wall { -100 100 500 20} ] {x=0, y=0, vx=0, vy=0, a=0}

type alias Keys = { x: Int, y: Int }

-- UPDATE

getvecship : Ship -> (Float, Float)
getvecship ship = (-(sin ship.a), (cos ship.a))

--update : (Float, Keys) -> Model -> Model
update (dt, keys, ltch) model =
  case model.state of
    Game ->
      let
        vec = getvecship model.ship
        button1Pressed = keys.x == -1 || List.any (\t -> t.x < 0) ltch
        button2Pressed = keys.y ==  1 || List.any (\t -> t.x > 0) ltch
        _ = log ">>" ltch
        ship' = model.ship
          |> rotateShip button1Pressed dt
          |> thrust button2Pressed dt
          |> physics model.ship.vx 0 dt

        walls' = List.map (physics 0 -model.ship.vy dt) model.walls

        state' = if (List.any identity (
                        (List.map (collision ship') walls') ++
                        List.map (collision ship') model.defaultWalls
                      )) then
                   Dead
                 else
                   model.state
      in
        { model |
          walls = walls',
          ship = ship',
          state = state'
        }
    Dead ->
      model


rotateShip : Bool -> Float -> Ship -> Ship
rotateShip x dt ship = if x then
                          { ship | a = ship.a + (dt * 0.1) }
                       else
                          ship

thrust : Bool -> Float -> Ship -> Ship
thrust y dt ship = if y then
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

collision : Ship -> Wall -> Bool
collision ship wall = (dist (ship.x, ship.y) (getClosest ship wall)) < ship.size

dist : (Float, Float) -> (Float, Float) -> Float
dist (x,y) (x2,y2) = sqrt ((x - x2) ^ 2) + ((y - y2) ^ 2)

getClosest ship wall =
  if ship.y > wall.y+wall.h/2 then -- TOP
    if ship.x < wall.x-wall.w/2 then -- top-left
       (wall.x - wall.w/2, wall.y + wall.h/2)
    else if ship.x < wall.x+wall.w/2 && ship.x > wall.x-wall.w/2 then -- top-middle
       (ship.x, wall.y + wall.h/2)
    else -- top-right
       (wall.x + wall.w/2, wall.y + wall.h/2)
  else if ship.y < wall.y+wall.h/2 && ship.y > wall.y-wall.h/2 then -- MIDDLE
    if ship.x < wall.x-wall.w/2 then -- middle-left
       (wall.x - wall.w/2, ship.y)
    else if ship.x < wall.x+wall.w/2 && ship.x > wall.x-wall.w/2 then -- middle-middle
       (ship.x, ship.y)
    else -- middle-right
       (wall.x + wall.w/2, ship.y)
  else -- BOTTOM
    if ship.x < wall.x-wall.w/2 then -- bottom-left
       (wall.x - wall.w/2, wall.y - wall.h/2)
    else if ship.x < wall.x+wall.w/2 && ship.x > wall.x-wall.w/2 then -- bottom-middle
       (ship.x, wall.y - wall.h/2)
    else -- bottom-right
       (wall.x + wall.w/2, wall.y - wall.h/2)


-- VIEW

drawWall : (Float, Float) -> Wall -> Form
drawWall (xr, yr) wall = rect (wall.w * xr) (wall.h * yr)
                            |> filled (rgb 0 0 0)
                            |> move (wall.x * xr, wall.y * yr)

drawShip : (Float, Float) -> Ship -> Form
drawShip (xr, yr) ship = group [
                  circle (ship.size * (xr+yr)/2)
                    |> filled (rgb 255 0 0)
                  ,
                  rect 10 50
                    |> filled (rgb 0 0 0)
                    |> move (0, -20)
                ]
                  |> rotate ship.a
                  |> move (ship.x * xr, ship.y * yr)

view : (Int, Int) -> Model -> Element
view (w',h') model =
  let
    (w,h) = (toFloat w', toFloat h')

    (xRatio, yRatio) = ((w/gameWidth), (h/gameHeight))
  in
    collage w' h'
      ([ rect w h
          |> filled (rgb 174 238 238),
        drawShip (xRatio, yRatio) model.ship,
        toForm (show (getvecship model.ship))
          |> move (-100, -100),
        toForm (show model.ship.a)
          |> move (-100, -150)
      ] ++
      (List.map (drawWall (xRatio, yRatio)) model.walls) ++
      (List.map (drawWall (xRatio, yRatio)) model.defaultWalls) ++
      case model.state of
        Dead -> [toForm (show "YOU DEAD!")
                  |> move (100, 50) ]
        _ -> []
    )


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update defaultModel input)


input : Signal (Float, Keys, List Touch.Touch)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows Touch.touches)
