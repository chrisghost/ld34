import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug exposing (..)
import Touch
import Text

import Levels

-- MODEL

type alias Positioned a = { a | x : Float, y : Float }

type alias Ship =
  Positioned {
    vx : Float
  , vy : Float
  , a : Float
  , size: Float
  , sizeRatio: Float
  }

type alias Wall = Positioned { w: Float, h: Float, orientation: String }

type alias Goal = Positioned { size: Float }

type alias Fire = Positioned { size: Float, halo: Float }

type alias Smoke = Positioned { vx: Float, vy: Float, age: Int, size: Float }

type DeathCause = Fall | Burn
type GameState = Menu | Game | Won | Dead DeathCause


type alias Model =
  {
    defaultWalls : List Wall,
    walls : List Wall,
    goal : Goal,
    fires : List Fire,
    ship: Ship,
    state: GameState,
    smokes: List Smoke,
    t: Int
  }

(gameWidth, gameHeight) = (640,1000)

startWalls : List Wall
startWalls = [
               --{x= -200, y= 100, w= 400, h= 20 }
             --, {x= 200, y= 300, w= 400, h= 20 }
             --, {x= -250, y= 300, w= 100, h= 20 }
             ]

defaultWalls : List Wall
defaultWalls = []
--                 {x= 300, h= 64, w= 64, y= gameHeight/2, orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(1*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(2*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(3*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(4*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(5*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(6*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(7*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(8*64), orientation= "l"}
--               , {x= 300, h= 64, w= 64, y= gameHeight/2-(9*64), orientation= "l"}
--               , {x= -300, y= 0, w= 30, h= gameHeight , orientation= "lrtb"}
--               ]

defaultModel : Model
defaultModel = Model
                  defaultWalls
                  --startWalls
                  (Levels.getWalls 1)
                  (Levels.getGoal 1)
                  (Levels.getFires 1)
                  {x= 0, y= -300, vx=0, vy=0, a=0, size= 28, sizeRatio= 1}
                  Menu
                  []
                  0
--model = Model [ Wall { -100 100 500 20} ] {x=0, y=0, vx=0, vy=0, a=0}

loadLevel1: Model -> Model
loadLevel1 model = { model |
                     fires = Levels.getFires 1
                   , walls = Levels.getWalls 1
                   , goal = Levels.getGoal 1
                 }

type alias Keys = { x: Int, y: Int }

-- UPDATE
randomInt : Int -> Int -> Int -> Int
randomInt seed l h = let
                  rand = round (sin (toFloat seed) * 10000)
                  res = l + (rand % (h-l))
                in
                   res

getvecship : Ship -> (Float, Float)
getvecship ship = (-(sin ship.a), (cos ship.a))

rotateVec : Float -> Float -> Float -> (Float, Float)
rotateVec x y a = (
    x * cos a + y * -1 * sin a
  , x * sin a + y * cos a
  )

updateSmokes model dt button2Pressed = List.map
                    (\s -> let
                               s' = physics s.vx s.vy dt s
                           in
                              { s' | age = s'.age - 1 }
                    )
                    (List.filter
                      (\s -> s.age > 0 && s.x > -gameWidth/2 && s.x < gameWidth/2)
                      (model.smokes ++
                        if button2Pressed then
                          let
                            (vx, vy) = getvecship model.ship
                            --_ = log "model t" model.t
                            --_ = log "smoke age " (randomInt model.t 1 100)
                            --dirVar = 0.5 + (toFloat (randomInt model.t 0 100)/100) * 1.0
                            --_ = log "dirvar " dirVar
                            --_ = log "vx" (-vx)
                            --_ = log "vy" (-vy)
                            rotate = -0.3 + (toFloat (randomInt model.t 0 100)/100) * 0.6
                            (nvx, nvy) = rotateVec -vx -vy rotate
                            --_ = log "nvx nvy" (nvx, nvy)
                          in
                            [ {
                              x=model.ship.x
                            , y=model.ship.y
                            , vx= (nvx)*10
                            , vy= (nvy)*10
                            , age= randomInt model.t 3 10
                            , size= (toFloat 10) } ]
                        else []
                    ))


--update : (Float, Keys) -> Model -> Model
update (dt, keys, ltch) model =
  case model.state of
    Menu ->
      let
        button3Pressed = keys.x == 1 || List.any (\t -> t.x < 0) ltch
        state' = if button3Pressed then Game else Menu
      in
         { model | state = state' }
    Game ->
      let
        vec = getvecship model.ship
        button1Pressed = keys.x == -1 || List.any (\t -> t.x < 0) ltch
        button2Pressed = keys.y ==  1 || List.any (\t -> t.x > 0) ltch

        ship' = model.ship
          |> rotateShip button1Pressed dt
          |> thrust button2Pressed dt
          |> physics model.ship.vx 0 dt
          |> friction dt

        smokes' = updateSmokes model dt button2Pressed

        walls' = List.map (physics 0 -model.ship.vy dt) model.walls

        goal' = physics 0 -model.ship.vy dt model.goal

        fires' = List.map (physics 0 -model.ship.vy dt) model.fires
                  |> List.filter (\f -> List.all (\x -> not x) (List.map (\s -> collisionCircle s f) smokes' ))
                  |> List.map (\f -> 
                                if (model.t%10) > 7 then
                                   let
                                     inc = (toFloat
                                       (randomInt (
                                         round (f.x + f.y + toFloat model.t)
                                       ) 0 4)
                                     ) / 10
                                   in
                                { f
                                | halo =

                                  if f.halo + inc > 1.5 then f.halo + inc - 0.7 else f.halo + inc

                                  --max f.size ((f.size * 2) * (toFloat ( randomInt (round (f.x + f.y + toFloat model.t)) 0 10) / 10))
                                }
                                else
                                f
                )
        
--        fires'' = if randomInt model.t 0 100 < 10 then
--                     case (List.head (List.take (randomInt model.t 0 ((List.length fires') - 1)) fires')) of
--                       Just f -> fires' ++ [(findAdjacent model.fires f)]
--                       Nothing -> fires'
--                  else fires'

        state' = if (List.any identity (
                        (List.map (collision {ship' | size = 3 } ) walls') ++
                        [ship'.x < -gameWidth/2, ship'.x > gameWidth/2]
                        --List.map (collision ship') model.defaultWalls
                      ))
                      then
                     Dead Fall
                   else if List.any identity (List.map (collisionCircle ship') model.fires) then
                     Dead Burn
                   else if (collisionCircle ship' model.goal) then
                     Won
                 else
                   model.state
      in
        { model |
          walls = walls',
          fires = fires',
          ship = ship',
          state = state',
          smokes = smokes',
          goal = goal',
          t = model.t + round dt
        }
    state ->
      let
        ship = (model.ship)
                  |> physics model.ship.vx model.ship.vy dt
                  |> friction dt
        ship' = case state of
          Dead Fall -> { ship | sizeRatio = ship.sizeRatio * 0.95 }
          _ -> ship

        smokes' = updateSmokes model dt False

        button3Pressed = keys.x ==  1 --|| List.any (\t -> t.x > 0) ltch
        state' = if button3Pressed then Game else model.state

        ship'' = if button3Pressed then { ship' |
                                          x = defaultModel.ship.x
                                        , y = defaultModel.ship.y
                                        , vx = 0
                                        , vy = 0
                                        , sizeRatio = 1
                                      }
                                      else ship'

        model' = if button3Pressed then
                    loadLevel1 model
                    else model
      in
        { model' | ship = ship'', smokes = smokes', state = state' }


--findAdjacent : List Fire -> Fire -> Fire
--findAdjacent lst fire = if (not (List.any (\f -> f.x == fire.x - 64 && f.y == fire.y) lst ) ) then
--                           { x = fire.x - 64, y = fire.y, size= 32 }
--                        else if not (List.any (\f -> f.x == fire.x + 64 && f.y == fire.y) lst ) then
--                           { x = fire.x + 64, y = fire.y , size= 32 }
--                        else if not (List.any (\f -> f.x == fire.x && f.y == fire.y - 64) lst ) then
--                           { x = fire.x, y = fire.y - 64 , size= 32 }
--                        else if not (List.any (\f -> f.x == fire.x && f.y == fire.y + 64) lst ) then
--                           { x = fire.x, y = fire.y + 64 , size= 32 }
--                        else
--                           { x = fire.x, y = fire.y , size= 32 }


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

friction dt obj = { obj | vx = obj.vx * (1 - (0.1 / dt)) , vy = obj.vy * (1 - (0.1 / dt)) }

collisionCircle a b = let
                          dst = (dist (a.x, a.y) (b.x, b.y))
                          --_ = log "size" (a.size +  b.size)
                          --_ = log "dst" dst
                          --_ = log "a b" ((a.x, a.y),  (b.x, b.y))
                      in
                         dst < (a.size + b.size)

collision : Ship -> Wall -> Bool
collision ship wall = (dist (ship.x, ship.y) (getClosest ship wall)) < ship.size

dist : (Float, Float) -> (Float, Float) -> Float
dist (x,y) (x2,y2) = (sqrt ( ((x2 - x) ^ 2) + ((y2 - y) ^ 2) ))

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

drawSmoke : (Float, Float) -> Smoke -> Form
  --circle (smoke.size * (xr+yr)/2) |> filled (rgb 255 255 255)
drawSmoke (xr, yr) smoke = image 60 60 "img/smoke.png"
                            |> toForm
                            |> move (smoke.x * xr + smoke.vx * 6, smoke.y * yr + smoke.vy * 6)

drawGoal : (Float, Float) -> Goal -> Form
  --circle (smoke.size * (xr+yr)/2) |> filled (rgb 255 255 255)
drawGoal (xr, yr) goal = circle goal.size
                            |> filled (rgb 0 200 0)
                            |> move (goal.x * xr, goal.y * yr)

drawFire (xr, yr) fire = group [
                      --circle (fire.size * (xr+yr)/2) |> filled (rgb 255 0 0) |> move (fire.x * xr, fire.y * yr) ,
                          image (round (64*xr)) (round (64*yr)) "img/fire.png"
                            |> toForm
                            |> move (fire.x * xr, fire.y * yr)
                      ]

drawFireHalo (xr, yr) t fire = let
                              sz = ((fire.size * (xr+yr)/2) * 2) * fire.halo

                              grad1 = radial (0,0) 10 (0,10) sz
                                  [ (  0, rgba  255 0 0 0.3)
                                  , (0.5, rgba  228 199 0 0.1)
                                  , (  1, rgba 0 0 0 0)
                                  ]

                             in
                                 group [
                      --circle (fire.size * (xr+yr)/2) |> filled (rgb 255 0 0) |> move (fire.x * xr, fire.y * yr) ,

                      (gradient grad1 (circle sz))
                          --circle ((fire.size * (xr+yr)/2) * 2)
                            --|> filled (rgb 255 0 0)
                            --|> alpha 0.3
                            |> move (fire.x * xr, fire.y * yr)
                      ]

drawWall : (Float, Float) -> Wall -> Form
drawWall (xr, yr) wall = 
  --rect (wall.w * xr) (wall.h * yr)
                        image 64 64 ("img/wall_" ++ wall.orientation ++ ".png")
                            |> toForm
                            --|> filled (rgb 0 0 0)
                            |> move (wall.x * xr, wall.y * yr)

drawShip : (Float, Float) -> Ship -> Form
drawShip (xr, yr) ship = group [
                      --circle (ship.size * (xr+yr)/2) |> filled (rgb 255 0 0) ,
                    image (round ((60*xr) * ship.sizeRatio)) (round ((100*yr) * ship.sizeRatio)) "img/player.png"
                      |> toForm
                      |> move (0, -10)
--                  rect 10 50
--                    |> filled (rgb 0 0 0)
--                    |> move (0, -20)
                ]
                  |> rotate ship.a
                  |> move (ship.x * xr, ship.y * yr)

view : (Int, Int) -> Model -> Element
view (w',h') model =
  let
    (w,h) = (toFloat w', toFloat h')

    (xRatio, yRatio) = (1, 1) -- ((w/gameWidth), (h/gameHeight))
  in
     collage w' h'
       (case model.state of
         Menu -> 
           let
               style = { typeface = [ "Times New Roman", "serif" ]
                            , height   = Just 64
                            , color    = red
                            , bold     = False
                            , italic   = False
                            , line     = Nothing
                            }
           in
             ([ rect w h
                |> filled (rgb 205 205 205),
                Text.style style (Text.fromString "Office Fireman")
                  |> leftAligned
                  |> toForm
                  |> move (0, 250),
                Text.style { style | color = black } (Text.fromString "Controls\nLeft - Rotate\nUp - Thrust")
                  |> centered
                  |> toForm
                  |> move (0, 50),
                Text.style { style | color = blue } (Text.fromString "Press Right to start")
                  |> centered
                  |> toForm
                  |> move (0, -250)
             ])

         _ ->
           let
               style = { typeface = [ "Times New Roman", "serif" ]
                            , height   = Just 64
                            , color    = red
                            , bold     = False
                            , italic   = False
                            , line     = Nothing
                            }
           in
              ([ rect w h
                  |> filled (rgb 205 205 205)
                --toForm (show (getvecship model.ship))
                  --|> move (-100, -100),
                --toForm (show model.ship.a)
                  --|> move (-100, -150)
              ,  rect (w/2 - 300) h
                  |> filled (rgb 0 0 0)
                  |> move ( 300 + (((w/2) - 300)/2), 0)
              ,  rect (w/2 - 300) h
                  |> filled (rgb 0 0 0)
                  |> move ( (-1 * 300) - (((w/2) - 300)/2), 0)

              ] ++
              (List.map (drawWall (xRatio, yRatio)) model.walls) ++
              (List.map (drawWall (xRatio, yRatio)) model.defaultWalls) ++
              (List.map (drawSmoke (xRatio, yRatio)) model.smokes) ++
              (List.map (drawFireHalo (xRatio, yRatio) model.t) model.fires) ++
              (List.map (drawFire (xRatio, yRatio)) model.fires) ++
              [drawGoal (xRatio, yRatio) model.goal] ++
              (case model.state of
                Dead cause -> [
                               drawShip (xRatio, yRatio) model.ship
                              ] ++ (case cause of
                                Burn -> [

                                  Text.style style (Text.fromString "YOU BURNED")
                                    |> leftAligned
                                    |> toForm
                                    |> move (0, 250)
                                  , drawFire (xRatio, yRatio) model.ship ]
                                Fall -> [
                                  Text.style style (Text.fromString "YOU FELL")
                                    |> leftAligned
                                    |> toForm
                                    |> move (0, 250)
                                  ]) ++ [
                                rect w h
                                  |> filled (rgb 0 0 0)
                                  |> alpha 0.3]
                Won -> [
                  Text.style { style | color = green } (Text.fromString "YOU WON")
                    |> leftAligned
                    |> toForm
                    |> move (0, 250)
                , drawShip (xRatio, yRatio) model.ship
                , rect w h
                  |> filled (rgb 0 0 0)
                  |> alpha 0.3
                ]
                _ -> [drawShip (xRatio, yRatio) model.ship]
              )
            )
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

