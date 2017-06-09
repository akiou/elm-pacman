import Array
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Text
import Time exposing (..)
import Window


-- MODEL

type alias Model =
  { x : Int
  , y : Int
  , vx : Float
  , vy : Float
  , next : Direction
  , color : Color
  }

type alias Game =
  { pacman : Model
  , enemies : List(Model)
  , stage : List(List(Int))
  , score : Int
  , state : State
  , timeLimit : Float
  }

type Direction = Left | Right | Up | Down
type State = Play | Clear | GameOver


type alias Keys = { x:Int, y:Int }


type alias Input =
  { keys : Keys
  , space : Bool
  , time : Float
  }

type alias Inputs =
  { input : Input
  , ps : List(Model)
  }

pacman = { x = 10, y = -160, vx = 0, vy = 0, next = Down, color = yellow }

enemy1 = { x = 10, y = 80, vx = 0, vy = -1, next = Left, color = lightRed }
enemy2 = { x = 10, y = 80, vx = 0, vy = -1, next = Left, color = lightGreen }
enemy3 = { x = 10, y = 80, vx = 0, vy = -1, next = Left, color = lightBlue }
enemy4 = { x = 10, y = 80, vx = 0, vy = -1, next = Left, color = lightPurple }

game : Game
game =
  { pacman = pacman
  , enemies = [ enemy1, enemy2, enemy3, enemy4 ]
  , stage = stage
  , score = 0
  , state = Play
  , timeLimit = 0
  }



-- SUBFUNCTION

get : Int -> a -> List a -> a
get n default list =
  list
    |> Array.fromList
    |> Array.get n
    |> Maybe.withDefault default

set : Int -> a -> List a -> List(a)
set n elem list =
  list
    |> Array.fromList
    |> Array.set n elem
    |> Array.toList

toStage : (Int, Int) -> List(List(Int)) -> Int
toStage (x, y) stage =
  let (x', y') = ((x + 300) // 20, (-y + 330) // 20) in
  stage
    |> get y' []
    |> get x' 1


successable : (Int, Int) -> Direction -> List(List(Int)) -> Bool
successable (x, y) next stage =
  (next == Up && toStage (x + 9, y + 11) stage /= 0 && toStage (x - 9, y + 11) stage /= 0) ||
  (next == Down && toStage (x + 9, y - 11) stage /= 0 && toStage (x - 9, y - 11) stage /= 0) ||
  (next == Right && toStage (x + 11, y + 9) stage /= 0 && toStage (x + 11, y - 9) stage /= 0) ||
  (next == Left && toStage (x - 11, y + 9) stage /= 0 && toStage (x - 11, y - 9) stage /= 0)


eatFood : (Int, Int) -> (List(List(Int)), Int) -> Int -> (List(List(Int)), Int, Bool)
eatFood (x, y) (stage, score) status =
  let (x', y') = ((x + 300) // 20, (-y + 330) // 20) in
  let
    new_row = stage
      |> get y' []
      |> set x' 1
  in
  let bigfood = status == 3 in
  ( stage
     |> set y' new_row
  , score + if status == 2 then 10 else 50
  , bigfood)


allEat : List(List(Int)) -> Bool
allEat stage =
  not (List.foldr (||) False (List.concat [List.map (List.member 2) stage, List.map (List.member 3) stage]))


hitEnemies : Game -> Bool
hitEnemies { pacman, enemies, stage, score, state } =
  let range = 15 in
  List.map (hitEnemy pacman) enemies
    |> List.foldr (||) False


hitEnemy : Model -> Model -> Bool
hitEnemy pacman enemy =
  (abs (pacman.x - enemy.x) < 15 && abs (pacman.y - enemy.y) < 15 )


vec : Model -> List(List(Int)) -> Model -> Array.Array Direction
vec pacman stage enemy =
  let (r, theta) = toPolar (toFloat (pacman.x - enemy.x), toFloat (pacman.y - enemy.y)) in
  if theta <= -pi * 3 / 4 then        Array.fromList [ Left, Down, Up, Right ]
  else if theta <= -pi / 2 then      Array.fromList [ Down, Left, Right, Up ]
  else if theta <= -pi / 4 then       Array.fromList [ Down, Right, Left, Up ]
  else if theta <= 0 then               Array.fromList [ Right, Down, Up, Left ]
  else if theta <= pi / 4 then        Array.fromList [ Right, Up, Down, Left ]
  else if theta <= pi / 2 then        Array.fromList [ Up, Right, Left, Down ]
  else if theta <= pi * 3 / 4 then  Array.fromList [ Up, Left, Right, Down ]
  else                                               Array.fromList [ Left, Up, Down, Right ]


-- UPDATE

updateGame : Inputs -> Game -> Game
updateGame ({input, ps} as inputs) game =
  let
    p1 = List.head ps
      |> Maybe.withDefault pacman
    nomalTime = game.timeLimit <= 0
    nextEnemy enemy p =
      if not (hitEnemy p1 enemy)
      then updateEnemy enemy game.stage p nomalTime
      else { enemy | x = 10, y = 80, vx = 0, vy = -1, next = Left }
  in
  if allEat game.stage || game.state == Clear then
    { game | state = Clear }
  else if (hitEnemies { game | pacman = p1} && nomalTime) || game.state == GameOver then
    { game | state = GameOver }
  else
    let
      newEnemies
        = List.map2 nextEnemy game.enemies ps
      (newStage, newScore, bigfood) = updateStageandScore (game.stage, game.score) p1
      addScore = if (not nomalTime) && (hitEnemies { game | pacman = p1 }) then 200 else 0
      newState = Play
      newTimeLimit = if bigfood then 10000 else if not nomalTime then game.timeLimit - input.time else 0
      weakColor = darkBlue
    in

    { game |
        pacman = p1,
        enemies = List.map2 (\e c -> { e | color = if nomalTime then c else weakColor }) newEnemies [ lightRed, lightGreen, lightBlue, lightPurple ],
        stage = newStage,
        score = newScore + addScore,
        state = newState,
        timeLimit = newTimeLimit
    }


-- UPDATE_PACMAN

updatePacman : List(List(Int)) -> Input -> Model -> Model
updatePacman stage ({keys, space, time} as input) pacman =
  pacman
    |> setNext keys
    |> curve stage
    |> warp
    |> physics 5


setNext : Keys -> Model -> Model
setNext keys pacman =
  { pacman |
      next =
        if keys.x == 1 then
          Right
        else if keys.x == -1 then
          Left
        else if keys.y == 1 then
          Up
        else if keys.y == -1 then
          Down
        else pacman.next
  }



warp : Model -> Model
warp pacman =
  let add = if pacman.x > 300 then -620 else if pacman.x < -320 then 630 else 0 in
    { pacman | x = pacman.x + add}


curve : List(List(Int)) -> Model -> Model
curve stage pacman =
  let (x, y) = (pacman.x, pacman.y) in
  if pacman.next == Right && (successable (x, y) Right stage) then
    { pacman | vx = 1, vy = 0 }
  else if  pacman.next == Left && (successable (x, y) Left stage) then
    { pacman | vx = -1, vy = 0 }
  else if  pacman.next == Up && (successable (x, y) Up stage) then
    { pacman | vx = 0, vy = 1 }
  else if  pacman.next == Down && (successable (x, y) Down stage) then
    { pacman | vx = 0, vy = -1 }
  else if (pacman.vx > 0 && not (successable (x + 1, y) Right stage)) ||
             (pacman.vx < 0 && not (successable (x - 1, y) Left stage)) ||
             (pacman.vy > 0 && not (successable (x, y + 1) Up stage)) ||
             (pacman.vy < 0 && not (successable (x, y - 1) Down stage))  then
    { pacman | vx = 0, vy = 0}
  else
    pacman


physics : Int -> Model -> Model
physics dt pacman =
  { pacman |
      x = pacman.x + (round pacman.vx) * dt,
      y = pacman.y + (round pacman.vy) * dt
  }


-- UPDATE_ENEMY



updateEnemy : Model -> List(List(Int)) -> Model -> Bool -> Model
updateEnemy enemy stage pacman nomalTime =
  enemy
    |> setDirection
    |> curveEnemy pacman stage nomalTime
    |> warp
    |> physics (if nomalTime then 5 else 2)
    |> correction nomalTime



setDirection : Model -> Model
setDirection enemy =
  let
    dir = if enemy.vx > 0 then Right
    else if enemy.vx < 0 then Left
    else if enemy.vy > 0 then Up
    else Down
  in
  { enemy | next = dir }


curveEnemy : Model -> List(List(Int)) -> Bool -> Model -> Model
curveEnemy pacman stage nomalTime enemy =
  let (x, y) = (enemy.x, enemy.y) in
  if (enemy.vx /= 0 && ((successable (x, y) Down stage) || (successable (x, y) Up stage))) || (enemy.vy /= 0 && ((successable (x, y) Right stage) || (successable (x, y) Left stage))) then
    let
      vect = if nomalTime then vec pacman stage enemy else vec enemy stage pacman
      newNext =
        if (successable (enemy.x, enemy.y) (Maybe.withDefault Down (Array.get 0 vect)) stage) then
          Maybe.withDefault Down (Array.get 0 vect)
        else if (successable (enemy.x, enemy.y) (Maybe.withDefault Down (Array.get 1 vect)) stage) then
          Maybe.withDefault Down (Array.get 1 vect)
        else if (successable (enemy.x, enemy.y) (Maybe.withDefault Down (Array.get 2 vect)) stage) then
          Maybe.withDefault Down (Array.get 2 vect)
        else
          Maybe.withDefault Down (Array.get 3 vect)
      (vx, vy) =
        if newNext == Left then (-1, 0)
        else if newNext == Right then (1, 0)
        else if newNext == Up then (0, 1)
        else (0, -1)
    in
    { enemy |
        next = newNext,
        vx = vx,
        vy = vy
    }
  else
    enemy


correction : Bool -> Model -> Model
correction nomalTime enemy =
  let (x, y) = (enemy.x, enemy.y) in
  if nomalTime then
    { enemy | x = x // 5 * 5, y = y // 5 * 5 }
  else enemy


-- UPDATE_STAGE


updateStageandScore : (List(List(Int)), Int) -> Model -> (List(List(Int)), Int, Bool)
updateStageandScore (stage, score) pacman =
  (stage , score)
    |> eat pacman

eat : Model -> (List(List(Int)), Int) -> (List(List(Int)), Int, Bool)
eat pacman (stage, score) =
  let
    (x, y) = (pacman.x, pacman.y)
    status = toStage (x, y) stage
  in
  if status == 2 || status == 3 then
    eatFood (x, y) (stage, score) status
  else
    (stage, score, False)



-- VIEW

view : (Int, Int) -> Game -> Element
view (w',h') game =
  let score = txt (Text.height 30) green ("SCORE  " ++ toString game.score) in
  if game.state == Clear then
    let endText = txt (Text.height 20) white "CREAR" in
    collage 600 660 (List.concat
      [ makestage game.stage
      , [toForm score
          |> move (-190, -310)
        , toForm endText
          |> move (0, -70)
      ]])
  else if game.state == GameOver then
    let endText = txt (Text.height 20) white "GAME OVER" in
    collage 600 660 (List.concat
      [ makestage game.stage
      , [toForm score
          |> move (-190, -310)
        , toForm endText
          |> move (0, -70)
      ]])
  else
    collage 600 660 (List.concat
      ([ makestage game.stage
      , draw game.pacman
      , [toForm score
          |> move (-190, -310)
      ]] ++ List.map draw game.enemies))


txt f color string =
  Text.fromString string
    |> Text.color color
    |> Text.monospace
    |> f
    |> leftAligned


makestage : List(List(Int)) -> List(Form)
makestage stage =
  stage
      |> List.concat
      |> List.map2 makeblock [0..999]
      |> List.concat


makeblock : Int -> Int -> List(Form)
makeblock pos n =
  let (x, y) = (pos % 30 * 20 - 290, 320 - pos // 30 * 20) in
  if n == 0 then
    [ rect 20 20
         |> filled blue
         |> move (toFloat x, toFloat y)
    ]
  else if n == 1 then
    [ rect 20 20
        |> filled black
        |> move (toFloat x, toFloat y)
    ]
  else if n == 2 then
    [ rect 20 20
        |> filled black
        |> move (toFloat x, toFloat y)
    , circle 2
        |> filled yellow
        |> move (toFloat x, toFloat y)
    ]
  else
    [ rect 20 20
        |> filled black
        |> move (toFloat x, toFloat y)
    , circle 5
        |> filled yellow
        |> move (toFloat x, toFloat y)
    ]


draw : Model -> List(Form)
draw model =
  [ circle 9
      |> filled model.color
      |> move (toFloat model.x, toFloat model.y)
  ]

stage : List(List(Int))
stage =
  [ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 3, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 3, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 3, 0, 0]
  , [0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0]
  , [0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0]
  , [0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  , [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]]

-- SIGNALS

main : Signal Element
main = Signal.map2 view Window.dimensions gameState


gameState : Signal Game
gameState = Signal.foldp updateGame game (inputs input)


inputs : Signal Input -> Signal Inputs
inputs input =
  let
    delay = Time.delay 1000
    newP = Signal.foldp (updatePacman stage) pacman input -- Signal Model (Pacman)
    delayP1 = delay newP
    delayP2 = delay delayP1
    delayP3 = delay delayP2
    pacmans = Signal.map4 (\p1 p2 p3 p4 -> [p1, p2, p3, p4]) newP delayP1 delayP2 delayP3
  in
  Signal.sampleOn (fps 30) (Signal.map2 Inputs input pacmans)


input : Signal Input
input =
  let
    keys = Keyboard.arrows
    space = Keyboard.space
    time = fps 30
  in
  Signal.sampleOn time (Signal.map3 Input keys space time)
