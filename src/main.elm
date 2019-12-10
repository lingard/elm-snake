module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import Json.Decode as Decode
import Random exposing (Generator)
import Time

type alias Coord =
  (Int, Int)

type Direction
  = Left
  | Right
  | Up
  | Down
  | Other

type alias Model =
  { grid: (Int, Int)
  , food: List((Int, Int))
  , foodQueue: List((Int, Int))
  , snake: List((Int, Int))
  , direction: Direction
  , gameOver: Bool
  }

initModel : Model
initModel =
  { grid = (24, 24)
  , food = [(14, 20)]
  , foodQueue = []
  , snake = [(12, 12), (11, 12), (10, 12)]
  , direction = Right
  , gameOver = False
  }

init : () -> (Model, Cmd Msg)
init _ =
  (initModel, Cmd.none)

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

toDirection : String -> Direction
toDirection string =
  case string of
    "ArrowLeft" ->
      Left

    "ArrowRight" ->
      Right

    "ArrowUp" ->
      Up
    
    "ArrowDown" ->
      Down

    _ ->
      Other

type Msg =
  KeyDown String | Tick Time.Posix | RandomFoodPositionResult Int

fst (a, _) = a
snd (_, b) = b

getNewHeadFromDirection : Direction -> List((Int, Int)) -> (Int, Int)
getNewHeadFromDirection direction snake =
  case List.head snake |>
    Just (x, y) ->
      case direction of
        Left ->
          (x >> 0 x 23)
        Right ->
          clamp 0 24 x
    Nothing ->
      (0, 0)

updateSnake : Model -> Model
updateSnake { snake, food, foodQueue, direction } =
  let
    (x, y) = Maybe.withDefault (0, 0) <| List.head snake
    head = 
      case direction of
        Left -> 
          case (x) of
            0 -> (23, y)
            _  -> (x - 1, y)
          
        Right ->
          case (x + 1) of
            24 -> (0, y)
            _  -> (x + 1, y)

        Up ->
          case (y) of
            0 -> (x, 23)
            _  -> (x, y - 1)

        Down -> 
          case (y + 1) of
            24 -> (x, 0)
            _  -> (x, y + 1)
        _ -> (x, y)

    last = Maybe.withDefault (0, 0) <| List.head <| List.reverse snake

    newSnake = head :: (List.reverse <| Maybe.withDefault [] <| List.tail <| List.reverse snake)
    (foundFood, foo) = (List.partition (==) food)
  in
    case List.head foodQueue of
      Just _ ->
        { snake |
          snake = List.append newSnake [last],
          food = foo,
          foodQueue = Maybe.withDefault([]) <| List.tail <| foodQueue ++ foundFood
        }
      Nothing ->
        (newSnake, foo, foodQueue ++ foundFood)

randomGridPositionGenerator : Generator Int
randomGridPositionGenerator =
  Random.int 0 23

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown key ->
      let 
        direction = toDirection key
        newDir =
          case (direction, model.direction) of
            (Other, _) ->
              model.direction
            (Left, Right) ->
              model.direction
            (Left, _) ->
              Left
            (Right, Left) ->
              model.direction
            (Right, _) ->
              Right
            (Up, Down) ->
              model.direction
            (Up, _) ->
              Up
            (Down, Up) ->
              model.direction
            (Down, _) ->
              Down
      in
        (
          { model | direction = newDir },
          Cmd.none
        )

    RandomFoodPositionResult position ->
      ({ model | food = List.append [(position, position)] model.food }, Cmd.none)

    Tick _ ->
      let
        (newSnake, food, foodQueue) = updateSnake model
        head = Maybe.withDefault (0, 0) <| List.head newSnake
        (foundSnake, _) = (List.partition (\(a, b) -> (a == (fst head) && b == (snd head))) (Maybe.withDefault [] <| List.tail newSnake))
      in
      case List.length foundSnake of
        1 ->
          ({ model | gameOver = True }, Cmd.none)
        _ ->
          (
            { model |
              snake = newSnake,
              food = food,
              foodQueue = foodQueue
            },
            case (List.isEmpty food) of
              True ->
                Random.generate RandomFoodPositionResult randomGridPositionGenerator
              _ ->
                Cmd.none
          )
    -- _ ->
    --   model

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.gameOver of
    False ->
      Sub.batch
        [ onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , Time.every (clamp 30 400 (300 - (toFloat (30 * (List.length model.snake))))) Tick
        ]
    True ->
      Sub.none

snakePartView : (Int, Int) -> (Int, Int) -> Html Msg
snakePartView (n, m) (x, y) =
  div [
    class "snake-part",
    style "width" "24px",
    style "height" "24px",
    style "background-color" "red",
    style "position" "absolute",
    style "left" ((String.fromInt (24 * x)) ++ "px"),
    style "top" ((String.fromInt (24 * y)) ++ "px")
  ] []

foodView : (Int, Int) -> (Int, Int) -> Html Msg
foodView (n, m) (x, y) =
  div [
    class "food-part",
    style "width" "24px",
    style "height" "24px",
    style "background-color" "yellow",
    style "position" "absolute",
    style "left" ((String.fromInt (24 * x)) ++ "px"),
    style "top" ((String.fromInt (24 * y)) ++ "px")
  ] []

view : Model -> Html Msg
view { snake, food, grid, gameOver } =
  case gameOver of
    False ->
      div []
        [
          div
            [ style "background" "black"
            , style "position" "relative"
            , style "width" ((String.fromInt (24 * 24)) ++ "px")
            , style "height" ((String.fromInt (24 * 24)) ++ "px")
            ]
            [ div [] <| List.map (snakePartView grid) snake
            , div [] <| List.map (foodView grid) food
            ],
          div []
            [
              text ((String.fromInt ((List.length snake) - 3)) ++ " points")
            ]
        ]
    True ->
      div []
        [ text "Game over fool... "
        , text ((String.fromInt ((List.length snake) - 3)) ++ " points")
          ]

