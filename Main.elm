import Html exposing (Html, text, div, h1, button)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.App as App
import Maybe exposing (Maybe(Nothing, Just), withDefault)
import AnimationFrame
import Date exposing (Date)
import Keyboard
import Task

app =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

main = app

-- -- -- -- -- -- -- --
--       Model       --
-- -- -- -- -- -- -- --

type alias Model =
  { startTS : Int
  , started : Bool
  , suspended : Int
  , diff : Maybe Date
  }

model : Model
model =
  { startTS = 0
  , started = False
  , suspended = 0
  , diff = Nothing
  }

init =
  (model, Cmd.none)

-- -- -- -- -- -- -- --
--      Update       --
-- -- -- -- -- -- -- --

type ActionType = Start | Stop | Reset | Tick Float | NoOp

update : ActionType -> Model -> (Model, Cmd ActionType)
update action model =
  case action of
    Start ->
      ({model | started = True}, Cmd.none)
    Stop ->
      let
        diff = withDefault (Date.fromTime 82800000) model.diff
      in
        ({model |
            started = False
          , suspended = round (Date.toTime diff)
          , startTS = 0
         }, Cmd.none)
    Reset ->
      ({ startTS = 0
      , started = False
      , suspended = 0
      , diff = Nothing
      }, Cmd.none)
    Tick time ->
      let
        startTS =
          if model.startTS == 0
            then round time - model.suspended
            else model.startTS
        diff = Date.fromTime (time - toFloat startTS)
      in
        ({model | startTS = startTS, diff = Just(diff)}, Cmd.none)
    NoOp ->
       (model, Cmd.none)

-- -- -- -- -- -- -- --
--   Subsciptions    --
-- -- -- -- -- -- -- --

subscriptions : Model -> Sub ActionType
subscriptions model =
  if model.started == True then
    Sub.batch [
      AnimationFrame.times Tick
    , Keyboard.downs (onKeyPress model)
    ]
  else
    Keyboard.downs (onKeyPress model)

onKeyPress : Model -> Int -> ActionType
onKeyPress model =
  \keyCode ->
    case keyCode of
      32 ->
        if model.started == False
          then Start
          else Stop
      27 ->
        Reset
      _ ->
        NoOp

-- -- -- -- -- -- -- --
--       View        --
-- -- -- -- -- -- -- --

view : Model -> Html ActionType
view model =
  let
    diff = withDefault (Date.fromTime 82800000) model.diff
    h = round (toFloat (Date.millisecond diff)/10)
    hundredths = if h < 100 then h else 0
    minutes = Date.minute diff
    seconds = Date.second diff
  in
    div
      [
        classList [
          ("Chrono", True)
        , ("started", model.started)
        , ("stopped", not model.started && model.suspended > 0)
        ]
      ]
      [ h1 [] [
        text (pad minutes)
      , text ":"
      , text (pad seconds)
      , text ":"
      , text (pad hundredths)
      ]
      , div [class "buttons"]
        [ button [onClick Start] [text "START"]
        , button [onClick Stop] [text "STOP"]
        , button [onClick Reset] [text "RESET"]
        ]
      ]

pad : Int -> String
pad number =
  if number < 10 then "0" ++ toString number else toString number
