import Html exposing (Html, div, text, fieldset, label, input, span, h1)
import Html.Events exposing (onInput, targetValue)
import Html.Attributes exposing (type_, name, checked, id)

import Array
import Browser
import List
import Time
import Random

-- Types
type Msg =
    Reset State
    | Tick Time.Posix

type GameResult =
    Win
    | Lose
    | Ignore

type State = Standard | RandomVariant

type alias Model =
    {
        state: State,
        wins: Int,
        total: Int,
        logs: List GameLog,
        seed: Random.Seed
    }

type alias GameLog =
    {
        carDoor: Int,
        playerChooseDoor: Int,
        hostShowDoor: Int,
        standard: Bool,
        win: GameResult
    }


init : () -> (Model, Cmd Msg)
init _ =
    ({
        state = Standard,
        wins = 0,
        total = 0,
        logs = [],
        seed = Random.initialSeed 0
    }, Cmd.none)

-- View

logToView log =
    let bold = \x -> Html.b [] [text x]
        car = [text "Car is in Door ", bold (Debug.toString log.carDoor)]
        player = [text ", Player chooses Door ", bold (Debug.toString log.playerChooseDoor)]
        host = [text ", Host shows Door ", bold (Debug.toString log.hostShowDoor)]

        resultText = case log.win of
            Win ->
                [text ", Player swaps and ", bold "WINS!!"]
            Lose ->
                [text ", Player swaps and ", bold "LOSES!"]
            Ignore ->
                [text ", Host accidentally shows the car! This case is ignored."]
    in
        div [] (car ++ player ++ host ++ resultText)


labelToView model state s =
    label [] [
            input [ type_ "radio", name "choice", checked (model.state == state), onInput (\x -> Reset state) ] [],
            text s
        ]

view : Model -> Html Msg
view model =
    div [] [
        h1 [] [text "Monty Hall Problem Simulation"],

        fieldset [] [
            labelToView model Standard "Host always shows the goat",
            labelToView model RandomVariant "Host shows randomly"
        ],

        div [id "status"] [
            span [] [text ("Wins: " ++ (Debug.toString model.wins))],
            span [] [text ("Total: " ++ (Debug.toString model.total))],
            span [] [text ("Win %: " ++ (toPercent model.wins model.total))]
            ],

        div [id "log"] (List.map logToView model.logs)


    ]

toPercent a b =
    let s = (Debug.toString (100 * (toFloat a) / (toFloat b)))
    in
        if not (String.contains "." s) then
            s ++ ".00"
        else
            String.slice 0 5 (s ++ "00")


getRanInt model max =
    let (ranNum, newSeed) = Random.step (Random.int 1 max) model.seed
    in
        ({model | seed = newSeed}, ranNum)


getIndex i list =
    let arr = Array.fromList list
    in
        Maybe.withDefault -1 (Array.get i arr)

getRanFromList model list =
    let (model2, ranNum) = getRanInt model (List.length list)
    in
        (model2, (getIndex (ranNum - 1) list))

getRanIntExcept model i =
    case i of
        1 -> getRanFromList model [2, 3]
        2 -> getRanFromList model [1, 3]
        3 -> getRanFromList model [1, 2]
        _ -> (model, -1)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Reset state ->
            let (newModel, _) = init ()
            in
                ({newModel | state=state}, Cmd.none)
        Tick t ->
            (playGame model, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
    [
    Time.every 500 Tick
    ]


addGameLog : Model -> Int -> Int -> Int -> GameResult -> Model
addGameLog model cDoor pDoor hDoor win =
    let gameLog = {
            carDoor = cDoor,
            playerChooseDoor = pDoor,
            hostShowDoor = hDoor,
            win = win,
            standard = (model.state == Standard) }

        logs = List.take 100 (gameLog :: model.logs) -- limit to 100
    in
        { model | logs = logs }


playGame : Model -> Model
playGame model =
    -- 1. host places car in random door
    -- 2. player chooses door
    -- 3. host shows goat(?)
    -- 4. swap
    let (model2, carDoor) = getRanInt model 3
        (model3, playerChooseDoor) = getRanInt model2 3
        carChosen = (carDoor == playerChooseDoor)
    in
        -- Player initially chooses Car door, LOSE
        if carChosen then
            let (model4, hostShowDoor) = getRanIntExcept model3 playerChooseDoor
                newModel = addGameLog model4 carDoor playerChooseDoor hostShowDoor Lose
            in
                -- SWAP LOSES
                { newModel | total = newModel.total + 1 }

        -- Player initially chooses Goat door, WIN
        else if model.state == Standard then
            let getRemainingDoor = \x -> x /= playerChooseDoor && x /= carDoor
                hostShowDoor = [1, 2, 3]
                                    |> List.filter getRemainingDoor
                                    |> List.head
                                    |> Maybe.withDefault -1
                newModel = addGameLog model3 carDoor playerChooseDoor hostShowDoor Win
            in
                -- SWAP WINS
                { newModel | wins = newModel.wins + 1, total = newModel.total + 1 }

        -- Variant: Player initially chooses Goat door and host shows random door
        else
            let (model4, hostRandomShowDoor) = getRanIntExcept model3 playerChooseDoor
                hostCarChosen = (carDoor == hostRandomShowDoor)
                gameResult = if hostCarChosen then Ignore else Win
                newModel = addGameLog model4 carDoor playerChooseDoor hostRandomShowDoor gameResult
            in
                if hostCarChosen then
                    -- HOST ACCIDENTALLY SHOWS CAR
                    newModel
                else
                    -- HOST ACCIDENTALLY SHOWS GOAT
                    -- SWAP WINS
                    {newModel | wins = newModel.wins + 1, total = newModel.total + 1}




main =
    Browser.element {
        init = init,
        update = update,
        view = view,
        subscriptions = subscriptions
    }
