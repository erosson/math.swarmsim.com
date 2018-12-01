module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Polynomial as Poly
import Production as Prod
import Round exposing (round)
import Task
import Time exposing (Posix)



---- MODEL ----


type Model
    = PendingModel
    | ReadyModel ModelData


type alias Duration =
    Int


type alias ModelData =
    { started : Posix
    , now : Posix
    , offset : Duration
    , paused : Bool
    , swarmCount : String
    , swarmProd : String
    }


elapsed : ModelData -> Duration
elapsed { started, now, offset, paused } =
    let
        diff =
            if paused then
                0

            else
                Time.posixToMillis now - Time.posixToMillis started
    in
    diff + offset


swarmCount : ModelData -> List Int
swarmCount =
    .swarmCount >> String.split "," >> List.map String.toInt >> List.filterMap identity >> List.reverse


swarmProd : ModelData -> List Float
swarmProd =
    .swarmProd >> String.split "," >> List.map String.toFloat >> List.filterMap identity >> List.reverse


swarmUnits : ModelData -> Prod.Units
swarmUnits model =
    Prod.Units (swarmCount model) (swarmProd model)


reify : ModelData -> List Float
reify model =
    model |> swarmUnits |> Prod.toPolynomials |> List.map (Poly.evaluate ((elapsed model |> toFloat) / 1000))


init : ( Model, Cmd Msg )
init =
    ( PendingModel, Time.now |> Task.perform Tick )


initReady : Posix -> ModelData
initReady t =
    { started = t
    , now = t
    , offset = 0
    , paused = False
    , swarmCount = "1,2,3"
    , swarmProd = "4,5"
    }



---- UPDATE ----


type Msg
    = Tick Posix
    | Pause
    | Resume
    | SetElapsed String
    | SetSwarmCount String
    | SetSwarmProd String
    | Reify


update : Msg -> Model -> ( Model, Cmd Msg )
update msg lmodel =
    case lmodel of
        PendingModel ->
            case msg of
                Tick t ->
                    ( initReady t |> ReadyModel, Cmd.none )

                _ ->
                    ( lmodel, Cmd.none )

        ReadyModel model ->
            case msg of
                Tick t ->
                    ( ReadyModel { model | now = t }, Cmd.none )

                Pause ->
                    ( ReadyModel { model | paused = True, offset = model |> elapsed }, Cmd.none )

                Resume ->
                    ( ReadyModel { model | paused = False, started = model.now }, Cmd.none )

                SetElapsed str ->
                    case str |> String.toFloat of
                        Nothing ->
                            ( ReadyModel { model | paused = True }, Cmd.none )

                        Just dur ->
                            ( ReadyModel { model | paused = True, offset = dur * 1000 |> floor }, Cmd.none )

                SetSwarmCount str ->
                    ( ReadyModel { model | swarmCount = str }, Cmd.none )

                SetSwarmProd str ->
                    ( ReadyModel { model | swarmProd = str }, Cmd.none )

                Reify ->
                    ( ReadyModel
                        { model
                            | started = model.now
                            , offset = 0
                            , swarmCount = model |> reify |> List.reverse |> List.map (floor >> String.fromInt) |> String.join ","
                        }
                    , Cmd.none
                    )


subs model =
    Sub.batch [ Browser.Events.onAnimationFrame Tick ]



---- VIEW ----


view : Model -> Html Msg
view lmodel =
    case lmodel of
        PendingModel ->
            div [] [ text "loading..." ]

        ReadyModel model ->
            let
                dur =
                    model |> elapsed

                units =
                    model |> swarmUnits

                unitDiff =
                    List.length units.count - (List.length units.prodEach + 1)

                unitDiffError =
                    if unitDiff > 0 then
                        String.fromInt unitDiff ++ " missing unit production - some unit counts will be ignored" |> Just

                    else if unitDiff < 0 then
                        String.fromInt -unitDiff ++ " missing unit count - some production values will be ignored" |> Just

                    else
                        Nothing
            in
            div []
                [ div []
                    [ h1 [] [ text "The math of ", a [ href "https://www.swarmsim.com" ] [ text "Swarm Simulator" ] ]
                    , p [] [ i [] [ text "2018/12/01: Please don't share this page just yet! I want to add some things before it's shared widely: better number formatting, better layout, visual breakdown of polynomials. Thanks for your patience." ] ]
                    , text "Time (seconds): "
                    , input [ type_ "number", onInput SetElapsed, value (model |> elapsed |> toFloat |> (*) (1 / 1000) |> round 3) ] []
                    , if model.paused then
                        button [ onClick Resume ] [ text "resume" ]

                      else
                        button [ onClick Pause ] [ text "pause" ]
                    ]

                --, viewPoly dur [ 1, 2, 3 ]
                --, viewPoly dur [ 1, 2, 3, 4 ]
                --, viewProd dur (Prod.Units [ 1, 2, 3, 4 ] [ 4, 5, 6 ])
                --, viewProd dur (Prod.Units [ 1 ] [])
                --, viewProd dur (Prod.Units [ 7, 3 ] [ 5 ])
                --, viewProd dur (Prod.Units [ 7, 3, 1 ] [ 5, 1 ])
                , div []
                    [ text "Unit counts: "
                    , input [ type_ "text", onInput SetSwarmCount, value model.swarmCount ] []
                    , button [ onClick Reify ] [ text "Reify: change count(0) to count(t)" ]
                    ]
                , div []
                    [ text " Production: "
                    , input [ type_ "text", onInput SetSwarmProd, value model.swarmProd ] []
                    ]
                , div [ class "error" ] [ unitDiffError |> Maybe.withDefault "" |> text ]
                , viewProd dur units
                ]


viewProd : Duration -> Prod.Units -> Html msg
viewProd dur units =
    table [ class "prod" ]
        [ thead []
            [ tr []
                [ th [] [ text "Unit" ]
                , th [] [ text "Count(0)" ]
                , th [] [ text "Production" ]
                , th [] [ text "Count(t)" ]
                , th [] [ text "Polynomial" ]
                , th [] [ text "Evaluation" ]
                ]
            ]
        , tbody []
            (List.map4 (viewProd1 dur)
                (units.count |> List.length |> List.range 0)
                units.count
                (0 :: units.prodEach)
                (units |> Prod.toPolynomials)
                |> List.reverse
            )
        ]


unitNames =
    [ "Meat", "Drone", "Queen", "Nest", "Greater Queen", "Hive", "Hive Queen", "Hive Empress" ]


renderUnitName : Int -> String
renderUnitName tier =
    unitNames |> List.drop tier |> List.head |> Maybe.withDefault ("Unit T" ++ String.fromInt tier)


viewProd1 : Duration -> Int -> Int -> Float -> Poly.Polynomial -> Html msg
viewProd1 t id count prod poly =
    [ id |> renderUnitName
    , count |> String.fromInt
    , if id == 0 then
        ""

      else
        round 0 prod ++ " " ++ renderUnitName (id - 1) ++ "/sec"
    , poly |> Poly.evaluate (toFloat t / 1000) |> floor |> String.fromInt
    , "f(t) = " ++ Poly.toString poly
    , formatPoly t poly
    ]
        |> List.map (\cell -> td [] [ text cell ])
        |> tr []


formatPoly : Duration -> Poly.Polynomial -> String
formatPoly dur0 poly =
    let
        dur =
            toFloat dur0 / 1000
    in
    "f("
        ++ round 2 dur
        ++ ") = "
        ++ (poly |> Poly.evaluate dur |> round 2)
        ++ " = "
        ++ (poly |> Poly.evaluateTerms dur |> Poly.termsToString)


viewPoly : Duration -> Poly.Polynomial -> Html msg
viewPoly dur0 poly =
    div []
        [ div [] [ "Polynomial: " |> text, poly |> Poly.toString |> text ]
        , div [] [ formatPoly dur0 poly |> text ]
        , hr [] []
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
