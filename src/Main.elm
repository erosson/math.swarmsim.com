module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Events
import FormatNumber
import FormatNumber.Locales
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import NumberSuffix exposing (standardConfig)
import Polynomial as Poly
import Production as Prod
import Round
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
    , uiTime : Maybe String
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
    , uiTime = Nothing
    , swarmCount = "1000,2,3"
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
                    ( ReadyModel { model | paused = True, offset = model |> elapsed, uiTime = Just ((model |> elapsed |> toFloat) / 1000 |> Round.round 3) }, Cmd.none )

                Resume ->
                    ( ReadyModel { model | paused = False, started = model.now, uiTime = Nothing }, Cmd.none )

                SetElapsed str ->
                    case str |> String.toFloat of
                        Nothing ->
                            ( ReadyModel { model | paused = True, uiTime = Just str }, Cmd.none )

                        Just dur ->
                            ( ReadyModel { model | paused = True, uiTime = Just str, offset = dur * 1000 |> floor }, Cmd.none )

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
                        formatInt unitDiff ++ " missing unit production - some unit counts will be ignored" |> Just

                    else if unitDiff < 0 then
                        formatInt -unitDiff ++ " missing unit count - some production values will be ignored" |> Just

                    else
                        Nothing
            in
            div []
                [ div []
                    [ h1 [] [ text "The math of ", a [ href "https://www.swarmsim.com" ] [ text "Swarm Simulator" ] ]
                    , text "Time (seconds): "
                    , input [ type_ "number", onInput SetElapsed, value (model.uiTime |> Maybe.withDefault (model |> elapsed |> toFloat |> (*) (1 / 1000) |> Round.round 3)) ] []
                    , if model.paused then
                        button [ onClick Resume ] [ text "resume" ]

                      else
                        button [ onClick Pause ] [ text "pause" ]
                    ]

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
    unitNames |> List.drop tier |> List.head |> Maybe.withDefault ("Unit Tier " ++ formatInt tier)


viewProd1 : Duration -> Int -> Int -> Float -> Poly.Polynomial -> Html msg
viewProd1 t id count prod poly =
    let
        textCell =
            text >> List.singleton
    in
    [ id |> renderUnitName |> textCell
    , count |> formatInt |> textCell
    , (if id == 0 then
        ""

       else
        formatFloat 2 prod ++ " " ++ renderUnitName (id - 1) ++ "/sec"
      )
        |> textCell
    , poly |> Poly.evaluate (toFloat t / 1000) |> floor |> formatInt |> textCell

    -- , "f(t) = " ++ Poly.format (formatFloat 2) poly |> textCell
    , text "f(t) = " :: viewPoly poly
    , formatPoly t poly |> textCell
    ]
        |> List.map (td [])
        |> tr []


viewPoly : Poly.Polynomial -> List (Html msg)
viewPoly =
    let
        render : Int -> String -> Html msg
        render degree coeff =
            span []
                (text coeff
                    :: (case degree of
                            0 ->
                                [ text "" ]

                            1 ->
                                [ text " t" ]

                            _ ->
                                [ text " t", sup [] [ text <| String.fromInt degree ] ]
                       )
                )
    in
    Poly.formatCoefficients (formatFloat 3) >> List.indexedMap render >> List.reverse >> List.intersperse (text " + ")


formatFloat : Int -> Float -> String
formatFloat sigfigs =
    NumberSuffix.format
        { standardConfig
            | sigfigs = sigfigs
            , getSuffix = NumberSuffix.suffixStandardShort
        }


formatInt : Int -> String
formatInt =
    toFloat >> formatFloat 5


formatPoly : Duration -> Poly.Polynomial -> String
formatPoly dur0 poly =
    let
        dur =
            toFloat dur0 / 1000
    in
    "f("
        -- ++ formatFloat 0 dur
        ++ FormatNumber.format FormatNumber.Locales.usLocale dur
        ++ ") = "
        ++ (poly |> Poly.evaluateTerms dur |> Poly.formatTerms (formatFloat 3))



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subs
        }
