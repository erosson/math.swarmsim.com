module Polynomial exposing (Polynomial, evaluate, evaluateTerms, format, formatTerms, toString)


type alias Polynomial =
    List Float


evaluate : Float -> Polynomial -> Float
evaluate t =
    evaluateTerms t >> List.sum


evaluateTerms : Float -> Polynomial -> List Float
evaluateTerms t =
    let
        eval1 degree term =
            term * t ^ toFloat degree
    in
    List.indexedMap eval1


formatTerms : (Float -> String) -> List Float -> String
formatTerms formatFloat =
    List.map formatFloat >> List.reverse >> String.join " + "


format : (Float -> String) -> Polynomial -> String
format numFormat =
    let
        degreeToString : Int -> Float -> String
        degreeToString degree term =
            let
                pow =
                    case degree of
                        0 ->
                            ""

                        1 ->
                            " t"

                        _ ->
                            -- " t^" ++ (degree |> toFloat |> numFormat)
                            " t^" ++ String.fromInt degree

                coeff =
                    if term == 1 && pow /= "" then
                        ""

                    else
                        term |> numFormat
            in
            coeff ++ pow
    in
    List.indexedMap degreeToString
        >> List.reverse
        >> String.join " + "


toString : Polynomial -> String
toString =
    format String.fromFloat
