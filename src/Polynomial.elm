module Polynomial exposing (Polynomial, evaluate, evaluateTerms, termsToString, toString)

import Round exposing (round)


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


termsToString : List Float -> String
termsToString =
    List.map (round 2) >> List.reverse >> String.join " + "


toString : Polynomial -> String
toString =
    let
        degreeToString : Int -> Float -> String
        degreeToString degree term =
            let
                pow =
                    case degree of
                        0 ->
                            ""

                        1 ->
                            "t"

                        _ ->
                            "t^" ++ String.fromInt degree

                coeff =
                    if term == 1 && pow /= "" then
                        ""

                    else
                        String.fromFloat term
            in
            coeff ++ pow
    in
    List.indexedMap degreeToString
        >> List.reverse
        >> String.join " + "
