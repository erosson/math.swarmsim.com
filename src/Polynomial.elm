module Polynomial exposing (Polynomial, evaluate, evaluateTerms, format, formatCoefficients, formatTerms, toString)


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


{-| for example, "12345 + 321 + 1"
-}
formatTerms : (Float -> String) -> List Float -> String
formatTerms formatFloat =
    List.map formatFloat >> List.reverse >> String.join " + "


{-| just the coefficients for `format`. Useful for fancier rendering.
-}
formatCoefficients : (Float -> String) -> Polynomial -> List String
formatCoefficients numFormat =
    let
        format1 degree term =
            if term == 1 && degree /= 0 then
                ""

            else
                term |> numFormat
    in
    List.indexedMap format1


{-| for example, "3t^2 + 2t + 1"
-}
format : (Float -> String) -> Polynomial -> String
format numFormat =
    let
        degreeToString : Int -> String -> String
        degreeToString degree coeff =
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
            in
            coeff ++ pow
    in
    formatCoefficients numFormat
        >> List.indexedMap degreeToString
        >> List.reverse
        >> String.join " + "


toString : Polynomial -> String
toString =
    format String.fromFloat
