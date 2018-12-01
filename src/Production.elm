module Production exposing (Units, toPolynomials)

import Polynomial exposing (Polynomial)


type alias Units =
    { count : List Int
    , prodEach : List Float
    }


toPolynomials : Units -> List Polynomial
toPolynomials { count, prodEach } =
    let
        toPolynomials_ c p =
            case c of
                [] ->
                    []

                c1 :: cs ->
                    toPolynomial c p :: toPolynomials_ cs (List.drop 1 p)
    in
    toPolynomials_ count prodEach


factorial : Int -> Int
factorial =
    let
        loop accum n =
            if n <= 1 then
                accum

            else
                loop (n * accum) (n - 1)
    in
    loop 1 >> Debug.log "fact"


toPolynomial : List Int -> List Float -> Polynomial
toPolynomial counts prodEach =
    {-
       For example, swarmsim units:

          queen = queenCount + nestProd * (nestCount / 1!)
          drone = droneCount + queenProd * (queenCount / 1! + nestProd * (nestCount / 2!))

          meat = meatCount + droneProd * (droneCount / 1! + queenProd * (queenCount / 2! + nestProd * (nestCount / 3!)))
               = meatCount + (droneProd * droneCount / 1!) + (droneProd * queenProd * queenCount / 2!) + (droneProd * queenProd * nestProd * nestCount / 3!)

       In the example above, last term:
       * `facts` is `nestCount / 3!`
       * `prods` is `droneProd * queenProd * nestProd`

    -}
    let
        mapFact degree count =
            toFloat count / toFloat (factorial degree)

        facts =
            counts |> List.indexedMap mapFact

        foldProd prod ( childProd, children ) =
            let
                p =
                    childProd * prod
            in
            ( p, p :: children )

        prods =
            prodEach |> List.foldl foldProd ( 1, [] ) |> Tuple.second |> (::) 1
    in
    List.map2 (*) facts prods
