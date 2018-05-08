module Components.Deck exposing (..)

import Maybe exposing (..)
import Random exposing (Generator, Seed)
import Array exposing (Array, toList, fromList)


type Suit
    = Diamonds
    | Hearts
    | Spades
    | Clubs


type Rank
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace


type alias Card =
    { suit : Suit
    , rank : Rank
    }


type alias Deck =
    List Card


suitFromString : String -> Maybe Suit
suitFromString suit =
    case suit of
        "Clubs" ->
            Just Clubs

        "C" ->
            Just Clubs

        "Diamonds" ->
            Just Diamonds

        "D" ->
            Just Diamonds

        "Hearts" ->
            Just Hearts

        "H" ->
            Just Hearts

        "Spades" ->
            Just Spades

        "S" ->
            Just Spades

        _ ->
            Nothing


rankFromNumber : Int -> Maybe Rank
rankFromNumber rank =
    case rank of
        1 ->
            Just Ace

        2 ->
            Just Two

        3 ->
            Just Three

        4 ->
            Just Four

        5 ->
            Just Five

        6 ->
            Just Six

        7 ->
            Just Seven

        8 ->
            Just Eight

        9 ->
            Just Nine

        10 ->
            Just Ten

        11 ->
            Just Jack

        12 ->
            Just Queen

        13 ->
            Just King

        _ ->
            Nothing


rankForSuit : Suit -> List Card
rankForSuit suit =
    let
        mapToRank : Maybe Rank -> Rank
        mapToRank =
            withDefault Ace

        ranks =
            List.map rankFromNumber (List.range 1 13)

        actualRanks =
            List.map mapToRank ranks
    in
        List.map (\r -> Card suit r) actualRanks


newDeck : Deck
newDeck =
    let
        suits =
            List.map suitFromString [ "C", "D", "S", "H" ]

        actualSuits =
            List.map (withDefault Clubs) suits

        deck =
            List.concatMap rankForSuit actualSuits
    in
        Debug.log "Deck: " deck


valueOfCard : Card -> Int
valueOfCard { suit, rank } =
    case rank of
        Two ->
            2

        Three ->
            3

        Four ->
            4

        Five ->
            5

        Six ->
            6

        Seven ->
            7

        Eight ->
            8

        Nine ->
            9

        Ace ->
            11

        _ ->
            10


constant : a -> Generator a
constant value =
    Random.map (\_ -> value) Random.bool


choose : Array a -> Generator ( Maybe a, Array a )
choose arr =
    if Array.isEmpty arr then
        constant ( Nothing, arr )
    else
        let
            lastIndex =
                Array.length arr - 1

            front i =
                Array.slice 0 i arr

            back i =
                if i == lastIndex then
                    Array.empty
                else
                    Array.slice (i + 1) (lastIndex + 1) arr

            gen =
                Random.int 0 lastIndex
        in
            Random.map
                (\index ->
                    ( Array.get index arr, Array.append (front index) (back index) )
                )
                gen


shuffleDeck : Deck -> Generator Deck
shuffleDeck deck =
    let
        arrDeck =
            fromList deck
    in
        shuffle arrDeck
            |> Random.map toList


shuffle : Array a -> Generator (Array a)
shuffle arr =
    if Array.isEmpty arr then
        constant arr
    else
        let
            helper ( done, remaining ) =
                choose remaining
                    |> Random.andThen
                        (\( m_val, shorter ) ->
                            case m_val of
                                Nothing ->
                                    constant ( done, shorter )

                                Just val ->
                                    helper ( val :: done, shorter )
                        )
        in
            Random.map (Tuple.first >> Array.fromList) (helper ( [], arr ))
