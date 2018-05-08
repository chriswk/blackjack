module Components.Blackjack exposing (..)

import Components.Deck exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Hand =
    List Card


type Msg
    = NewGame
    | Hit
    | Stand
    | NewDeck Deck
    | CheckGame


type GameStatus
    = Waiting
    | PlayerDrawing
    | DealerDrawing
    | AllDone
    | Draw
    | PlayerWin
    | DealerWin


type PlayerStatus
    = Blackjack
    | Bust
    | Playing


type alias Player =
    { hand : List Card }


type alias Model =
    { deck : Deck
    , player : Player
    , dealer : Player
    , playerWins : Int
    , dealerWins : Int
    , draws : Int
    , gamesPlayed : Int
    , gameStatus : GameStatus
    , dealerHitLimit : Int
    }


cardToHtml : Card -> Html Msg
cardToHtml { suit, rank } =
    let
        suitClass =
            String.toLower (toString suit) ++ " unit"

        rankClass =
            String.toLower (toString rank)
    in
        div [ class suitClass ]
            [ div [ class rankClass ] []
            ]


winPercentage : Model -> String
winPercentage model =
    let
        winPct =
            (toFloat model.playerWins) / (toFloat (gamesPlayed model))
    in
        toString winPct


gameStatus : Model -> Html Msg
gameStatus model =
    let
        playerWins =
            "Player wins: " ++ (toString model.playerWins)

        gamesPlayed =
            "Games played: " ++ (toString model.gamesPlayed)

        dealerWins =
            "Dealer wins: " ++ (toString model.dealerWins)

        draws =
            "Draws: " ++ (toString model.draws)

        winPercentage_ =
            if model.gamesPlayed > 0 then
                winPercentage model
            else
                "No games played yet"
    in
        div [ class "line gameStatus" ]
            [ div [ class "line player" ] [ div [ class "unit" ] [ text playerWins ] ]
            , div [ class "line dealer" ] [ div [ class "unit" ] [ text dealerWins ] ]
            , div [ class "line draws" ] [ div [ class "unit" ] [ text draws ] ]
            , div [ class "line played" ] [ div [ class "unit" ] [ text gamesPlayed ] ]
            , div [ class "line wins" ] [ div [ class "unit" ] [ text winPercentage_ ] ]
            ]


cardsHtml : List Card -> List (Html Msg)
cardsHtml =
    List.map cardToHtml


dealerHtml : Model -> Html Msg
dealerHtml model =
    let
        cards =
            cardsHtml model.dealer.hand
    in
        div [ class "line dealer" ] cards


playerHtml : Model -> Html Msg
playerHtml model =
    let
        cards =
            cardsHtml model.player.hand
    in
        div [ class "line player" ]
            [ div [ class "cards" ]
                cards
            , gameButtons model
            ]


gameButtons : Model -> Html Msg
gameButtons model =
    let
        hitButton =
            case model.gameStatus of
                PlayerDrawing ->
                    button [ class "unit", onClick Hit ] [ text "Hit me!" ]

                DealerDrawing ->
                    button [ class "unit", disabled True ] [ text "Waiting on dealer" ]

                _ ->
                    button [ class "unit", onClick NewGame ] [ text "Start a new game" ]

        standButton =
            case model.gameStatus of
                PlayerDrawing ->
                    button [ class "unit", onClick Stand ] [ text "Stand" ]

                _ ->
                    button [ class "unit", disabled True ]
                        [ text "Stand" ]
    in
        div [ class "line" ]
            [ hitButton
            , standButton
            ]


gameTableHtml : Model -> Html Msg
gameTableHtml model =
    div [ id "game", class "unit r-size2of3" ]
        [ dealerHtml model
        , playerHtml model
        ]


sidebarHtml : Model -> Html Msg
sidebarHtml model =
    div [ id "sidebar", class "unit r-size1of3" ]
        [ button [ onClick NewGame ] [ text "New Game" ]
        , gameStatus model
        ]


newGame : Model -> Model
newGame model =
    let
        model_ =
            case model.gameStatus of
                DealerWin ->
                    { model | dealerWins = model.dealerWins + 1 }

                PlayerWin ->
                    { model | playerWins = model.playerWins + 1 }

                Waiting ->
                    model

                _ ->
                    { model | draws = model.draws + 1 }
    in
        { model_ | deck = newDeck, gameStatus = PlayerDrawing }


gamesPlayed : Model -> Int
gamesPlayed model =
    model.dealerWins + model.playerWins + model.draws


cardScorer : Card -> Int -> Int
cardScorer card sofar =
    let
        toAdd =
            case card.rank of
                Ace ->
                    if (sofar + 11 > 21) then
                        1
                    else
                        11

                _ ->
                    valueOfCard card
    in
        sofar + toAdd


scoreHand : Hand -> Int
scoreHand hand =
    List.sortBy valueOfCard hand
        |> List.foldl cardScorer 0


isBust : Hand -> Bool
isBust hand =
    scoreHand hand > 21


isBlackjack : Hand -> Bool
isBlackjack hand =
    case scoreHand hand of
        21 ->
            List.length hand == 2

        _ ->
            False


hit : Model -> Model
hit model =
    let
        p =
            model.player

        ( hand, deck ) =
            case model.deck of
                [] ->
                    ( p.hand, model.deck )

                [ h ] ->
                    ( h :: p.hand, [] )

                h :: t ->
                    ( h :: p.hand, t )

        updatedPlayer =
            { p | hand = hand }
    in
        { model | player = updatedPlayer, deck = deck }


drawWhile : Hand -> Deck -> Int -> ( Hand, Deck )
drawWhile hand deck limit =
    let
        ( hand_, deck_ ) =
            case deck of
                [] ->
                    ( hand, deck )

                [ h ] ->
                    ( h :: hand, [] )

                h :: t ->
                    ( h :: hand, t )

        score =
            scoreHand hand_

        wantAnother =
            score < limit && List.length deck_ > 0
    in
        if wantAnother then
            drawWhile hand_ deck_ limit
        else
            ( hand_, deck_ )


playDealer : Model -> Model
playDealer model =
    let
        d =
            model.dealer

        ( hand, deck ) =
            drawWhile d.hand model.deck model.dealerHitLimit

        d_ =
            { d | hand = hand }
    in
        { model | dealer = d_ }


type alias PlayerHands =
    ( Hand, Hand )


initialDeal : Model -> Model
initialDeal model =
    let
        newModel =
            case model.deck of
                h :: i :: j :: k :: rest ->
                    let
                        p =
                            model.player

                        d =
                            model.dealer

                        p_ =
                            { p | hand = [ h, j ] }

                        d_ =
                            { d | hand = [ i, k ] }
                    in
                        { model | player = p_, dealer = d_, deck = rest }

                _ ->
                    model
    in
        newModel


pickWinnerFromScore : Player -> Player -> GameStatus
pickWinnerFromScore player dealer =
    let
        pScore =
            scoreHand player.hand

        dScore =
            scoreHand dealer.hand
    in
        if pScore > dScore then
            PlayerWin
        else if pScore < dScore then
            DealerWin
        else
            Draw


checkStatus : Model -> Model
checkStatus model =
    let
        newStatus =
            case model.gameStatus of
                Waiting ->
                    Waiting

                PlayerDrawing ->
                    let
                        pStatus =
                            playerStatus model.player
                    in
                        case pStatus of
                            Bust ->
                                DealerWin

                            Blackjack ->
                                PlayerWin

                            _ ->
                                PlayerDrawing

                DealerDrawing ->
                    let
                        dStatus =
                            playerStatus model.dealer
                    in
                        case dStatus of
                            Bust ->
                                PlayerWin

                            Blackjack ->
                                DealerWin

                            _ ->
                                DealerDrawing

                AllDone ->
                    pickWinnerFromScore model.player model.dealer

                Draw ->
                    Draw

                PlayerWin ->
                    PlayerWin

                DealerWin ->
                    DealerWin
    in
        { model | gameStatus = newStatus }


playerStatus : Player -> PlayerStatus
playerStatus p =
    if isBlackjack p.hand then
        Blackjack
    else if isBust p.hand then
        Bust
    else
        Playing
