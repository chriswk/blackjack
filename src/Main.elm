module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)
import Random
import Components.Blackjack exposing (..)
import Task exposing (Task)
import Components.Deck exposing (shuffleDeck, newDeck)


---- MODEL ----


initialModel : Model
initialModel =
    { deck = newDeck
    , playerWins = 0
    , dealerWins = 0
    , dealerHitLimit = 17
    , draws = 0
    , gamesPlayed = 0
    , player = Player [] Playing
    , dealer = Player [] Playing
    , gameStatus = Waiting
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



---- UPDATE ----


send : Msg -> Cmd Msg
send m =
    Task.succeed m
        |> Task.perform identity


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            let
                generator =
                    shuffleDeck model.deck
            in
                ( model, Random.generate NewDeck generator )

        StartPlaying ->
            let
                model_ =
                    initialDeal model
            in
                ( { model_ | gameStatus = PlayerDrawing }, Cmd.none )

        Hit ->
            ( hit model, send CheckGame )

        Stand ->
            ( playDealer model, send CheckGame )

        NewDeck d ->
            ( { model | deck = d }, send StartPlaying )

        CheckGame ->
            ( Debug.log "Checking state" checkStatus model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        dealerS =
            "Dealer status: " ++ (toString model.dealer)

        playerS =
            "Player status: " ++ (toString model.player)

        gameState =
            "Game state: " ++ (toString model.gameStatus)
    in
        div [ class "line" ]
            [ gameTableHtml model
            , sidebarHtml model
            , div [ class "unit" ] [ text playerS ]
            , div [ class "unit" ] [ text dealerS ]
            , div [ class "unit" ] [ text gameState ]
            ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
