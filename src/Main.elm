module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class)
import Random exposing (initialSeed)
import Components.Blackjack exposing (..)
import Task exposing (Task)
import Components.Deck exposing (shuffleDeck)


---- MODEL ----


initialModel : Model
initialModel =
    { deck = []
    , playerWins = 0
    , dealerWins = 0
    , dealerHitLimit = 17
    , draws = 0
    , gamesPlayed = 0
    , player = Player []
    , dealer = Player []
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

        Hit ->
            ( hit model, send CheckGame )

        Stand ->
            ( playDealer model, send CheckGame )

        NewDeck d ->
            ( { model | deck = d }, Cmd.none )

        CheckGame ->
            ( checkStatus model, Cmd.none )



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
            , div [ class "unit" ] [ text (toString model) ]
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
