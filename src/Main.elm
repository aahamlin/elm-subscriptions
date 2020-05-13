port module Main exposing (main)

import Browser
import Html exposing (Html, br, button, div, h1, h2, label, p, text)
import Html.Attributes exposing (for, id)
import Html.Events exposing (onClick)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { msg : String
    , val : Int
    }


type Msg
    = GotPortMsg Int
    | SendMsg
    | IncrementMsg


init : () -> ( Model, Cmd Msg )
init _ =
    ( { msg = "Initial msg"
      , val = 0
      }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Subscriptions" ]
        , p [] [ text model.msg ]
        , h2 [] [ text (String.fromInt model.val) ]
        , label [ for "inc-btn" ] [ text "Increase from UI" ]
        , button [ id "inc-btn", onClick IncrementMsg ] [ text "Increment" ]
        , br [] []
        , label [ for "dec-btn" ] [ text "Decrease from JavaScript" ]
        , button [ id "dec-btn", onClick SendMsg ] [ text "Decrement" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        newVal =
            model.val + 1
    in
    case Debug.log "update msg" msg of
        IncrementMsg ->
            ( { model | val = newVal, msg = "incremented" }
            , Cmd.none
            )

        SendMsg ->
            ( model
            , sendUpdateVal model.val
            )

        GotPortMsg inVal ->
            ( { model | val = inVal, msg = "decremented" }
            , Cmd.none
            )


sendUpdateVal : Int -> Cmd msg
sendUpdateVal i =
    let
        log =
            Debug.log "sendupdateval" i
    in
    storeVal i


port storeVal : Int -> Cmd msg


port onValChange : (Int -> msg) -> Sub msg


valChanges : (Int -> msg) -> Sub msg
valChanges toMsg =
    onValChange (\value -> toMsg value)


subscriptions : Model -> Sub Msg
subscriptions model =
    case Debug.log "subscription model" model of
        _ ->
            valChanges GotPortMsg
