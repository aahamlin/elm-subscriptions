port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h2, p, text)
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
        , button [ onClick IncrementMsg ] [ text "Inc" ]
        , button [ onClick SendMsg ] [ text "Send" ]
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
            ( { model | val = inVal, msg = "decremented from port " }
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



-- valChanges : (Int -> msg) -> Sub msg
-- valChanges toMsg =
--     onValChange (\value -> toMsg value)


subscriptions : Model -> Sub Msg
subscriptions model =
    case Debug.log "subscription model" model of
        _ ->
            onValChange GotPortMsg
