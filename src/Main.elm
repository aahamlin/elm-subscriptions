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
    | Recv Int
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
        , h2 [] [ text "Every 3 seconds, Recv str will be called from JS" ]
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
    case Debug.log "update msg" msg of
        IncrementMsg ->
            let
                newVal =
                    model.val + 1
            in
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

        Recv n ->
            ( { model | msg = String.fromInt n }
            , Cmd.none
            )


sendUpdateVal : Int -> Cmd msg
sendUpdateVal i =
    storeVal i


port storeVal : Int -> Cmd msg


port onValChange : (Int -> msg) -> Sub msg


port onMsgChange : (Int -> msg) -> Sub msg


valChanges : (Int -> msg) -> Sub msg
valChanges toMsg =
    onValChange (\value -> toMsg value)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ valChanges GotPortMsg
        , onMsgChange Recv
        ]



-- case Debug.log "subscription model" model of
--     _ ->
--         valChanges GotPortMsg
