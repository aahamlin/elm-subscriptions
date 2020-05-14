port module Main exposing (main)

import Browser
import Html exposing (Html, a, br, button, div, h1, h2, label, p, text)
import Html.Attributes exposing (for, href, id)
import Html.Events exposing (onClick)


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Count CountModel
    | Timer RecvModel


type alias CountModel =
    { msg : String
    , val : Int
    }


type alias RecvModel =
    { val : Int
    , count : CountModel
    }


type Msg
    = GotPortMsg Int
    | Recv Int
    | SendMsg
    | IncrementMsg
    | SwitchView


init : () -> ( Model, Cmd Msg )
init _ =
    ( Count
        { msg = "Initial msg"
        , val = 0
        }
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    case model of
        Count m ->
            div []
                [ h1 [] [ text "Subscriptions" ]
                , h2 [] [ text (String.fromInt m.val) ]
                , label [ for "inc-btn" ] [ text "Increase from UI" ]
                , button [ id "inc-btn", onClick IncrementMsg ] [ text "Increment" ]
                , br [] []
                , label [ for "dec-btn" ] [ text "Decrease from JavaScript" ]
                , button [ id "dec-btn", onClick SendMsg ] [ text "Decrement" ]
                , p [] [ text m.msg ]
                , a [ href "#", onClick SwitchView ] [ text "Switch view" ]
                ]

        Timer m ->
            div []
                [ h1 [] [ text "Javascript timer" ]
                , p [] [ text "Every 3 seconds, Recv str will be called from JS" ]
                , h2 [] [ text (String.fromInt m.val) ]
                , a [ href "#", onClick SwitchView ] [ text "Switch view" ]
                ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "update msg" ( msg, model ) of
        ( IncrementMsg, Count m ) ->
            ( Count { m | val = m.val + 1, msg = "incremented" }
            , Cmd.none
            )

        ( SendMsg, Count m ) ->
            ( Count m
            , sendUpdateVal m.val
            )

        ( GotPortMsg inVal, Count m ) ->
            ( Count { m | val = inVal, msg = "decremented" }
            , Cmd.none
            )

        ( Recv n, Timer m ) ->
            ( Timer { m | val = n }
            , Cmd.none
            )

        ( SwitchView, Timer m ) ->
            ( Count m.count
            , Cmd.none
            )

        ( SwitchView, Count m ) ->
            ( Timer { val = 0, count = m }
            , Cmd.none
            )

        ( _, _ ) ->
            ( model, Cmd.none )


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
subscriptions model =
    -- Sub.batch
    --     [ valChanges GotPortMsg
    --     , onMsgChange Recv
    --     ]
    case Debug.log "subscription model" model of
        Count _ ->
            valChanges GotPortMsg

        Timer _ ->
            onMsgChange Recv
