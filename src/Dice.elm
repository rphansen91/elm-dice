module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Random


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }


type alias Model =
    { one : Int
    , two : Int
    }


type alias Die =
    ( Float, Float )


type Msg
    = Roll
    | NewFace ( Int, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, rand )

        NewFace ( one, two ) ->
            ( Model one two, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div [ class "table" ] []
        , div [ class "container" ]
            [ results model
            , div [ class "dice" ]
                [ dice model.one
                , dice model.two
                ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : ( Model, Cmd Msg )
init =
    ( Model 1 1, rand )


rand : Cmd Msg
rand =
    Random.generate NewFace (Random.map2 (,) (Random.int 1 6) (Random.int 1 6))


dice : Int -> Svg msg
dice face =
    svg [ viewBox "0 0 1800 1800" ]
        [ rect [ cx "0", cy "0", Svg.Attributes.width "1800px", Svg.Attributes.height "1800px", fill "#fff", stroke "#333", rx "3", ry "3" ] []
        , g [] (List.map (dot 300) (List.map (scalar 1800) (positions face)))
        ]


results : Model -> Html Msg
results model =
    div [ class "results" ]
        [ h1 [] [ Html.text ("Total: " ++ total model) ]
        , button [ onClick Roll ] [ Html.text "Roll" ]
        ]


positions : Int -> List Die
positions face =
    case face of
        1 ->
            [ dieCM ]

        2 ->
            [ dieTL, dieBR ]

        3 ->
            [ dieTL, dieCM, dieBR ]

        4 ->
            [ dieTL, dieTR, dieBL, dieBR ]

        5 ->
            [ dieTL, dieTR, dieCM, dieBL, dieBR ]

        6 ->
            [ dieTL, dieTR, dieCL, dieCR, dieBL, dieBR ]

        _ ->
            []


scalar : Float -> Die -> Die
scalar value ( x, y ) =
    ( value * x, value * y )


dot : Float -> Die -> Svg msg
dot size ( cx, cy ) =
    Svg.use [ x (toString (cx - size / 2)), y (toString (cy - size / 2)), xlinkHref "logo.svg#elm" ] []


total : Model -> String
total model =
    toString (model.one + model.two)


dieTL =
    ( 0.2, 0.2 )


dieTR =
    ( 0.8, 0.2 )


dieCL =
    ( 0.2, 0.5 )


dieCM =
    ( 0.5, 0.5 )


dieCR =
    ( 0.8, 0.5 )


dieBL =
    ( 0.2, 0.8 )


dieBR =
    ( 0.8, 0.8 )
