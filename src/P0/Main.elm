module P0.Main exposing (main)

import Animator
import Animator.Inline as Inline
import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import Html.Events as Events
import List.Extra as List
import Random
import Time exposing (Posix)



-- モデル


type alias Model =
    Animator.Timeline (List Int)


type Msg
    = Shuffle (List Int)


default : List Int
default =
    List.range 0 8


init : () -> ( Model, Cmd Msg )
init _ =
    ( Animator.init default
    , Random.uniform 1 [ -1, -3, 3 ]
        |> Random.list 24
        |> Random.generate Shuffle
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Shuffle nexts ->
            let
                stage =
                    model
                        |> Animator.current
                        |> shuffle 0 nexts
                        |> Debug.log "model"
            in
            ( Animator.go Animator.immediately stage model
            , Cmd.none
            )


shuffle : Int -> List Int -> List Int -> List Int
shuffle zero dirs nums =
    case dirs of
        [] ->
            nums

        x :: xs ->
            let
                next =
                    zero + x
            in
            if 0 <= next && next <= 8 then
                List.swapAt zero next nums
                    |> shuffle next xs

            else
                shuffle zero xs nums


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [] []
