module P3.Main exposing (main)

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
    | Tick Posix
    | Click Int


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



-- Model更新用関数(ほぼテンプレート)


animator : Animator.Animator Model
animator =
    Animator.animator
        |> Animator.watching identity always


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

        Tick posix ->
            ( Animator.update posix animator model, Cmd.none )

        Click n ->
            let
                clicked =
                    List.elemIndex n (Animator.current model)
                        |> Maybe.withDefault -1

                blank =
                    List.elemIndex 0 (Animator.current model)
                        |> Maybe.withDefault -1

                distance =
                    abs (clicked - blank)

                next =
                    if distance == 1 || distance == 3 then
                        List.swapAt clicked blank (Animator.current model)

                    else
                        Animator.current model
            in
            ( Animator.go Animator.slowly next model
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



-- subscriptionで一定時間ごとにTickを呼び出すようにする


subscriptions : Model -> Sub Msg
subscriptions model =
    Animator.toSubscription Tick model animator


view : Model -> Html Msg
view model =
    div
        [ style "width" "300px"
        , style "margin" "auto"
        , style "padding" "100px 0"
        ]
        [ div
            [ style "height" "64px"
            , style "text-align" "center"
            , style "font-size" "32px"
            ]
            [ if Animator.current model == default then
                text "SOLVED!"

              else
                text ""
            ]
        , div [] (List.map (cell model) (Animator.current model))
        ]


cell : Animator.Timeline (List Int) -> Int -> Html Msg
cell timeline value =
    case value of
        0 ->
            div [] []

        _ ->
            div
                [ style "position" "absolute"
                , Events.onClick (Click value)
                , Inline.xy timeline <|
                    \xs ->
                        let
                            i =
                                List.elemIndex value xs
                                    |> Maybe.withDefault 0
                        in
                        { x = Animator.at (toFloat (100 * remainderBy 3 i))
                        , y = Animator.at (toFloat (100 * (i // 3)))
                        }
                , style "background-color" "orange"
                , style "width" "99px"
                , style "height" "99px"
                ]
                [ text (String.fromInt value) ]
