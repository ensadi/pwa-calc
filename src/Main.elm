module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import String exposing (contains, fromFloat)



-- model


type Msg
    = Dot
    | Number Float
    | Clear
    | Operation Op
    | Negative
    | Percent
    | Equal


type State
    = Init
    | Entry
    | Calculate


type Op
    = None
    | Add
    | Sub
    | Div
    | Mul


type alias Model =
    { acc : Float
    , display : String
    , state : State
    , operation : Op
    }



-- subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- update


update msg model =
    case msg of
        Dot ->
            ( addDot model, Cmd.none )

        Number num ->
            ( updateDisplay model num, Cmd.none )

        Clear ->
            ( initialModel, Cmd.none )

        Operation op ->
            let
                newmodel =
                    case model.state of
                        Init ->
                            calculate model

                        Entry ->
                            calculate model

                        Calculate ->
                            model
            in
            ( { newmodel | state = Calculate, operation = op }, Cmd.none )

        Negative ->
            ( negate model, Cmd.none )

        Percent ->
            ( calculatePercent model, Cmd.none )

        Equal ->
            let
                newmodel =
                    calculate model
            in
            ( { newmodel | acc = 0, state = Init, operation = None }, Cmd.none )


addDot : Model -> Model
addDot model =
    case model.state of
        Init ->
            { model | display = "0.", state = Entry }

        Calculate ->
            { model | display = "0.", state = Entry }

        Entry ->
            if contains "." model.display then
                model

            else
                { model | display = model.display ++ "." }


negate : Model -> Model
negate model =
    if Maybe.withDefault 0 (String.toFloat model.display) == 0 then
        model

    else if contains "-" model.display then
        { model | display = String.dropLeft 1 model.display }

    else
        { model | display = "-" ++ model.display }


updateDisplay : Model -> Float -> Model
updateDisplay model input =
    case model.state of
        Init ->
            { model | display = fromFloat input, state = Entry }

        Entry ->
            { model | display = model.display ++ fromFloat input }

        Calculate ->
            { model | display = fromFloat input, state = Entry }


getOp : Op -> (Float -> Float -> Float)
getOp op =
    case op of
        None ->
            \a b -> b

        Add ->
            \a b -> a + b

        Sub ->
            \a b -> a - b

        Div ->
            \a b -> a / b

        Mul ->
            \a b -> a * b


calculate : Model -> Model
calculate model =
    let
        opfunc =
            getOp model.operation

        newacc =
            opfunc model.acc (Maybe.withDefault 0 (String.toFloat model.display))

        newdisplay =
            fromFloat newacc
    in
    { model | acc = newacc, display = newdisplay }


calculatePercent : Model -> Model
calculatePercent model =
    case model.state of
        Init ->
            model

        Calculate ->
            model

        Entry ->
            let
                opfunc =
                    getOp model.operation

                percent =
                    Maybe.withDefault 0 (String.toFloat model.display) / 100

                newacc =
                    case model.operation of
                        None ->
                            percent

                        Add ->
                            opfunc model.acc (model.acc * percent)

                        Sub ->
                            opfunc model.acc (model.acc * percent)

                        Mul ->
                            opfunc model.acc percent

                        Div ->
                            opfunc model.acc percent

                newdisplay =
                    fromFloat newacc
            in
            { model | acc = newacc, display = newdisplay, state = Init, operation = None }



-- view


view model =
    div [ class "w3-container w3-auto" ]
        [ div
            [ class "w3-row w3-padding-24 w3-padding-large w3-gray w3-right-align"
            , style "font-size" "30px"
            , style "overflow" "hidden"
            ]
            [ text model.display ]
        , div [ class "w3-row" ]
            [ div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick Clear, class "w3-block w3-green w3-ripple button", style "font-size" "40px" ] [ text "C" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick Negative, class "w3-block w3-green w3-ripple button", style "font-size" "40px" ] [ text "+/-" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick Percent, class "w3-block w3-green w3-ripple button", style "font-size" "40px" ] [ text "%" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Operation Div), class "w3-block w3-deep-orange w3-ripple button", style "font-size" "40px" ] [ text "/" ] ]
            ]
        , div [ class "w3-row" ]
            [ div
                [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 7), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "7" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 8), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "8" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 9), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "9" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Operation Mul), class "w3-block w3-deep-orange w3-ripple button", style "font-size" "40px" ] [ text "x" ] ]
            ]
        , div [ class "w3-row" ]
            [ div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 4), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "4" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 5), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "5" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 6), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "6" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Operation Sub), class "w3-block w3-deep-orange w3-ripple button", style "font-size" "40px" ] [ text "-" ] ]
            ]
        , div [ class "w3-row" ]
            [ div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 1), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "1" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 2), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "2" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Number 3), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "3" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick (Operation Add), class "w3-block w3-deep-orange w3-ripple button", style "font-size" "40px" ] [ text "+" ] ]
            ]
        , div [ class "w3-row" ]
            [ div [ class "w3-col", style "width" "50%" ]
                [ button [ onClick (Number 0), class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "0" ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick Dot, class "w3-block w3-blue w3-ripple button", style "font-size" "40px" ] [ text "." ] ]
            , div [ class "w3-col", style "width" "25%" ]
                [ button [ onClick Equal, class "w3-block w3-purple w3-ripple button", style "font-size" "40px" ] [ text "=" ] ]
            ]
        ]



-- program main


initialModel : Model
initialModel =
    Model 0 "0" Init None


main =
    Browser.element { init = \() -> ( initialModel, Cmd.none ), update = update, view = view, subscriptions = subscriptions }
