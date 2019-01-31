port module Main exposing (Model, Msg(..), add1, init, main, toJs, update, view)

--import Html.Attributes exposing (..)
--import Html.Events exposing (..)

import Browser
import Browser.Navigation as Nav
import Color as C exposing (black, blue, brown, charcoal, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, gray, green, grey, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, orange, purple, red, white, yellow)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html
import Http exposing (Error(..))
import Json.Decode as Decode
import Material.Icons.Action exposing (account_balance)
import Material.Icons.Content exposing (add_circle, archive, content_copy, redo, remove_circle, unarchive, undo)
import Material.Icons.Navigation exposing (menu)



-- ---------------------------
-- PORTS
-- ---------------------------


port toJs : String -> Cmd msg



-- ---------------------------
-- MODEL
-- ---------------------------


type alias Model =
    { counter : Int
    , serverMessage : String
    , screen :
        { width : Int
        , height : Int
        }
    }


type alias Flags =
    { counter : Int
    , width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { counter = flags.counter
      , serverMessage = ""
      , screen =
            { width = flags.width
            , height = flags.height
            }
      }
    , Cmd.none
    )



-- ---------------------------
-- UPDATE
-- ---------------------------


type Msg
    = Inc
    | Set Int
    | TestServer
    | OnServerResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Inc ->
            ( add1 model, toJs "Hello Js" )

        Set m ->
            ( { model | counter = m }, toJs "Hello Js" )

        TestServer ->
            let
                expect =
                    Http.expectJson OnServerResponse (Decode.field "result" Decode.string)
            in
            ( model
            , Http.get { url = "/test", expect = expect }
            )

        OnServerResponse res ->
            case res of
                Ok r ->
                    ( { model | serverMessage = r }, Cmd.none )

                Err err ->
                    ( { model | serverMessage = "Error: " ++ httpErrorToString err }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        BadUrl _ ->
            "BadUrl"

        Timeout ->
            "Timeout"

        NetworkError ->
            "NetworkError"

        BadStatus _ ->
            "BadStatus"

        BadBody s ->
            "BadBody: " ++ s


{-| increments the counter

    add1 5 --> 6

-}
add1 : Model -> Model
add1 model =
    { model | counter = model.counter + 1 }



-- ---------------------------
-- VIEW
-- ---------------------------


view : Model -> Html.Html Msg
view model =
    Element.layout [ Font.size 20, Background.color (uiColor lightGray) ]
        (Element.column [ explain Debug.todo ]
            [ heading model
            , myColumnOfStuff
            , Element.row []
                [ incrementButton
                , Element.text <| String.fromInt model.counter
                ]
            ]
        )


uiColor : C.Color -> Element.Color
uiColor color =
    color |> C.toRgba |> Element.fromRgb


heading : Model -> Element msg
heading model =
    Element.row [ Region.heading 1 ]
        [ el [] (html (menu red 36))
        , el
            [ alignLeft
            , Font.size 24
            ]
            (Element.text "Raison D'être Elm 0.19 Webpack Starter, with hot-reloading")
        ]


footer : Model -> Element msg
footer model =
    Element.row [] []



-- left sidebar
-- content box
-- navigation tabs
-- cards
-- breadcrumbs


sidebar : Element Msg
sidebar =
    Element.column [ width (px 70), height shrink, centerY, centerX, spacing 36, padding 10 ]
        [ incrementButton
        ]


incrementButton : Element Msg
incrementButton =
    Input.button []
        { onPress = Just Inc
        , label = Element.text "+1"
        }


myColumnOfStuff : Element Msg
myColumnOfStuff =
    Element.column [ width shrink, height shrink, centerY, centerX, spacing 36, padding 10, explain Debug.todo ]
        [ myRowOfStuff
        ]


myRowOfStuff : Element Msg
myRowOfStuff =
    Element.row [ width fill, centerY, padding 10, spacing 30, clip ]
        [ myElement
        , myTable
        , image [ width (px 70) ] { description = "logo", src = "/images/logo.png" }
        , el [] (html (account_balance orange 48))
        , el [ alignRight ] myElement
        ]


myElement : Element Msg
myElement =
    el
        [ Background.color (uiColor purple)
        , Font.color (uiColor white)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")


type alias Person =
    { firstName : String
    , lastName : String
    }


persons : List Person
persons =
    [ { firstName = "David"
      , lastName = "Bowie"
      }
    , { firstName = "Florence"
      , lastName = "Welch"
      }
    ]


myTable =
    Element.table []
        { data = persons
        , columns =
            [ { header = Element.text "First Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.firstName
              }
            , { header = Element.text "Last Name"
              , width = fill
              , view =
                    \person ->
                        Element.text person.lastName
              }
            ]
        }



-- ---------------------------
-- MAIN
-- ---------------------------


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                { title = "Elm 0.19 starter Raison D'être"
                , body = [ view m ]
                }
        , subscriptions = \_ -> Sub.none
        }
