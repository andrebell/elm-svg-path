--SVG Playground - A progressive web app to create SVGs
--Copyright (C) 2020 André Alexander Bell <post@andre-bell.de>
--
--This program is free software: you can redistribute it and/or modify
--it under the terms of the GNU General Public License as published by
--the Free Software Foundation, either version 3 of the License, or
--(at your option) any later version.
--
--This program is distributed in the hope that it will be useful,
--but WITHOUT ANY WARRANTY; without even the implied warranty of
--MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--GNU General Public License for more details.
--
--You should have received a copy of the GNU General Public License
--along with this program.  If not, see <https://www.gnu.org/licenses/>.


module Main exposing (..)

import Browser
import Browser.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html as H exposing (Html)
import Html.Attributes as HA
import List
import Logo exposing (logo)
import Svg as S
import Svg.Attributes as SA
import Time



---- MODEL ----


type alias AnimationFrameMeta =
    { last : Time.Posix
    , current : Time.Posix
    , dt_ms : Int
    , fps : Float
    }


updateAnimationFrameMeta : Time.Posix -> Maybe AnimationFrameMeta -> Maybe AnimationFrameMeta
updateAnimationFrameMeta time afmeta_ =
    case afmeta_ of
        Nothing ->
            Just <| AnimationFrameMeta time time 0 0

        Just afmeta ->
            let
                last =
                    afmeta.current

                current =
                    time

                dt_ms =
                    Time.posixToMillis current - Time.posixToMillis last

                fps =
                    1000 / toFloat dt_ms
            in
            Just
                { afmeta
                    | last = last
                    , current = current
                    , dt_ms = dt_ms
                    , fps = fps
                }


type alias Model =
    { windowsettings : WindowSettings
    , afmeta : Maybe AnimationFrameMeta
    }


type alias Flags =
    { width : Int
    , height : Int
    }


type alias WindowSettings =
    { size : Flags
    , split : Float
    , resultWidth : Int
    , resultHeight : Int
    }



---- INIT ----


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        split =
            0.5

        resultWidth =
            round <| (split * toFloat (flags.width - 10))

        resultHeight =
            flags.height - 79
    in
    ( { windowsettings = WindowSettings flags split resultWidth resultHeight
      , afmeta = Nothing
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = BrowserResize ( Int, Int )
    | AnimationFrame Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BrowserResize ( w, h ) ->
            let
                size =
                    Flags w h

                resultWidth =
                    round <| (model.windowsettings.split * toFloat (w - 10))

                resultHeight =
                    h - 79
            in
            ( { model
                | windowsettings = WindowSettings (Flags w h) model.windowsettings.split resultWidth resultHeight
              }
            , Cmd.none
            )

        AnimationFrame time ->
            ( { model
                | afmeta = updateAnimationFrameMeta time model.afmeta
              }
            , Cmd.none
            )



---- UI SCHEME ----


uiColor =
    { black = rgb 0 0 0
    , white = rgb 1 1 1
    , red = rgb 1 0 0
    , green = rgb 0 1 0
    , blue = rgb 0 0 1
    , darkgrey = rgb255 29 30 34
    , midgrey = rgb255 68 72 87
    }


uiFont =
    { small = 12
    , normal = 16
    , big = 20
    }



---- VIEW ----


scale =
    modular 16 1.25


view : Model -> Html Msg
view model =
    let
        device =
            classifyDevice model.windowsettings.size
    in
    layout
        [ Background.color uiColor.darkgrey
        , Font.color uiColor.white
        , Font.size uiFont.normal
        ]
        (viewDesktopLayout model)



--(case device.class of
--    Phone ->
--        viewPhoneLayout model device.orientation
--    _ ->
--        viewDesktopLayout model
--)


viewResult : Model -> Element Msg
viewResult model =
    el
        [ Background.color uiColor.darkgrey
        , height fill
        , width <| px model.windowsettings.resultWidth
        ]
        (html <| viewSVG model)


viewSVG : Model -> Html Msg
viewSVG model =
    let
        resultHeight =
            model.windowsettings.size.height - 79
    in
    S.svg
        [ HA.style "background-color" "white"
        , HA.style "width" <| String.fromInt <| min model.windowsettings.resultWidth model.windowsettings.resultHeight
        , HA.style "margin" "auto"
        , SA.viewBox "0 0 1000 1000"
        ]
        [ S.rect
            [ SA.x "10"
            , SA.y "10"
            , SA.width "980"
            , SA.height "980"
            , SA.fill "none"
            , SA.stroke "black"
            , SA.strokeWidth "1"
            ]
            []
        , S.circle
            [ SA.cx "500"
            , SA.cy "500"
            , SA.r "480"
            , SA.fill "none"
            , SA.stroke "black"
            , SA.strokeWidth "1"
            ]
            []
        , S.rect
            [ SA.x "20"
            , SA.y "20"
            , SA.width "960"
            , SA.height "960"
            , SA.fill "blue"
            , SA.opacity "0.3"
            ]
            []
        , S.circle
            [ SA.cx "500"
            , SA.cy "500"
            , SA.r "450"
            , SA.fill "green"
            ]
            []
        ]



---- VIEW PHONE ----


viewPhoneLayout : Model -> Orientation -> Element Msg
viewPhoneLayout model orientation =
    column
        [ height fill, width fill, Background.color uiColor.white ]
        [ viewDesktopTitleBar model
        , row
            [ Background.color uiColor.green, width fill, height fill ]
            [ text <| "Sorry, the phone layout is not yet available" ]
        , viewDesktopStatusBar model
        ]



---- VIEW DESKTOP ----


viewHDivider : Int -> Element Msg
viewHDivider size =
    el
        [ Background.color uiColor.midgrey
        , height (px size)
        , width fill
        , padding 0
        , spacing 0
        ]
        none


viewVDivider : Int -> Element Msg
viewVDivider size =
    el
        [ Background.color uiColor.midgrey
        , height fill
        , width (px size)
        , padding 0
        , spacing 0
        ]
        none


viewDesktopLayout : Model -> Element Msg
viewDesktopLayout model =
    column
        [ height fill
        , width fill
        ]
        [ viewDesktopTitleBar model
        , viewHDivider 3
        , viewDesktopPlayground model
        , viewHDivider 3
        , viewDesktopStatusBar model
        ]


viewDesktopTitleBar : Model -> Element Msg
viewDesktopTitleBar model =
    row
        [ Background.color uiColor.darkgrey
        , Font.color uiColor.white
        , Font.size uiFont.big
        , width fill
        , spacing 10
        , padding 10
        ]
        [ el [] (html <| logo)

        --image
        --    [ Background.color uiColor.white
        --    , Border.rounded 5
        --    , height (px 30)
        --    ]
        --    { src = "/svg-playground/logo.svg", description = "Logo" }
        , text "Playground"
        ]


viewDesktopPlayground : Model -> Element Msg
viewDesktopPlayground model =
    row
        [ height fill
        , width fill
        ]
        [ viewResult model
        , viewVDivider 10
        , el
            [ Background.color uiColor.darkgrey
            , height fill
            , width fill
            ]
            (text "Right")
        ]


viewDesktopStatusBar : Model -> Element Msg
viewDesktopStatusBar model =
    wrappedRow
        [ Background.color uiColor.darkgrey
        , Border.widthEach
            { top = 0, left = 0, bottom = 1, right = 0 }
        , Border.color uiColor.midgrey
        , Font.light
        , Font.size uiFont.small
        , alignLeft
        , width fill
        ]
        [ --el
          --    [ padding 5
          --    ]
          --    (text <| Debug.toString <| classifyDevice model.windowsettings.size)
          --, el
          --    [ padding 5
          --    ]
          --    (text <| Debug.toString model.windowsettings.size)
          viewAnimationFrameMetaDT model.afmeta
        , viewAnimationFrameMetaFPS model.afmeta
        , el
            [ Border.widthEach { top = 0, left = 0, bottom = 0, right = 0 }
            , Border.color uiColor.midgrey
            , width fill
            , padding 5
            ]
            (if model.windowsettings.size.width > 1024 then
                link [ alignRight ] { url = "https://andre-bell.de/svg-playground", label = text "https://andre-bell.de/svg-playground © 2020 by André Alexander Bell" }

             else if model.windowsettings.size.width > 700 then
                link [ alignRight ] { url = "https://andre-bell.de/svg-playground", label = text "© 2020 by André Alexander Bell" }

             else
                link [ alignRight ] { url = "https://andre-bell.de/svg-playground", label = text "© 2020 by AB" }
            )
        ]


viewAnimationFrameMetaDT : Maybe AnimationFrameMeta -> Element Msg
viewAnimationFrameMetaDT afmeta_ =
    case afmeta_ of
        Nothing ->
            el [ padding 5 ] (text "DT: -")

        Just afmeta ->
            el [ padding 5 ]
                (text <| "DT: " ++ String.fromInt afmeta.dt_ms)


viewAnimationFrameMetaFPS : Maybe AnimationFrameMeta -> Element Msg
viewAnimationFrameMetaFPS afmeta_ =
    case afmeta_ of
        Nothing ->
            el [ padding 5 ] (text "FPS: -")

        Just afmeta ->
            el [ padding 5 ]
                (text <| "FPS: " ++ String.fromFloat (toFloat (round (afmeta.fps / 10) * 10)))



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onResize (\w h -> BrowserResize ( w, h ))
        , Browser.Events.onAnimationFrame (\time -> AnimationFrame time)

        --, Time.every dt Tick
        ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
