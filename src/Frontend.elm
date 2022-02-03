module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Lamdera
import OAuth
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , status = ""
      }
    , Lamdera.sendToBackend (Authenticate url)
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged _ ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        AuthRedirect url ->
            ( model
            , Nav.load (Url.toString url)
            )

        AuthError error ->
            let
                errorMessage =
                    case error of
                        OAuthError err ->
                            err.errorDescription |> Maybe.withDefault (OAuth.errorCodeToString err.error)

                        StateMismatch ->
                            "State mismatch"
            in
            ( { model | status = "Error - " ++ errorMessage }
            , Cmd.none
            )

        AuthSuccess ->
            ( { model | status = "Login success" }
            , Cmd.none
            )


view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text ("Status: " ++ model.status) ]
            ]
        ]
    }
