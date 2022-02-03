module Backend exposing (..)

import Base64
import Dict
import Env
import Lamdera exposing (ClientId, SessionId)
import OAuth.AuthorizationCode as OAuth
import Random
import Random.Char
import Random.String
import Types exposing (..)
import Url exposing (Protocol(..), Url)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> Lamdera.onConnect UserConnected
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { sessions = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        UserConnected sessionId _ ->
            let
                existingSession =
                    Dict.get sessionId model.sessions
            in
            ( { model
                | sessions =
                    case existingSession of
                        Just _ ->
                            model.sessions

                        Nothing ->
                            Dict.insert sessionId { state = Nothing } model.sessions
              }
            , Cmd.none
            )

        GotRandomState sessionId clientId url state ->
            let
                redirectUri : Url
                redirectUri =
                    { url | query = Nothing, fragment = Nothing }

                authorization : OAuth.Authorization
                authorization =
                    { clientId = Env.clientId
                    , redirectUri = redirectUri
                    , scope = [ "openid", "profile" ]
                    , state = Just state
                    , url =
                        { protocol = Https
                        , host = Env.authHost
                        , path = "/authorize"
                        , port_ = Nothing
                        , query = Nothing
                        , fragment = Nothing
                        }
                    }
            in
            ( { model
                | sessions =
                    Dict.update sessionId
                        (Maybe.map
                            (\session -> { session | state = Just state })
                        )
                        model.sessions
              }
            , Lamdera.sendToFrontend clientId (AuthRedirect (OAuth.makeAuthorizationUrl authorization))
            )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Authenticate url ->
            case OAuth.parseCode url of
                OAuth.Empty ->
                    ( model
                    , generateRandomState sessionId clientId url
                    )

                OAuth.Error error ->
                    ( model
                    , Lamdera.sendToFrontend clientId (AuthError (OAuthError error))
                    )

                OAuth.Success { code, state } ->
                    case Dict.get sessionId model.sessions |> Maybe.andThen .state of
                        Nothing ->
                            ( model
                            , Lamdera.sendToFrontend clientId (AuthError StateMismatch)
                            )

                        Just sessionState ->
                            if Just sessionState /= state then
                                ( model
                                , Lamdera.sendToFrontend clientId (AuthError StateMismatch)
                                )

                            else
                                ( model
                                , Lamdera.sendToFrontend clientId AuthSuccess
                                )


generateRandomState : SessionId -> ClientId -> Url -> Cmd BackendMsg
generateRandomState sessionId clientId url =
    Random.generate
        (GotRandomState sessionId clientId url)
        (Random.String.string 20 Random.Char.english |> Random.map Base64.encode)
