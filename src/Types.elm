module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera
import OAuth.AuthorizationCode as OAuth
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , status : String
    }


type alias BackendModel =
    { sessions : Dict Lamdera.SessionId Session }


type alias Session =
    { state : Maybe String }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg


type ToBackend
    = Authenticate Url


type BackendMsg
    = UserConnected Lamdera.SessionId Lamdera.ClientId
    | GotRandomState Lamdera.SessionId Lamdera.ClientId Url String


type ToFrontend
    = AuthRedirect Url
    | AuthError AuthError
    | AuthSuccess


type AuthError
    = OAuthError OAuth.AuthorizationError
    | StateMismatch
