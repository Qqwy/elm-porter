port module Example exposing (..)

import Porter
import Html exposing (Html, text)
import Json.Encode as Encode
import Json.Decode as Decode


-- Configure Porter


port outgoing : Encode.Value -> Cmd msg


port incoming : (Decode.Value -> msg) -> Sub msg


porterConfig : Porter.Config String String Msg
porterConfig =
    { outgoingPort = outgoing
    , incomingPort = incoming

    -- Porter works with a single Request and Response data types. They can both be anything, as long as you supply decoders :)
    , encodeRequest = Encode.string
    , decodeResponse = Decode.string

    -- Porter uses a message added to your Msg type for its internal communications (See `type Msg` below)
    , porterMsg = PorterMsg
    }



-- Application model


type alias Model =
    { porter : Porter.Model String String Msg
    , response : String
    , advancedResponse : String
    , mapResponse : String
    }


init : ( Model, Cmd Msg )
init =
    ( { porter = Porter.init
      , response = ""
      , advancedResponse = ""
      , mapResponse = ""
      }
    , Cmd.batch
        [ -- Send a request through porter, specifying the response handler directly
          -- Porter.send porterConfig Receive (Porter.request "Reverse me!")

        -- Or send multiple requests one after the other:
        -- , Porter.request ("Reverse me too!")
        --     |> Porter.andThen (\reversedStr -> Porter.request (reversedStr ++ " The Quick Brown Fox!"))
        --     |> Porter.andThen (\reversedStr -> Porter.request (reversedStr ++ " A man a plan a canal: panama"))
        --     |> Porter.map (\str -> String.repeat 2 str)
        --     |> Porter.send porterConfig ReceiveAdvanced
         combinedRequest
          |> Porter.send porterConfig ReceiveMapped
        ]
    )

req1 = Porter.request "Foo"
req2 = Porter.request "Bar"
combinedRequest = Porter.map2 (\a b -> a ++ " | " ++ b) req2 req1

-- Message includes Porter's message


type Msg
    = PorterMsg (Porter.Msg String String Msg)
    | Receive String
    | ReceiveAdvanced String
    | ReceiveMapped String



-- Update porter accordingly


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PorterMsg porterMsg ->
            let
                ( porterModel, porterCmd ) =
                    Porter.update porterConfig porterMsg model.porter
            in
                ( { model | porter = porterModel }, porterCmd )

        Receive response ->
            ( { model | response = response }, Cmd.none )

        ReceiveAdvanced response ->
            ( { model | advancedResponse = response }, Cmd.none )
        ReceiveMapped response ->
            ( { model | mapResponse = response }, Cmd.none )



-- Set up porter's subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Porter.subscriptions porterConfig



-- Any view you like


view : Model -> Html Msg
view model =
    text (toString model)



--


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
