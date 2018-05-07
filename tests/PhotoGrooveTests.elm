module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
--import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

-- PhotoGroove specific stuff
import PhotoGroove exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode


samplePhotoJson : String
samplePhotoJson =
    """
        { "url": "fruits.com"
        , "size": 5
        }
    """

suite : Test
suite =
    test "one plus one equals two" ( \_ -> Expect.equal 2 (1+1))

decodeTest : Test
decodeTest =
    let
        titleResult = "(untitled)"
    in
        
    test "Test defaults to (untitled)" <| 
        \_ -> 
            [ ("url", Encode.string "fruits.com")
            , ("size", Encode.int 5)
            ]
                |> Encode.object
                |> decodeValue photoDecoder
                |> Result.map .title
                |> Expect.equal ( Ok titleResult )
