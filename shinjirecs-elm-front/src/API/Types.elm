module API.Types exposing (API,System,Channels)

type alias System = {
        index   : Int -> Int,
        get     : Int -> Int,
        create  : Int -> Int,
        modify  : Int -> Int,
        destroy : Int -> Int
    }

type alias Channels = {
        index   : Int -> Int,
        get     : Int -> Int,
        create  : Int -> Int,
        modify  : Int -> Int,
        destroy : Int -> Int
    }
    

type alias API = {
        system   : System,
        channels : Channels
    }

