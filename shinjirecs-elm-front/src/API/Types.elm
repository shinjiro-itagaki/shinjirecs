module API.Types exposing (API,SystemI,ChannelsI,ColumnInfo,columnInfoDecoder,ResourcesI)
import Json.Decode as D exposing (map6,nullable)
import Dict exposing (Dict)
import Http
import Models.Types exposing (Entity)
import Models.Area exposing (Area)
import Models.Program as M exposing (Program)
import Models.ProgramTitle exposing (ProgramTitle)
import Models.System exposing (System)
import Models.Channel exposing (Channel)
import Models.ProgramCategory exposing (ProgramCategory)
import Models.Reservation exposing (Reservation)


{- type Error = BadUrl String
              | Timeout
              | NetworkError
              | BadStatus (Response String)
              | BadPayload String (Response String)
-}

{- type alias Response body = { url : String
                              , status : { code : Int
                                         , message : String
                                         }
                              , headers : Dict String String
                              , body : body
                              }
-}

type alias API =
    { system            : SystemI
--    , areas             : AreasI
    , channels          : ChannelsI
--    , programs          : ProgramsI
--    , programCategories : ProgramCategoriesI
--    , programTitles     : ProgramTitlesI
--    , reservations      : ReservationsI
    }

-- type ColumnType = IntegerT | StringT | TimeT
    
type alias ColumnInfo = { nullable  : Bool
                        , default   : String
                        , limit     : Maybe Int
                        , precision : Maybe Int
                        , scale     : Maybe Int
                        , tipe      : String
                        }

columnInfoDecoder : D.Decoder ColumnInfo
columnInfoDecoder =
    D.map6
        ColumnInfo
        (D.at ["nullable"]  D.bool)
        (D.at ["default"]   D.string)
        (D.at ["limit"]     (nullable D.int))
        (D.at ["precision"] (nullable D.int))
        (D.at ["scale"]     (nullable D.int))
        (D.at ["type"]      D.string)
            
type alias ResourcesI a =
    { index   : Maybe Int -> Cmd (Result Http.Error (List (Entity a))) -- Int => limit
    , get     : Int       -> Cmd (Result Http.Error a) 
    , create  : a         -> Cmd (Result Http.Error (Entity a)) 
    , modify  : Entity a  -> Cmd (Result Http.Error a)
    , destroy : Entity a  -> Cmd (Result Http.Error Bool)
    , info    : Cmd (Result Http.Error (Dict String ColumnInfo))
    }

type alias AreasI             = ResourcesI Area
type alias ChannelsI          = ResourcesI Channel
type alias ProgramsI          = ResourcesI M.Program
type alias ProgramCategoriesI = ResourcesI ProgramCategory
type alias ProgramTitlesI     = ResourcesI ProgramTitle
type alias ReservationsI      = ResourcesI Reservation
type alias SystemI            = ResourcesI System
