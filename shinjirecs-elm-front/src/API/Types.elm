module API.Types exposing (API,SystemI,ChannelsI,ResourcesI)
import Dict exposing (Dict)
import Http
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Records.Area exposing (Area)
import Records.EpgProgram exposing (EpgProgram)
import Records.ProgramTitle exposing (ProgramTitle)
import Records.System exposing (System)
import Records.Channel exposing (Channel)
import Records.EpgProgramCategory exposing (EpgProgramCategory)
import Records.Reservation exposing (Reservation)


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
type alias EpgProgramsI          = ResourcesI EpgProgram
type alias EpgProgramCategoriesI = ResourcesI EpgProgramCategory
type alias ProgramTitlesI     = ResourcesI ProgramTitle
type alias ReservationsI      = ResourcesI Reservation
type alias SystemI            = ResourcesI System
