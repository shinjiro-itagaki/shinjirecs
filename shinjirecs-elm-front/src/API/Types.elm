module API.Types exposing (API,AreasI,ChannelsI,EpgProgramsI,EpgProgramCategoriesI,ProgramTitlesI,ReservationsI,SystemI,ResourcesI,Cache,emptyCache)
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

type alias Cache = { system               : Maybe (Entity System)
                   , areas                : Maybe (Dict Int (Entity Area))
                   , channels             : Maybe (Dict Int (Entity Channel))
                   , epgPrograms          : Maybe (Dict Int (Entity EpgProgram))
                   , epgProgramCategories : Maybe (Dict Int (Entity EpgProgramCategory))
                   , programTitles        : Maybe (Dict Int (Entity ProgramTitle))
                   , reservations         : Maybe (Dict Int (Entity Reservation))
                   }

emptyCache = Cache Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    
type alias API =
    { system            : SystemI
    , areas             : AreasI
    , channels          : ChannelsI
    , epgPrograms       : EpgProgramsI
    , epgProgramCategories : EpgProgramCategoriesI
    , programTitles     : ProgramTitlesI
    , reservations      : ReservationsI
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
type alias SystemI            = { get    : Cmd (Result Http.Error (Entity System))
                                , modify : Entity System -> Cmd (Result Http.Error System)
                                , info   : Cmd (Result Http.Error (Dict String ColumnInfo))
                                , all    : Cmd (Result Http.Error Cache)
                                }-- ResourcesI System
