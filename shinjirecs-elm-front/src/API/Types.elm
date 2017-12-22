module API.Types exposing (..)
import Dict exposing (Dict)
import Http
import Http.Progress exposing (Progress)
import Records.Types exposing (Entity)
import Records.ColumnInfo exposing (ColumnInfo,columnInfoDecoder)
import Records.Area exposing (Area)
import Records.EpgProgram exposing (EpgProgram)
import Records.ProgramSeries exposing (ProgramSeries)
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
                   , epgProgramMediumCategories : Maybe (Dict Int (Entity EpgProgramCategory))
                   , programSeries        : Maybe (Dict Int (Entity ProgramSeries))
                   , reservations         : Maybe (Dict Int (Entity Reservation))
                   }

emptyCache = Cache Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    
type alias API =
    { system            : SystemI
    , areas             : AreasI
    , channels          : ChannelsI
    , epgPrograms       : EpgProgramsI
    , epgProgramsEx     : EpgProgramsExI                          
    , epgProgramCategories : EpgProgramCategoriesI
    , programSeries     : ProgramSeriesI
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
    , indexAsync   : Maybe Int -> Sub (Progress (List (Entity a))) -- Int => limit
    , getAsync     : Int       -> Sub (Progress a) 
    , createAsync  : a         -> Sub (Progress (Entity a)) 
    , modifyAsync  : Entity a  -> Sub (Progress a)
    , destroyAsync : Entity a  -> Sub (Progress Bool)
    , infoAsync    : Sub (Progress (Dict String ColumnInfo))
    }

type alias AreasI         = ResourcesI Area
type alias ChannelsI      = ResourcesI Channel
type alias EpgProgramsI   = ResourcesI EpgProgram
type alias EpgProgramsExI = { epgdump : Cmd (Result Http.Error Bool) -- post "epg_programs/epgdump"
                            , newReservation : Entity EpgProgram -> Cmd (Result Http.Error (Entity Reservation)) -- post "epg_programs/:id/new_reservation"
                            }
type alias EpgProgramCategoriesI = ResourcesI EpgProgramCategory
type alias ProgramSeriesI     = ResourcesI ProgramSeries
type alias ReservationsI      = ResourcesI Reservation
type alias SystemI            = { get    : Cmd (Result Http.Error (Entity System))
                                , modify : Entity System -> Cmd (Result Http.Error System)
                                , info   : Cmd (Result Http.Error (Dict String ColumnInfo))
                                , all    : Cmd (Result Http.Error Cache)
                                , getAsync    : Sub (Progress (Entity System))
                                , modifyAsync : Entity System -> Sub (Progress System)
                                , infoAsync   : Sub (Progress (Dict String ColumnInfo))
                                , allAsync    : Sub (Progress Cache)
                                }-- ResourcesI System
