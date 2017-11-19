module API.Types exposing (API,AreasI,ChannelsI,EpgProgramsI,EpgProgramCategoriesI,ProgramTitlesI,ReservationsI,SystemI,ResourcesI)
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
                   , areas                : Dict Int Area
                   , channels             : Dict Int Channel
                   , epgPrograms          : Dict Int EpgProgram
                   , epgProgramCategories : Dict Int EpgProgramCategory
                   , programTitles        : Dict Int ProgramTitle
                   , reservations         : Dict Int Reservation
                   }

caster : (Cache, List Http.Error)
       -> (Cache -> a -> Cache)         
       -> Result Http.Error a
       -> (Cache, List Http.Error)
caster (cache, errs) func res =
    case res of
        Ok data -> (func cache data, errs)
        Err err -> (cache, errs ++ [err])
    
refreshEach : Cache
            -> (Cmd (Result Http.Error a), (Cache -> a -> Cache))
            -> (Cmd (Result Http.Error b), (Cache -> b -> Cache))
            -> (Cmd (Result Http.Error c), (Cache -> c -> Cache))
            -> (Cmd (Result Http.Error d), (Cache -> d -> Cache))
            -> (Cmd (Result Http.Error e), (Cache -> e -> Cache))
            -> (Cmd (Result Http.Error f), (Cache -> f -> Cache))
            -> (Cmd (Result Http.Error g), (Cache -> g -> Cache))
            -> Cmd (Cache, List Http.Error)
    
refreshEach cache (ca,fa) (cb,fb) (cc,fc) (cd,fd) (ce,fe) (cf,ff) (cg,fg) =
    Cmd.map (caster (cache, []) fa) ca
--        |> Cmd.map (caster cb)
--        |> Cmd.map (caster cc)
--        |> Cmd.map (caster cd)
--        |> Cmd.map (caster ce)
--        |> Cmd.map (caster cf)
--        |> Cmd.map (caster cg)
{-    
refreshCache : API -> Cache -> Cmd (Cache, List Http.Error)
refreshCache api old =
    refreshEach old
        (api.system.get                     ,  (\c res -> {c| system            = res} ) )
        (api.areas.index             Nothing,  (\c res -> {c| areas             = res} ) )
        (api.channels.index          Nothing,  (\c res -> {c| channels          = res} ) )
        (api.programs.index          Nothing,  (\c res -> {c| programs          = res} ) )
        (api.programCategories.index Nothing,  (\c res -> {c| programCategories = res} ) )
        (api.programTitles.index     Nothing,  (\c res -> {c| programTitles     = res} ) )
        (api.reservations.index      Nothing,  (\c res -> {c| reservations      = res} ) )
-- Cmd.map (\res -> ({old | system = Just s}, emptyHttpErrors)) api.system.get
-}
    
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
                                }-- ResourcesI System
