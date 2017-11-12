module API exposing (getAPI)
import API.Types as T exposing (API)
import API.Remote as Impl exposing (getImpl)

-- type alias API = T.API
getAPI : String -> T.API
getAPI = Impl.getImpl
