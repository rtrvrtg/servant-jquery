module Servant.JQuery.Types where

data Settings = Settings
    { _functionFormat :: FunctionFormat
    , _baseURI        :: String
    }

defaultSettings :: Settings
defaultSettings = Settings Hoisted ""

data FunctionFormat = Hoisted
                    | NonHoisted
                    | Module String
                    | PurescriptFriendly
                    | Anonymous Bool

data JSFunction =
    HoistedFn
    { _funcName     :: String
    , _funcArgs     :: [String]
    , _funcContents :: Either JSFunction String
    } | NonHoistedFn
    { _funcName     :: String
    , _funcArgs     :: [String]
    , _funcModule   :: Maybe String
    , _funcContents :: Either JSFunction String
    } | AnonymousFn
    { _funcArgs     :: [String]
    , _funcReturned :: Bool
    , _funcContents :: Either JSFunction String
    }
