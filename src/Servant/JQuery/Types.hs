module Servant.JQuery.Types where

-- | Defines rendering settings for Javascript functions that contain
-- jQuery AJAX requests.
data Settings = Settings
    { _functionFormat :: FunctionFormat -- ^ Format for wrapper function
    , _baseURI        :: String -- ^ Base URL (yet to be used)
    }

-- | Default settings: hoisted functions, with no base URL.
--
-- Equivalent to:
-- @
--     Settings Hoisted ""
-- @
defaultSettings :: Settings
defaultSettings = Settings Hoisted ""

-- | Selects which format is used to render Javascript functions.
data FunctionFormat = Hoisted -- ^ Hoisted functions can be invoked in code
                              -- before they are declared. The function name
                              -- is 'hoisted' to the top, and invokes the
                              -- body where it is.
                    | NonHoisted -- ^ Function declared as a variable, can
                                 -- only be invoked later in the code.
                    | Module String -- ^ Function that belongs to an existing
                                    -- module.
                                    --
                                    -- String parameter denotes the
                                    -- module name.
                    | PurescriptFriendly -- ^ Function prepared with the right
                                         --  nesting for Purescript.
                    | Anonymous Bool -- ^ Anonymous function.
                                     --
                                     -- Bool parameter denotes whether the
                                     -- anonymous function is being returned
                                     -- as the result of another function.

-- | Internal representation of Javascript functions.
data JSFunction =
    HoistedFn
    { _funcName     :: String -- ^ Function name
    , _funcArgs     :: [String] -- ^ Function arguments
    , _funcContents :: Either JSFunction String -- ^ Function body
    } | NonHoistedFn
    { _funcName     :: String -- ^ Function name
    , _funcArgs     :: [String] -- ^ Function arguments
    , _funcModule   :: Maybe String -- ^ Name of the module this function is
                                    -- part of.
                                    -- 
                                    -- If Nothing, is treated as a var in the
                                    -- current scope.
    , _funcContents :: Either JSFunction String -- ^ Function body
    } | AnonymousFn
    { _funcArgs     :: [String] -- ^ Function arguments
    , _funcReturned :: Bool -- ^ Whether the function is returned
                            -- as a variable
    , _funcContents :: Either JSFunction String -- ^ Function body
    }
