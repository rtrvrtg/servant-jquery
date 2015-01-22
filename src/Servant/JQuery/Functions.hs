module Servant.JQuery.Functions (
    renderFunctionWrap
) where

import Data.List
import Data.Monoid
import Servant.JQuery.Types

-- | Renders a JS function wrapper
renderFunctionWrap
    :: FunctionFormat -- ^ Format of function
    -> String -- ^ Function name
    -> [String] -- ^ Function argument variable names
    -> String -- ^ Function content
    -> String -- ^ Rendered JS function
renderFunctionWrap f n a m = show $ getFunctionWrap f n a (Right m)

-- | Prepares a JS function wrapper
getFunctionWrap
    :: FunctionFormat -- ^ Format of function
    -> String -- ^ Function name
    -> [String] -- ^ Function argument variable names
    -> Either JSFunction String -- ^ Function content
    -> JSFunction -- ^ Prepared JS function
getFunctionWrap Hoisted n a m     = HoistedFn n a m
getFunctionWrap NonHoisted n a m  = NonHoistedFn n a Nothing m
getFunctionWrap (Module mn) n a m = NonHoistedFn n a (Just mn) m
getFunctionWrap PurescriptFriendly n [] m     = HoistedFn n [] m
getFunctionWrap PurescriptFriendly n (a:[]) m = HoistedFn n [a] m
getFunctionWrap PurescriptFriendly n a m = HoistedFn n a . Left $
    getFunctionWrap (Anonymous True) "" [] m
getFunctionWrap (Anonymous rt) _ a m = AnonymousFn a rt m

instance Show JSFunction where
    show (HoistedFn n a m)       = hoistedF n (argsStr a) (either show id m)
    show (NonHoistedFn n a mn m) = nonHoistedF mn n (argsStr a) (either show id m)
    show (AnonymousFn a rt m)    = anonF (argsStr a) rt (either show id m)

-- | Renders a hoisted function
hoistedF
    :: String -- ^ Function name
    -> String -- ^ Argument name/s
    -> String -- ^ Function content
    -> String -- ^ Rendered JS hoisted function
hoistedF fname margs middle = "\n"
    <> "function " <> fname
    <> "(" <> margs <> "){" <> "\n"
    <> middle <> "}" <> "\n"

-- | Render a non-hoisted function
nonHoistedF
    :: Maybe String -- ^ Either a module name or a Nothing to specify
                    -- we should use a var
    -> String -- ^ Function name
    -> String -- ^ Argument name/s
    -> String -- ^ Function content
    -> String -- ^ Rendered JS non-hoisted function
nonHoistedF mname fname margs middle = "\n"
    <> prefix <> fname <> " = function ("
    <> margs <> "){" <> "\n"
    <> middle <> "};" <> "\n"
  where
    prefix = case mname of
        Just mn -> mn <> "."
        Nothing -> "var "

-- | Renders an anonymous function
anonF
    :: String -- ^ Argument name/s
    -> Bool -- ^ Whether the function should be returned as a variable
    -> String -- ^ Function content
    -> String -- ^ Rendered JS anonymous function
anonF margs willReturn middle = "\n"
    <> returnOrNot
    <> "function (" <> margs
    <> "){" <> "\n"
    <> middle
    <> returnTrail <> "\n"
  where
    returnOrNot = if willReturn then "return " else ""
    returnTrail = if willReturn then "};" else "}"

-- Turns argument names into a comma separated string
argsStr
    :: [String] -- ^ Argument variable names
    -> String -- ^ Comma separated variable list
argsStr = intercalate ","
