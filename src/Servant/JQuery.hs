{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Servant.JQuery
-- Copyright   :  (C) 2014 Alp Mestanogullari
-- License     :  BSD3
-- Maintainer  :  Alp Mestanogullari <alpmestan@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
module Servant.JQuery
  ( jquery
  , generateJS
  , generateJSWith
  , printJS
  , printJSWith
  , module Servant.JQuery.Internal
  , Settings(..)
  , FunctionFormat(..)
  , defaultSettings
  ) where

import Control.Lens
import Data.List
import Data.Monoid
import Data.Proxy
import Servant.JQuery.Functions
import Servant.JQuery.Internal
import Servant.JQuery.Types

-- | Compiles a Servant API type represented by a Proxy into a series of
-- values joined by the
-- @
-- :\<|\>
-- @
-- combinator provided by Servant API, each representing an API endpoint.
-- These can then be used to render each API endpoint out as a Javascript
-- function that, when invoked, sends a request to this endpoint.
jquery
    :: HasJQ layout
    => Proxy layout -- ^ Proxy for a Servant API type
    -> JQ layout -- ^ A JQ layout object containing a series of AjaxReq
                 -- values separated by
                 -- @
                 -- :\<|\>
                 -- @
jquery p = jqueryFor p defReq

-- | Renders a jQuery AJAX request definition into Javascript code,
-- using default settings.
generateJS
    :: AjaxReq -- ^ AJAX request definition
    -> String  -- ^ Rendered Javascript
generateJS = generateJSWith defaultSettings

-- | Renders a jQuery AJAX request definition into Javascript code,
-- using custom settings provided as an argument.
generateJSWith
    :: Settings -- ^ Servant.JQuery Settings
    -> AjaxReq -- ^ AJAX request
    -> String  -- ^ Rendered Javascript
generateJSWith settings req = renderFunctionWrap (_functionFormat settings)
                                              fname args inner
  where 
        inner = "  $.ajax(\n"
             <> "    { url: " <> url <> "\n"
             <> "    , success: onSuccess\n"
             <> dataBody
             <> reqheaders
             <> "    , error: onError\n"
             <> "    , type: '" <> method <> "'\n"
             <> "    });\n"

        args = captures
            ++ map (view argName) queryparams
            ++ body
            ++ map (toValidFunctionName . (<>) "header" . headerArgName) hs
            ++ ["onSuccess", "onError"]
        
        captures = map captureArg
                 . filter isCapture
                 $ req ^. reqUrl.path

        hs = req ^. reqHeaders

        queryparams = req ^.. reqUrl.queryStr.traverse

        body = if req ^. reqBody
                 then ["body"]
                 else []

        dataBody =
          if req ^. reqBody
            then "\n    , data: JSON.stringify(body)\n"
            else ""

        reqheaders =
          if null hs
            then ""
            else "\n    , headers: { " ++ headersStr ++ " }\n"

          where headersStr = intercalate ", " $ map headerStr hs
                headerStr header = "\"" ++
                  headerArgName header ++
                  "\": " ++ show header

        fname = req ^. funcName
        method = req ^. reqMethod
        url = "'"
           ++ urlArgs
           ++ queryArgs

        urlArgs = jsSegments
                $ req ^.. reqUrl.path.traverse

        queryArgs = if null queryparams
                      then ""
                      else " + '?" ++ jsParams queryparams

-- | Renders a jQuery AJAX request definition into Javascript code,
-- using default settings, and prints it to STDOUT.
printJS
    :: AjaxReq -- ^ AJAX request definition
    -> IO ()
printJS = putStrLn . generateJS

-- | Renders a jQuery AJAX request definition into Javascript code,
-- using default settings, and prints it to STDOUT.
printJSWith
    :: Settings -- ^ Servant.JQuery Settings
    -> AjaxReq -- ^ AJAX request definition
    -> IO ()
printJSWith settings = putStrLn . generateJSWith settings
