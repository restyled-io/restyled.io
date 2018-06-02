{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Static file references
--
-- To refer to @static\/js\/script.js@:
--
-- > js_script_js
--
-- This is equivalent to:
--
-- @
-- 'StaticFile' ["js", "script.js"] []
-- @
--
module Settings.StaticFiles
    ( module Settings.StaticFiles
    )
where

import Settings (appStaticDir)
import Yesod.Static (staticFiles)

staticFiles appStaticDir
