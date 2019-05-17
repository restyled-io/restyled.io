module RIO.Handler
    ( runHandlerRIO
    )
where

import RIO

import Yesod (HandlerFor, getYesod)

runHandlerRIO :: RIO env a -> HandlerFor env a
runHandlerRIO f = do
    app <- getYesod
    runRIO app f
