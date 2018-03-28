module Main where

import Prelude
import Control.Coroutine as CR
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.Console (CONSOLE, log)
-- import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff (HalogenEffects, runHalogenAff, awaitBody)
import Halogen.VDom.Driver (runUI)
import Button as B

main :: Eff (HalogenEffects (console :: CONSOLE)) Unit
main = runHalogenAff do
    body <- awaitBody
    ui <- runUI B.myButton unit body

    ui.subscribe $ CR.consumer \(B.Toggled i) -> do
        log $ "No" <> show i
        pure Nothing

    -- ui.query $ H.action $ B.Toggle
    pure ui