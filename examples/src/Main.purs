module Main where

import Prelude (bind, Unit, unit)
import Control.Monad.Eff (Eff)

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Container (container)

main :: Eff (HA.HalogenEffects ()) Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI container unit body