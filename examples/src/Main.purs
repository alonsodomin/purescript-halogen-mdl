module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (forkAff)
import Effect.Class (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Router as Router

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  driver <- runUI Router.component unit body
  Router.routeSignal driver
