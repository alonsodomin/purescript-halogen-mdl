module Halogen.MDL.Progress where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe(..))

import Web.HTML (HTMLElement)

import Halogen as H
import Halogen.HTML as HH

foreign import setProgress :: HTMLElement -> Int -> Effect Unit
foreign import setBuffer :: HTMLElement -> Int -> Effect Unit

setProgressByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> Int -> H.HalogenM s f g p o m Unit
setProgressByRef ref value = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEffect $ setProgress element value
    Nothing -> pure unit

setBufferByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> Int -> H.HalogenM s f g p o m Unit
setBufferByRef ref value = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEffect $ setBuffer element value
    Nothing -> pure unit

cl ::
  { progress :: HH.ClassName
  , jsProgress :: HH.ClassName
  , progressIndeterminate :: HH.ClassName
  }
cl =
  { progress   : HH.ClassName "mdl-progress"
  , jsProgress : HH.ClassName "mdl-js-progress"
  , progressIndeterminate : HH.ClassName "mdl-progress__indeterminate"
  }
