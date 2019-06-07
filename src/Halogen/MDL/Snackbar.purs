module Halogen.MDL.Snackbar where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe(..))

import Web.HTML (HTMLElement)

import Halogen as H
import Halogen.HTML as HH

type SnackbarDataNoAction =
  { message :: String
  , timeout :: Int
  }

type SnackbarDataWithAction =
  { message :: String
  , timeout :: Int
  , actionHandler :: Unit -> Effect Unit
  , actionText :: String
  }

foreign import showSnackbarNoAction :: HTMLElement -> SnackbarDataNoAction -> Effect Unit
foreign import showSnackbarWithAction :: HTMLElement -> SnackbarDataWithAction -> Effect Unit

showSnackbarNoActionByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> SnackbarDataNoAction -> H.HalogenM s f g p o m Unit
showSnackbarNoActionByRef ref data_ = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEffect $ showSnackbarNoAction element data_
    Nothing -> pure unit

showSnackbarWithActionByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> SnackbarDataWithAction -> H.HalogenM s f g p o m Unit
showSnackbarWithActionByRef ref data_ = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEffect $ showSnackbarWithAction element data_
    Nothing -> pure unit

foreign import hideSnackbar :: HTMLElement -> Effect Unit

hideSnackbarByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> H.HalogenM s f g p o m Unit
hideSnackbarByRef ref = do
  maybeElement <- H.getHTMLElementRef ref
  case maybeElement of
    Just element -> H.liftEffect $ hideSnackbar element
    Nothing -> pure unit

cl ::
  { snackbar :: HH.ClassName
  , jsSnackbar :: HH.ClassName
  , snackbarText :: HH.ClassName
  , snackbarAction :: HH.ClassName
  , snackbarActive :: HH.ClassName
  }

cl =
  { snackbar       : HH.ClassName "mdl-snackbar"
  , jsSnackbar     : HH.ClassName "mdl-js-snackbar"
  , snackbarText   : HH.ClassName "mdl-snackbar__text"
  , snackbarAction : HH.ClassName "mdl-snackbar__action"
  , snackbarActive : HH.ClassName "mdl-snackbar--active"
  }
