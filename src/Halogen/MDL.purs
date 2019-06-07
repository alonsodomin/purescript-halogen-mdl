module Halogen.MDL where

import Prelude
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)

import Web.HTML (HTMLElement)

import Halogen as H
import Halogen.HTML as HH

-- Upgrade element to add MDL behaviors
foreign import upgradeElement :: HTMLElement -> Effect Unit

-- Upgrade elements by CSS selector (via querySelector)
foreign import upgradeElementsBySelector :: String -> Effect Unit

-- Upgrade elements by CSS selectors (via querySelector)
foreign import upgradeElementsBySelectors :: Array String -> Effect Unit

-- Upgrade elements by class
upgradeElementsByClassName :: HH.ClassName -> Effect Unit
upgradeElementsByClassName (HH.ClassName className) = upgradeElementsBySelector $ "." <> className

-- Upgrade elements by classes
upgradeElementsByClassNames :: Array (HH.ClassName) -> Effect Unit
upgradeElementsByClassNames classNames = void $ traverse upgradeElementsByClassName classNames

-- Upgrade element by Halogen.RefLabel
upgradeElementByRef :: forall s f g p o m. MonadEffect m => H.RefLabel -> H.HalogenM s f g p o m Unit
upgradeElementByRef ref = do
  element <- H.getHTMLElementRef ref
  case element of
    Just element -> H.liftEffect $ upgradeElement element
    Nothing -> pure unit

-- Upgrade elements by Halogen.RefLabels
upgradeElementsByRefs :: ∀ s f g p o m. MonadEffect m => Array H.RefLabel -> H.HalogenM s f g p o m Unit
upgradeElementsByRefs refs = void $ traverse upgradeElementByRef refs

-- Remove class from an element
-- Hacky solution to removing classes added outside of our rendering control
foreign import removeClass :: HTMLElement -> String -> Effect Unit

attr ::
  { disabled :: HH.AttrName
  , for :: HH.AttrName
  }
attr =
  { disabled : HH.AttrName "data-mdl-disabled"
  , for      : HH.AttrName "data-mdl-for"
  }
