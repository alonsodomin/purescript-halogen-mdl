module Router where

import Prelude

import Control.Alt ((<|>))
import Data.Maybe (Maybe(..))
import DemoContainer as DemoContainer
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, forkAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Route (Route(..), urlSegment)
import Routing.Hash (matches)
import Routing.Match (Match, lit)

-- Routing logic

routeSignal :: H.HalogenIO Query Void Aff -> Aff Unit
routeSignal driver =
  void $ forkAff <<< liftEffect $ matches matchRoute (goToRoute driver)

matchRoute :: Match Route
matchRoute
  =   badges
  <|> buttons
  <|> cards
  <|> chips
  <|> dialogs
  <|> lists
  <|> menus
  <|> progress
  <|> sliders
  <|> snackbars
  <|> spinners
  <|> tabs
  <|> toggles
  <|> home
  where
    badges = Badges <$ route (urlSegment Badges)
    buttons = Buttons <$ route (urlSegment Buttons)
    cards = Cards <$ route (urlSegment Cards)
    chips = Chips <$ route (urlSegment Chips)
    dialogs = Dialogs <$ route (urlSegment Dialogs)
    lists = Lists <$ route (urlSegment Lists)
    menus = Menus <$ route (urlSegment Menus)
    progress = Progress <$ route (urlSegment Progress)
    sliders = Sliders <$ route (urlSegment Sliders)
    snackbars = Snackbars <$ route (urlSegment Snackbars)
    spinners = Spinners <$ route (urlSegment Spinners)
    tabs = Tabs <$ route (urlSegment Tabs)
    toggles = Toggles <$ route (urlSegment Toggles)
    home = Home <$ lit ""
    route str = lit "" *> lit str

goToRoute :: H.HalogenIO Query Message Aff
  -> Maybe Route
  -> Route
  -> Effect Unit
goToRoute driver _ =
  launchAff_ <<< driver.query <<< H.action <<< GoTo

-- Router component

type State =
  { currentRoute :: Route
  }

data Query a
  = OnDemoContainerMessage DemoContainer.Message a
  | GoTo Route a

type Input = Unit

type Message = Void

data Slot = DemoContainerSlot
derive instance eqSlot :: Eq Slot
derive instance ordSlot :: Ord Slot

component :: H.Component HH.HTML Query Input Message Aff
component = H.parentComponent
  { initialState: initialState
  , receiver: receiver
  , render
  , eval
  }
  where
    initialState :: Input -> State
    initialState _ = { currentRoute: Home }

    receiver :: Input -> Maybe (Query Unit)
    receiver _ = Nothing

    render :: State -> H.ParentHTML Query DemoContainer.Query Slot Aff
    render state =
      HH.div
        [ HP.class_ $ HH.ClassName "root" ]
        [ HH.slot
            DemoContainerSlot
            DemoContainer.demoContainer
            (DemoContainer.init { currentRoute: state.currentRoute })
            (HE.input OnDemoContainerMessage)
        ]

    eval :: Query ~> H.ParentDSL State Query DemoContainer.Query Slot Message Aff
    eval = case _ of
      GoTo route next -> do
        H.modify_ (_ { currentRoute = route })
        pure next

      OnDemoContainerMessage message next -> do
        pure next
