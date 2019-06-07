module DemoContainer where

import Prelude

import Effect.Aff (Aff)
import Data.Array ((..))
import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.Aff as HA
import Halogen.Component.ChildPath ((:>))
import Halogen.Component.ChildPath as CP
import Halogen.Data.Prism (type (\/))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Halogen.MDL as MDL
import Halogen.MDL.Basic as Basic
import Halogen.MDL.Layout as Layout
import Halogen.MDL.MegaFooter as MegaFooter
import Halogen.MDL.Navigation as Navigation

import Route (Route(..))
import Route as Route
import DemoHome as DemoHome
import DemoBadges as DemoBadges
import DemoButtons as DemoButtons
import DemoCards as DemoCards
import DemoChips as DemoChips
import DemoDialogs as DemoDialogs
import DemoLists as DemoLists
import DemoMenus as DemoMenus
import DemoProgress as DemoProgress
import DemoSliders as DemoSliders
import DemoSnackbars as DemoSnackbars
import DemoSpinners as DemoSpinners
import DemoTabs as DemoTabs
import DemoToggles as DemoToggles

type State =
  { currentRoute :: Route
  }

data Query a
  = InitializeComponent a
  | FinalizeComponent a
  | UpdateState State a
  | UpdateRoute Route a
  | OnNavClick a
  | OnDemoHomeMessage DemoHome.Message a
  | OnDemoBadgesMessage DemoBadges.Message a
  | OnDemoButtonsMessage DemoButtons.Message a
  | OnDemoCardsMessage DemoCards.Message a
  | OnDemoChipsMessage DemoChips.Message a
  | OnDemoDialogsMessage DemoDialogs.Message a
  | OnDemoListsMessage DemoLists.Message a
  | OnDemoMenusMessage DemoMenus.Message a
  | OnDemoProgressMessage DemoProgress.Message a
  | OnDemoSlidersMessage DemoSliders.Message a
  | OnDemoSnackbarsMessage DemoSnackbars.Message a
  | OnDemoSpinnersMessage DemoSpinners.Message a
  | OnDemoTabsMessage DemoTabs.Message a
  | OnDemoTogglesMessage DemoToggles.Message a

data Input = Initialize State

type Message = Void

-- TODO: use custom sum type?

type ChildQuery
  =    DemoHome.Query
  <\/> DemoBadges.Query
  <\/> DemoButtons.Query
  <\/> DemoCards.Query
  <\/> DemoChips.Query
  <\/> DemoDialogs.Query
  <\/> DemoLists.Query
  <\/> DemoMenus.Query
  <\/> DemoProgress.Query
  <\/> DemoSliders.Query
  <\/> DemoSnackbars.Query
  <\/> DemoSpinners.Query
  <\/> DemoTabs.Query
  <\/> DemoToggles.Query
  <\/> Const Void

type ChildSlot
  =  DemoHomeSlot
  \/ DemoBadgesSlot
  \/ DemoButtonsSlot
  \/ DemoCardsSlot
  \/ DemoChipsSlot
  \/ DemoDialogsSlot
  \/ DemoListsSlot
  \/ DemoMenusSlot
  \/ DemoProgressSlot
  \/ DemoSlidersSlot
  \/ DemoSnackbarsSlot
  \/ DemoSpinnersSlot
  \/ DemoTabsSlot
  \/ DemoTogglesSlot
  \/ Void

-- Slots
data DemoHomeSlot = DemoHomeSlot
derive instance eqDemoHomeSlot :: Eq DemoHomeSlot
derive instance ordDemoHomeSlot :: Ord DemoHomeSlot
cpDemoHome :: CP.ChildPath DemoHome.Query ChildQuery DemoHomeSlot ChildSlot
cpDemoHome = CP.cp1

data DemoBadgesSlot = DemoBadgesSlot
derive instance eqDemoBadgesSlot :: Eq DemoBadgesSlot
derive instance ordDemoBadgesSlot :: Ord DemoBadgesSlot
cpDemoBadges :: CP.ChildPath DemoBadges.Query ChildQuery DemoBadgesSlot ChildSlot
cpDemoBadges = CP.cp2

data DemoButtonsSlot = DemoButtonsSlot
derive instance eqDemoButtonsSlot :: Eq DemoButtonsSlot
derive instance ordDemoButtonsSlot :: Ord DemoButtonsSlot
cpDemoButtons :: CP.ChildPath DemoButtons.Query ChildQuery DemoButtonsSlot ChildSlot
cpDemoButtons = CP.cp3

data DemoCardsSlot = DemoCardsSlot
derive instance eqDemoCardsSlot :: Eq DemoCardsSlot
derive instance ordDemoCardsSlot :: Ord DemoCardsSlot
cpDemoCards :: CP.ChildPath DemoCards.Query ChildQuery DemoCardsSlot ChildSlot
cpDemoCards = CP.cp4

data DemoChipsSlot = DemoChipsSlot
derive instance eqDemoChipsSlot :: Eq DemoChipsSlot
derive instance ordDemoChipsSlot :: Ord DemoChipsSlot
cpDemoChips :: CP.ChildPath DemoChips.Query ChildQuery DemoChipsSlot ChildSlot
cpDemoChips = CP.cp5

data DemoDialogsSlot = DemoDialogsSlot
derive instance eqDemoDialogsSlot :: Eq DemoDialogsSlot
derive instance ordDemoDialogsSlot :: Ord DemoDialogsSlot
cpDemoDialogs :: CP.ChildPath DemoDialogs.Query ChildQuery DemoDialogsSlot ChildSlot
cpDemoDialogs = CP.cp6

data DemoListsSlot = DemoListsSlot
derive instance eqDemoListsSlot :: Eq DemoListsSlot
derive instance ordDemoListsSlot :: Ord DemoListsSlot
cpDemoLists :: CP.ChildPath DemoLists.Query ChildQuery DemoListsSlot ChildSlot
cpDemoLists = CP.cp7

data DemoMenusSlot = DemoMenusSlot
derive instance eqDemoMenusSlot :: Eq DemoMenusSlot
derive instance ordDemoMenusSlot :: Ord DemoMenusSlot
cpDemoMenus :: CP.ChildPath DemoMenus.Query ChildQuery DemoMenusSlot ChildSlot
cpDemoMenus = CP.cp8

data DemoProgressSlot = DemoProgressSlot
derive instance eqDemoProgressSlot :: Eq DemoProgressSlot
derive instance ordDemoProgressSlot :: Ord DemoProgressSlot
cpDemoProgress :: CP.ChildPath DemoProgress.Query ChildQuery DemoProgressSlot ChildSlot
cpDemoProgress = CP.cp9

data DemoSlidersSlot = DemoSlidersSlot
derive instance eqDemoSlidersSlot :: Eq DemoSlidersSlot
derive instance ordDemoSlidersSlot :: Ord DemoSlidersSlot
cpDemoSliders :: CP.ChildPath DemoSliders.Query ChildQuery DemoSlidersSlot ChildSlot
cpDemoSliders = CP.cp10

data DemoSnackbarsSlot = DemoSnackbarsSlot
derive instance eqDemoSnackbarsSlot :: Eq DemoSnackbarsSlot
derive instance ordDemoSnackbarsSlot :: Ord DemoSnackbarsSlot
cpDemoSnackbars :: CP.ChildPath DemoSnackbars.Query ChildQuery DemoSnackbarsSlot ChildSlot
cpDemoSnackbars =
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpL

data DemoSpinnersSlot = DemoSpinnersSlot
derive instance eqDemoSpinnersSlot :: Eq DemoSpinnersSlot
derive instance ordDemoSpinnersSlot :: Ord DemoSpinnersSlot
cpDemoSpinners :: CP.ChildPath DemoSpinners.Query ChildQuery DemoSpinnersSlot ChildSlot
cpDemoSpinners =
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpL

data DemoTabsSlot = DemoTabsSlot
derive instance eqDemoTabsSlot :: Eq DemoTabsSlot
derive instance ordDemoTabsSlot :: Ord DemoTabsSlot
cpDemoTabs :: CP.ChildPath DemoTabs.Query ChildQuery DemoTabsSlot ChildSlot
cpDemoTabs =
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpL

data DemoTogglesSlot = DemoTogglesSlot
derive instance eqDemoTogglesSlot :: Eq DemoTogglesSlot
derive instance ordDemoTogglesSlot :: Ord DemoTogglesSlot
cpDemoToggles :: CP.ChildPath DemoToggles.Query ChildQuery DemoTogglesSlot ChildSlot
cpDemoToggles =
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :> CP.cpR :>
  CP.cpR :> CP.cpR :> CP.cpR :> CP.cpL

type DemoContainerHTML = H.ParentHTML Query ChildQuery ChildSlot Aff
type DemoContainerDSL = H.ParentDSL State Query ChildQuery ChildSlot Message Aff

init :: State -> Input
init state = Initialize state

demoContainer :: H.Component HH.HTML Query Input Message Aff
demoContainer =
  H.lifecycleParentComponent
    { initialState: initialState
    , initializer: initializer
    , finalizer: finalizer
    , receiver: receiver
    , render
    , eval
    }
  where

  layoutRef :: H.RefLabel
  layoutRef = H.RefLabel "mdl-layout-ref"

  drawerRef :: H.RefLabel
  drawerRef = H.RefLabel "mdl-layout-drawer"

  initialState :: Input -> State
  initialState (Initialize state) = state

  initializer :: Maybe (Query Unit)
  initializer = Just $ H.action InitializeComponent

  finalizer :: Maybe (Query Unit)
  finalizer = Just $ H.action FinalizeComponent

  receiver :: Input -> Maybe (Query Unit)
  receiver (Initialize state) = Just $ H.action $ UpdateState state

  render :: State -> DemoContainerHTML
  render state =
    HH.div
      [ HP.class_ Layout.cl.layoutContainer ]
      [ HH.div
          [ HP.classes [ Layout.cl.layout, Layout.cl.jsLayout, Layout.cl.layoutFixedHeader ]
          , HP.ref layoutRef
          ]
          [ renderLayoutHeader
          , renderLayoutDrawer
          , renderLayoutContent state
          , renderSpacer
          , renderMegaFooter
          ]
      ]

  renderLayoutHeader :: DemoContainerHTML
  renderLayoutHeader =
    HH.header
      [ HP.classes [ Layout.cl.layoutHeader ] ]
      [ HH.div
        [ HP.classes [ Layout.cl.layoutHeaderRow ] ]
        [ HH.span [ HP.classes [ Layout.cl.layoutTitle] ] [ HH.text "Halogen MDL" ]
        , HH.div [ HP.classes [ Layout.cl.layoutSpacer ] ] []
        , HH.nav
          [ HP.classes [ Navigation.cl.navigation, Layout.cl.layoutLargeScreenOnly ] ]
          [ renderLayoutHeaderLink { href: "#", text: "Link 1" }
          , renderLayoutHeaderLink { href: "#", text: "Link 2" }
          , renderLayoutHeaderLink { href: "#", text: "Link 3" }
          , renderLayoutHeaderLink { href: "#", text: "Link 4" }
          ]
        ]
      ]

  renderLayoutHeaderLink :: Basic.BasicLink -> DemoContainerHTML
  renderLayoutHeaderLink link =
    HH.a
      [ HP.href link.href, HP.classes [ Navigation.cl.navigationLink ] ]
      [ HH.text link.text ]

  renderLayoutDrawer :: DemoContainerHTML
  renderLayoutDrawer =
    HH.div
      [ HP.classes [ Layout.cl.layoutDrawer ]
      , HP.ref drawerRef
      ]
      [ HH.span
        [ HP.classes [ Layout.cl.layoutTitle ] ]
        [ HH.text "Halogen MDL" ]
      , HH.nav
        [ HP.classes [ Navigation.cl.navigation ] ]
        [ renderLayoutDrawerLink Home
        , renderLayoutDrawerLink Badges
        , renderLayoutDrawerLink Buttons
        , renderLayoutDrawerLink Cards
        , renderLayoutDrawerLink Chips
        , renderLayoutDrawerLink Dialogs
        , renderLayoutDrawerLink Lists
        , renderLayoutDrawerLink Menus
        , renderLayoutDrawerLink Progress
        , renderLayoutDrawerLink Sliders
        , renderLayoutDrawerLink Snackbars
        , renderLayoutDrawerLink Spinners
        , renderLayoutDrawerLink Tabs
        , renderLayoutDrawerLink Toggles
        ]
      ]

  renderLayoutDrawerLink :: Route -> DemoContainerHTML
  renderLayoutDrawerLink route =
    HH.a
      [ HP.href $ Route.href route
      , HP.classes [ Navigation.cl.navigationLink ]
      , HE.onClick $ HE.input_ OnNavClick
      ]
      [ HH.text $ Route.label route ]

  renderLayoutContent :: State -> DemoContainerHTML
  renderLayoutContent state =
    HH.div
      [ HP.classes [ Layout.cl.layoutContent ] ]
      [ HH.div
        [ HP.classes [ HH.ClassName "page-content" ] ]
        [ renderPageContent state
        --, renderSpacer
        --, renderMegaFooter
        ]
      ]

  renderPageContent :: State -> DemoContainerHTML
  renderPageContent state = case state.currentRoute of
    Home ->
      HH.slot'
        cpDemoHome
        DemoHomeSlot
        DemoHome.demoHome
        (DemoHome.init unit)
        (HE.input OnDemoHomeMessage)
    Badges ->
      HH.slot'
        cpDemoBadges
        DemoBadgesSlot
        DemoBadges.demoBadges
        (DemoBadges.init unit)
        (HE.input OnDemoBadgesMessage)
    Buttons ->
      HH.slot'
        cpDemoButtons
        DemoButtonsSlot
        DemoButtons.demoButtons
        (DemoButtons.init { clickDemo: { clickCount: 0 }, nonComponentDemo: { isLoading: false } })
        (HE.input OnDemoButtonsMessage)
    Cards ->
      HH.slot'
        cpDemoCards
        DemoCardsSlot
        DemoCards.demoCards
        (DemoCards.init unit)
        (HE.input OnDemoCardsMessage)
    Chips ->
      HH.slot'
        cpDemoChips
        DemoChipsSlot
        DemoChips.demoChips
        (DemoChips.init unit)
        (HE.input OnDemoChipsMessage)
    Dialogs ->
      HH.slot'
        cpDemoDialogs
        DemoDialogsSlot
        DemoDialogs.demoDialogs
        (DemoDialogs.init unit)
        (HE.input OnDemoDialogsMessage)
    Lists ->
      HH.slot'
        cpDemoLists
        DemoListsSlot
        DemoLists.demoLists
        (DemoLists.init unit)
        (HE.input OnDemoListsMessage)
    Menus ->
      HH.slot'
        cpDemoMenus
        DemoMenusSlot
        DemoMenus.demoMenus
        (DemoMenus.init unit)
        (HE.input OnDemoMenusMessage)
    Progress ->
      HH.slot'
        cpDemoProgress
        DemoProgressSlot
        DemoProgress.demoProgress
        (DemoProgress.init { progress: 0 })
        (HE.input OnDemoProgressMessage)
    Sliders ->
      HH.slot'
        cpDemoSliders
        DemoSlidersSlot
        DemoSliders.demoSliders
        (DemoSliders.init { slider1: 0.0, slider2: 20.0, slider3: 20.0 })
        (HE.input OnDemoSlidersMessage)
    Snackbars ->
      HH.slot'
        cpDemoSnackbars
        DemoSnackbarsSlot
        DemoSnackbars.demoSnackbars
        (DemoSnackbars.init { snackbar2ActionCount: 0 })
        (HE.input OnDemoSnackbarsMessage)
    Spinners ->
      HH.slot'
        cpDemoSpinners
        DemoSpinnersSlot
        DemoSpinners.demoSpinners
        (DemoSpinners.init unit)
        (HE.input OnDemoSpinnersMessage)
    Tabs ->
      HH.slot'
        cpDemoTabs
        DemoTabsSlot
        DemoTabs.demoTabs
        (DemoTabs.init { currentTab: DemoTabs.About })
        (HE.input OnDemoTabsMessage)
    Toggles ->
      HH.slot'
        cpDemoToggles
        DemoTogglesSlot
        DemoToggles.demoToggles
        (DemoToggles.init)
        (HE.input OnDemoTogglesMessage)

  renderMegaFooter :: DemoContainerHTML
  renderMegaFooter =
    MegaFooter.bl.megaFooter
      { middleSection:
          { dropDownSections: dummyDropDownSection <$> (1 .. 4) }
      , bottomSection:
          { title: "Bottom title"
          , linkList: dummyLinkList
          }
      }

  renderSpacer :: DemoContainerHTML
  renderSpacer =
    HH.div
      [ HP.class_ Layout.cl.layoutSpacer ]
      []

  dummyDropDownSection :: Int -> MegaFooter.DropDownSectionBlock
  dummyDropDownSection i =
    { title: "Drop down section " <> show i
    , linkList: dummyLinkList
    }

  dummyLinkList :: MegaFooter.LinkListBlock
  dummyLinkList =
    { links:
        [ { href: "#", text: "Link 1" }
        , { href: "#", text: "Link 2" }
        , { href: "#", text: "Link 3" }
        , { href: "#", text: "Link 4" }
        ]
    }

  eval :: Query ~> DemoContainerDSL
  eval = case _ of
    InitializeComponent next -> do
      MDL.upgradeElementByRef layoutRef
      pure next
    FinalizeComponent next -> do
      pure next
    UpdateState state next -> do
      H.put state
      pure next
    UpdateRoute route next -> do
      H.modify_ (\state -> state { currentRoute = route })
      pure next
    OnNavClick next -> do
      maybeDrawer <- H.getHTMLElementRef drawerRef
      case maybeDrawer of
        Just drawer -> H.liftEffect $ Layout.toggleDrawer
        Nothing -> pure unit
      pure next
    OnDemoHomeMessage _ next -> do
      pure next
    OnDemoBadgesMessage _ next -> do
      pure next
    OnDemoButtonsMessage _ next -> do
      pure next
    OnDemoCardsMessage _ next -> do
      pure next
    OnDemoChipsMessage _ next -> do
      pure next
    OnDemoDialogsMessage _ next -> do
      pure next
    OnDemoListsMessage _ next -> do
      pure next
    OnDemoMenusMessage _ next -> do
      pure next
    OnDemoProgressMessage _ next -> do
      pure next
    OnDemoSlidersMessage _ next -> do
      pure next
    OnDemoSnackbarsMessage _ next -> do
      pure next
    OnDemoSpinnersMessage _ next -> do
      pure next
    OnDemoTabsMessage _ next -> do
      pure next
    OnDemoTogglesMessage _ next -> do
      pure next

