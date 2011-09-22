{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , DeriveDataTypeable
           , KindSignatures
           , RankNTypes
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
           , FlexibleContexts
           , FlexibleInstances
           , UndecidableInstances
           , GADTs
           , EmptyDataDecls
           , CPP
  #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Safe
-- Copyright   :  (c) 2009–2010 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules provides the following guarantees for working with USB devices:
--
-- * You can't reference handles to devices that are closed. In other words: no
--   I/O with closed handles is possible.
--
-- * The programmer can specify the /region/ in which devices should remain
--   open. On exit from the region the opened devices will be closed
--   automatically.
--
-- * You can't reference handles to configurations that have not been set.
--
-- * You can't reference handles to interfaces that have not been claimed.
--
-- * Just like with devices, the programmer can specify the region in which
--   interfaces should remain claimed. On exit from the region the claimed
--   interfaces will be released automatically.
--
-- * You can't reference handles to alternates that have not been set.
--
-- * You can't reference endpoints that don't belong to a setted alternate.
--
-- * You can't read from an endpoint with an 'Out' transfer direction.
--
-- * You can't write to an endpoint with an 'In' transfer direction.
--
-- * You can't read from or write to endpoints with the unsupported transfer
--   types 'Control' and 'Isochronous'. Only I/O with endpoints with the
--   supported 'Bulk' and 'Interrupt' transfer types is allowed.
--
-- This modules makes use of a technique called /Lightweight monadic regions/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
-- This technique is implemented in the @regions@ package of which the
-- @Control.Monad.Trans.Region@ module is re-exported by this module.
--
-- See the @usb-safe-examples@ package for examples how to use this library:
--
-- @darcs get@ <http://code.haskell.org/~basvandijk/code/usb-safe-examples>
--
--------------------------------------------------------------------------------

module System.USB.Safe
    ( -- * USB devices as scarce resources

      {-|
      Note that this module re-exports the @Control.Monad.Trans.Region@ module
      from the @regions@ package which allows you to run regions using 'runRegionT'
      and duplicate a 'RegionalDeviceHandle' to a parent region using 'dup'.
      -}
      module Control.Monad.Trans.Region

      -- ** Regional device handles
    , RegionalDeviceHandle

    , openDevice
    , withDevice
    , withDeviceWhich

    , getDevice

      -- * Getting descriptors
    , GetDescriptor(getDesc)

      -- * Resetting devices
    , resetDevice

      -- * Configurations
    , Config
    , getConfigs

      -- ** Setting configurations
    , ConfigHandle
    , setConfig,       SettingAlreadySet(SettingAlreadySet)
    , useActiveConfig, NoActiveConfig(NoActiveConfig)
    , setConfigWhich

      -- * Interfaces
    , Interface
    , getInterfaces

      -- ** Claiming interfaces
    , RegionalInterfaceHandle
    , claim
    , withInterface
    , withInterfaceWhich

      -- * Alternates
    , Alternate
    , getAlternates

      -- ** Setting alternates
    , AlternateHandle
    , setAlternate
    , useActiveAlternate
    , setAlternateWhich

      -- * Endpoints
    , Endpoint
    , getEndpoints, getEndpoints'

      -- *** Transfer directions
    , TransferDirection(..)

    , Out
    , In

    , MkTransferDirection(..)

      -- *** Transfer types
    , TransferType(..)

    , Control
    , Isochronous
    , Bulk
    , Interrupt

    , MkTransferType(..)

      -- * Endpoint I/O
    , clearHalt

    , ReadAction
    , WriteAction

    , ReadEndpoint(readEndpoint)
    , WriteEndpoint(writeEndpoint)

    , EnumReadEndpoint(enumReadEndpoint)

      -- ** Control transfers
    , ControlAction
    , RequestType(..)
    , control
    , readControl, readControlExact
    , writeControl

      -- * String descriptors
    , getLanguages
    , getStrDesc
    , getStrDescFirstLang

      -- * USB kernel drivers
    , kernelDriverActive
    , detachKernelDriver
    , attachKernelDriver
    , withDetachedKernelDriver
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Control.Concurrent.MVar    ( MVar, newMVar, takeMVar, putMVar, withMVar )
import Control.Monad              ( Monad, return, (>>=), (>>), liftM )
import Control.Exception          ( Exception, bracket_ )
import Data.Typeable              ( Typeable )
import Data.Function              ( ($) )
import Data.Word                  ( Word8 )
import Data.Int                   ( Int )
import Data.Bool                  ( Bool(True, False) )
import Data.List                  ( map, head, filter, find )
import Data.Maybe                 ( Maybe(Nothing, Just), fromJust )
import System.IO                  ( IO )
import Text.Show                  ( Show )

#if __GLASGOW_HASKELL__ < 700
import Prelude                    ( fromInteger )
import Control.Monad              ( fail )
#endif

-- from base-unicode-symbols:
import Data.Bool.Unicode          ( (∧) )
import Data.Eq.Unicode            ( (≡) )
import Data.Function.Unicode      ( (∘) )

-- from bytestring:
import Data.ByteString            ( ByteString )

-- from text:
import Data.Text                  ( Text )

-- from transformers:
import Control.Monad.IO.Class     ( MonadIO, liftIO )

-- from monad-control:
import Control.Monad.IO.Control   ( MonadControlIO )
import Control.Exception.Control  ( throwIO )

-- from regions:
import Control.Monad.Trans.Region.OnExit ( FinalizerHandle, onExit )
import Control.Monad.Trans.Region.Unsafe ( unsafeControlIO, unsafeLiftIOOp_ )
import Control.Monad.Trans.Region     -- (re-exported entirely)

-- from iteratee:
import Data.Iteratee.Iteratee           ( Enumerator )
import Data.Iteratee.Base.ReadableChunk ( ReadableChunk )
import Data.NullPoint                   ( NullPoint )

-- from usb:
import qualified System.USB.Initialization as USB
    ( Ctx )

import qualified System.USB.Enumeration as USB
    ( Device, getDevices, deviceDesc )

import qualified System.USB.DeviceHandling as USB
    ( DeviceHandle, openDevice, closeDevice, getDevice
    , getConfig, setConfig
    , InterfaceNumber, claimInterface, releaseInterface
    , setInterfaceAltSetting
    , clearHalt, resetDevice
    , kernelDriverActive
    , detachKernelDriver, attachKernelDriver
    , withDetachedKernelDriver
    )

import qualified System.USB.Descriptors as USB
    ( DeviceDesc, deviceConfigs
    , ConfigDesc, configValue, configInterfaces
    , Interface
    , InterfaceDesc, interfaceNumber, interfaceAltSetting, interfaceEndpoints
    , EndpointDesc, endpointAddress, endpointAttribs
    , EndpointAddress, transferDirection
    , TransferDirection(In, Out)
    , TransferType(Control, Isochronous, Bulk, Interrupt)
    , getLanguages, LangId, StrIx, getStrDesc, getStrDescFirstLang
    )

import qualified System.USB.IO as USB
    ( Timeout, Status, Size
    , RequestType(Class, Vendor)
    , Recipient, Request, Value, Index
    , control, readControl, readControlExact, writeControl
    , readBulk,  readInterrupt
    , writeBulk, writeInterrupt
    )

import qualified System.USB.IO.StandardDeviceRequests as USB
    ( getInterfaceAltSetting )

import qualified System.USB.Exceptions as USB
    ( USBException(..) )

#ifdef __HADDOCK__
import System.USB.Descriptors ( maxPacketSize, endpointMaxPacketSize )
#endif

-- from usb-enumerator:
import qualified System.USB.IO.Iteratee as USB
    ( enumReadBulk, enumReadInterrupt )

#if MIN_VERSION_base(4,3,0)
import Control.Exception ( mask_ )
#else
import Control.Exception ( block )
mask_ ∷ IO α → IO α
mask_ = block
#endif


--------------------------------------------------------------------------------
-- ** Regional device handles
--------------------------------------------------------------------------------

{-| A regional handle to an opened USB device.

A regional handle to an opened USB device can be created by applying
'openDevice' or 'withDevice' to the USB device you wish to open.

Note that you can also /duplicate/ a regional device handle by applying 'dup' to it.
-}
data RegionalDeviceHandle (r ∷ * → *) = RegionalDeviceHandle !(USB.DeviceHandle)
                                                             !(MVar Bool)
                                                             !(FinalizerHandle r)

instance Dup RegionalDeviceHandle where
    dup (RegionalDeviceHandle h mv ch) = liftM (RegionalDeviceHandle h mv) $ dup ch

{-| Open a device and obtain a regional device handle.
The device is automatically closed when the region terminates.

This is a non-blocking function; no requests are sent over the bus.

Exceptions:

 * 'USB.NoMemException' if there is a memory allocation failure.

 * 'USB.AccessException' if the user has insufficient permissions.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
openDevice ∷ MonadControlIO pr
           ⇒ USB.Device → RegionT s pr (RegionalDeviceHandle (RegionT s pr))
openDevice dev = unsafeLiftIOOp_ mask_ $ do
                   h  ← liftIO $ USB.openDevice dev
                   mv ← liftIO $ newMVar False
                   ch ← onExit $ USB.closeDevice h
                   return $ RegionalDeviceHandle h mv ch

{-| Convenience function which opens the device, applies the given continuation
function to the resulting regional device handle and runs the resulting region.
-}
withDevice ∷ MonadControlIO pr
           ⇒ USB.Device
           → (∀ s. RegionalDeviceHandle (RegionT s pr) → RegionT s pr α)
           → pr α
withDevice dev f = runRegionT $ openDevice dev >>= f

{-| Convenience function which finds the first device attached to the system
which satisfies the given predicate on its descriptor, then opens that device
and applies the given continuation function to the resulting device handle.

Exceptions:

 * 'USB.NotFoundException' if no device is found which satisfies the given predicate.

 * 'USB.NoMemException' if there is a memory allocation failure.

 * 'USB.AccessException' if the user has insufficient permissions.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
withDeviceWhich ∷ ∀ pr α
                . MonadControlIO pr
                ⇒ USB.Ctx
                → (USB.DeviceDesc → Bool) -- ^ Predicate on the device descriptor.
                → (∀ s. RegionalDeviceHandle (RegionT s pr) → RegionT s pr α)
                                          -- ^ Continuation function
                → pr α
withDeviceWhich ctx p f = do
  devs ← liftIO $ USB.getDevices ctx
#if __GLASGOW_HASKELL__ < 700
  useWhich devs withDevice p f
#else
    -- GHC-7 simplified the treatment of impredicativity
    -- which unfortunately causes the above to break.
    -- See: http://thread.gmane.org/gmane.comp.lang.haskell.glasgow.user/19134/focus=19153
    -- A solution is to just inline the code of useWhich: (I know this is ugly!)
  case find (p ∘ getDesc) devs of
    Nothing  → throwIO USB.NotFoundException
    Just dev → withDevice dev f
#endif

-- | Internally used function which searches through the given list of USB
-- entities (like Devices, Configs, Interfaces or Alternates) for the first
-- entity which satisfies the given predicate on its descriptor. Then opens or
-- sets that entity by applying the given open or set function to the entity.
useWhich ∷ ∀ k desc e (m ∷ * → *) α
         . (GetDescriptor e desc, MonadIO m)
         ⇒ [e]           -- ^
         → (e → k → m α) -- ^ With
         → (desc → Bool) -- ^ Predicate on descriptor
         → k             -- ^ Continuation function
         → m α
useWhich ds w p f = case find (p ∘ getDesc) ds of
                      Nothing → throwIO USB.NotFoundException
                      Just d  → w d f

-- | Internally used function for getting the actual USB device handle from a
-- regional device handle.
getInternalDevHndl ∷ RegionalDeviceHandle r → USB.DeviceHandle
getInternalDevHndl (RegionalDeviceHandle h _ _) = h

-- | Convenience function for retrieving the device from the given regional handle.
getDevice ∷ RegionalDeviceHandle r → USB.Device
getDevice = USB.getDevice ∘ getInternalDevHndl


--------------------------------------------------------------------------------
-- * Getting descriptors
--------------------------------------------------------------------------------

class GetDescriptor α desc | α → desc, desc → α where
    -- | Get the descriptor of a given USB entity.
    getDesc ∷ α → desc

instance GetDescriptor USB.Device USB.DeviceDesc where
    getDesc = USB.deviceDesc


--------------------------------------------------------------------------------
-- * Resetting devices
--------------------------------------------------------------------------------

{-| Perform a USB port reset to reinitialize a device.
The system will attempt to restore the previous configuration
and alternate settings after the reset has completed.

Note the constraint: @pr \`AncestorRegion\` cr@. This allows this function to be
executed in any child region @cr@ of the parent region @pr@ in which the given
regional handle was created.

You can only reset a device when all computations passed to 'setConfig',
'useActiveConfig' and 'setConfigWhich' have been terminated. If you call
@resetDevice@ and such a computation is still running a 'SettingAlreadySet'
exception is thrown.

If the reset fails, the descriptors change, or the previous state cannot be
restored, the device will appear to be disconnected and reconnected. This means
that the device handle is no longer valid (you should close it) and rediscover
the device. A 'USB.NotFoundException' is raised to indicate that this is the case.

/TODO: Think about how to handle the implications of the the previous paragraph!/

This is a blocking function which usually incurs a noticeable delay.

Exceptions:

 * 'SettingAlreadySet' if a configuration has been set using 'setConfig',
   'useActiveConfig' and 'setConfigWhich'.

 * 'USB.NotFoundException' if re-enumeration is required, or if the
   device has been disconnected.

 * Another 'USB.USBException'.
-}
resetDevice ∷ (pr `AncestorRegion` cr, MonadIO cr)
            ⇒ RegionalDeviceHandle pr → cr ()
resetDevice (RegionalDeviceHandle internalDevHndl mv _) =
    liftIO $ withMVar mv $ \configAlreadySet → if configAlreadySet
                                               then throwIO SettingAlreadySet
                                               else USB.resetDevice internalDevHndl


--------------------------------------------------------------------------------
-- * Configurations
--------------------------------------------------------------------------------

{-| A supported configuration of a USB device
parameterized by the region @r@ in which it was created.

Note that, just like a regional device handle, a configuration can be duplicated
to a parent region using 'dup'.

Also note that you can get the descriptor of the configuration
by applying 'getDesc' to it.
-}
data Config (r ∷ * → *) = Config !(RegionalDeviceHandle r) !USB.ConfigDesc

instance GetDescriptor (Config r) USB.ConfigDesc where
    getDesc (Config _ configDesc) = configDesc

instance Dup Config where
    dup (Config regionalDevHndlC configDesc) = do
      -- Duplicating a configuration just means duplicating the associated
      -- regional device handle:
      regionalDevHndlP ← dup regionalDevHndlC

      -- And returning a new configuration with the same parameters of the given
      -- one but with a type that is parameterized by the parent region:
      return $ Config regionalDevHndlP configDesc

{-| Retrieve the supported configurations from the given regional handle.

Note that the configuration is parameterized by the same region @r@ in which the
regional handle was created. This ensures you can never use a configuration
outside that region.
-}
getConfigs ∷ RegionalDeviceHandle r → [Config r]
getConfigs regionalDevHndl = map (Config regionalDevHndl) $ getConfigDescs $
                               getInternalDevHndl regionalDevHndl

-- | Internally used function for getting all the configuration descriptors of
-- the given device.
getConfigDescs ∷ USB.DeviceHandle → [USB.ConfigDesc]
getConfigDescs = USB.deviceConfigs ∘ USB.deviceDesc ∘ USB.getDevice


--------------------------------------------------------------------------------
-- ** Setting configurations
--------------------------------------------------------------------------------

{-| A handle to an active 'Config'
which you can get using: 'setConfig', 'useActiveConfig' or 'setConfigWhich'.

The type variable @sCfg@ is used to ensure that
you can't return this handle from these functions.
-}
data ConfigHandle sCfg = ConfigHandle !USB.DeviceHandle !USB.ConfigDesc

{-| Set the active configuration for a device and then
apply the given continuation function to the resulting configuration handle.

USB devices support multiple configurations of which only one can be active at
any given time. When a configuration is set using 'setConfig', 'useActiveConfig'
or 'setConfigWhich' no threads may set a new configuration until the computation
passed to these functions terminates. If you do try to set one, a
'SettingAlreadySet' exception will be thrown.

The operating system may or may not have already set an active configuration on
the device. It is up to your application to ensure the correct configuration is
selected before you attempt to claim interfaces and perform other operations. If
you want to use the current active configuration: 'useActiveConfig'.

If you call this function on a device already configured with the selected
configuration, then this function will act as a lightweight device reset: it
will issue a @SET_CONFIGURATION@ request using the current configuration, causing
most USB-related device state to be reset (altsetting reset to zero, endpoint
halts cleared, toggles reset).

You cannot change/reset configuration if other applications or drivers have
claimed interfaces.

This is a blocking function.

Exceptions:

 * 'SettingAlreadySet' if a configuration has already been set using
   'setConfig', 'useActiveConfig' or 'setConfigWhich'.

 * 'USB.BusyException' if interfaces are currently claimed.

 * 'USB.NoDeviceException' if the device has been disconnected

 * Another 'USB.USBException'.
-}
setConfig ∷ ∀ pr cr s α
          . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
          ⇒ Config pr -- ^ The configuration you wish to set.
          → (∀ sCfg. ConfigHandle sCfg → RegionT s cr α) -- ^ Continuation function.
          → RegionT s cr α
setConfig (Config (RegionalDeviceHandle internalDevHndl mv _) configDesc) f =
    unsafeControlIO $ \runInIO → withUnsettedMVar mv $ do
      USB.setConfig internalDevHndl $ Just $ USB.configValue configDesc
      runInIO $ f $ ConfigHandle internalDevHndl configDesc

-- | Internally used function which throws a 'SettingAlreadySet' exception if
-- the given @MVar@ was set. If the given @MVar@ wasn't set it will be set
-- before the given computation is performed. When the computation terminates,
-- wheter normally or by raising an exception, the @MVar@ will be unset again.
withUnsettedMVar ∷ MVar Bool → IO α → IO α
withUnsettedMVar mv = bracket_ (do alreadySet ← takeMVar mv
                                   if alreadySet
                                     then do putMVar mv alreadySet
                                             throwIO SettingAlreadySet
                                     else putMVar mv True)
                               (overwriteMVar mv False)

-- | Internally used function which sets the @MVar@ before the computation is
-- performed. When the computation terminates, wheter normally or by raising an
-- exception, the @MVar@ will be unset again.
withSettedMVar ∷ MVar Bool → IO α → IO α
withSettedMVar mv = bracket_ (overwriteMVar mv True) (overwriteMVar mv False)

-- | Internally used function which overwrites the value in the @MVar@.
--
-- Note that because this function is used in a context in which asynchronous
-- exceptions are already masked I don't mask them again. So be carefull
-- when using this function in any other context!
overwriteMVar ∷ MVar α → α → IO ()
overwriteMVar mv x = takeMVar mv >> putMVar mv x

{-| This exception can be thrown in:

* 'resetDevice'

* 'setConfig' or 'setConfigWhich'

* 'setAlternate' or 'setAlternateWhich'

to indicate that the device was already configured with a setting.
-}
data SettingAlreadySet = SettingAlreadySet deriving (Show, Typeable)

instance Exception SettingAlreadySet

{-| Apply the given continuation function to the configuration handle of the
current active configuration of the given device handle.

This function needs to determine the current active configuration. This
information may be cached by the operating system. If it isn't cached this
function will block while a control transfer is submitted to retrieve the
information.

Exceptions:

 * 'NoActiveConfig' if the device is not configured.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
useActiveConfig ∷ ∀ pr cr s α
                . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
                ⇒ RegionalDeviceHandle pr -- ^ Regional handle to the device
                                          --   from which you want to use the
                                          --   active configuration.
                → (∀ sCfg. ConfigHandle sCfg → RegionT s cr α) -- ^ Continuation function
                → RegionT s cr α
useActiveConfig (RegionalDeviceHandle internalDevHndl mv _) f =
    unsafeControlIO $ \runInIO → withSettedMVar mv $ do
      mbActiveConfigValue ← USB.getConfig internalDevHndl
      case mbActiveConfigValue of
        Nothing → throwIO NoActiveConfig
        Just activeConfigValue →
          let activeConfigHandle = ConfigHandle internalDevHndl activeConfigDesc
              activeConfigDesc = fromJust $ find isActive $ getConfigDescs internalDevHndl
              isActive = (activeConfigValue ≡) ∘ USB.configValue
          in runInIO $ f activeConfigHandle

{-| This exception can be thrown in 'useActiveConfig' to indicate that the
device is currently not configured.
-}
data NoActiveConfig = NoActiveConfig deriving (Show, Typeable)

instance Exception NoActiveConfig

{-| Convenience function which finds the first configuration of
the given device handle which satisfies the given predicate on its descriptor,
then sets that configuration and applies the given function to the resulting
configuration handle.

This function calls 'setConfig' so do see its documentation.

Exceptions:

 * 'SettingAlreadySet' if a configuration has already been set using
   'setConfig', 'useActiveConfig' or 'setConfigWhich'.

 * 'USB.NotFoundException' if no configuration is found that satisfies the given
   predicate.

 * 'USB.BusyException' if interfaces are currently claimed.

 * 'USB.NoDeviceException' if the device has been disconnected

 * Another 'USB.USBException'.
-}
setConfigWhich ∷ ∀ pr cr s α
               . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
               ⇒ RegionalDeviceHandle pr -- ^ Regional handle to the device for
                                         --   which you want to set a
                                         --   configuration.
               → (USB.ConfigDesc → Bool) -- ^ Predicate on the configuration
                                         --   descriptor.
               → (∀ sCfg. ConfigHandle sCfg → RegionT s cr α) -- ^ Continuation function.
               → RegionT s cr α
setConfigWhich h = useWhich (getConfigs h) setConfig


--------------------------------------------------------------------------------
-- * Interfaces
--------------------------------------------------------------------------------

{-| A supported interface of a configuration
which you can retrieve using 'getInterfaces'.

To retrieve the 'USB.Interface' descriptors of an interface use 'getDesc'.
-}
data Interface sCfg = Interface !USB.DeviceHandle
                                !USB.InterfaceNumber
                                !USB.Interface

instance GetDescriptor (Interface sCfg) USB.Interface where
    getDesc (Interface _ _ ifDescs) = ifDescs

{-| Retrieve the supported interfaces from the configuration handle.

Note that the interface is parameterized by the @sCfg@ of the configuration
handle it is derived from. This ensures that it can never be returned from the
functions that created this configuration handle: 'setConfig', 'useActiveConfig'
and 'setConfigWhich'.

The latter is useful because outside those functions the active configuration
may change. If at that moment you still have an interface of the old
configuration claiming it would be an error.
-}
getInterfaces ∷ ConfigHandle sCfg → [Interface sCfg]
getInterfaces (ConfigHandle internalDevHndl configDesc) =
    map newIf $ USB.configInterfaces configDesc
        where
          newIf alts = Interface internalDevHndl
                                 (USB.interfaceNumber $ head alts)
                                 alts


--------------------------------------------------------------------------------
-- ** Interface regions
--------------------------------------------------------------------------------

{-| A regional handle to a claimed interface.

A regional handle to a claimed interface can be created
by applying 'claim' or 'withInterface' to the interface you wish to claim.
-}
data RegionalInterfaceHandle sCfg (r ∷ * → *) = RegionalInterfaceHandle
                                                  !(Interface sCfg)
                                                  !(MVar Bool)
                                                  !(FinalizerHandle r)

instance Dup (RegionalInterfaceHandle sCfg) where
    dup (RegionalInterfaceHandle interface mv ch) =
      liftM (RegionalInterfaceHandle interface mv) $ dup ch

{-| Claim the given interface in the region.
When the region terminates the interface is released automatically.

Note that it is allowed to claim an already-claimed interface.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

This is a non-blocking function.

Exceptions:

 * 'USB.BusyException' if another program or driver has claimed the interface.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
claim ∷ ∀ pr sCfg s
      . MonadControlIO pr
      ⇒ Interface sCfg  -- ^ Interface you wish to claim
      → RegionT s pr
          (RegionalInterfaceHandle sCfg
            (RegionT s pr))
claim interface@(Interface internalDevHndl ifNum _) = unsafeLiftIOOp_ mask_ $ do
  mv ← liftIO $ newMVar False
  liftIO $ USB.claimInterface internalDevHndl ifNum
  ch ← onExit $ USB.releaseInterface internalDevHndl ifNum
  return $ RegionalInterfaceHandle interface mv ch

withInterface ∷ ∀ pr sCfg α
              . MonadControlIO pr
              ⇒ Interface sCfg -- ^ The interface you wish to claim.
              → (∀ s. RegionalInterfaceHandle sCfg (RegionT s pr)
                    → RegionT s pr α
                ) -- ^ Continuation function.
              → pr α
withInterface interface f = runRegionT $ claim interface >>= f

{-| Convenience function which finds the first interface of the given
configuration handle which satisfies the given predicate on its descriptors,
then claims that interfaces and applies the given continuation function to the
resulting regional handle.

Exceptions:

 * 'USB.NotFoundException' if no interface was found that satisfies the fiven predicate.

 * 'USB.BusyException' if another program or driver has claimed the interface.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.

-}
withInterfaceWhich ∷ ∀ pr sCfg α
                   . MonadControlIO pr
                   ⇒ ConfigHandle sCfg      -- ^ Handle to a configuration of which
                                            --   you want to claim an interface.
                   → (USB.Interface → Bool) -- ^ Predicate on the interface descriptors.
                   → (∀ s. RegionalInterfaceHandle sCfg (RegionT s pr)
                         → RegionT s pr α
                     ) -- ^ Continuation function.
                   → pr α
withInterfaceWhich h = useWhich (getInterfaces h) withInterface


--------------------------------------------------------------------------------
-- * Alternates
--------------------------------------------------------------------------------

-- | A supported 'Interface' alternate setting
-- which you can retrieve using 'getAlternates'.
data Alternate sCfg (r ∷ * → *) = Alternate !(RegionalInterfaceHandle sCfg r)
                                            !USB.InterfaceDesc

instance GetDescriptor (Alternate sIntrf r) USB.InterfaceDesc where
    getDesc (Alternate _ ifDesc) = ifDesc

instance Dup (Alternate sCfg) where
    dup (Alternate regionalIfHndlC ifDesc) = do
      regionalIfHndlP ← dup regionalIfHndlC
      return $ Alternate regionalIfHndlP ifDesc

{-| Retrieve the supported alternate settings from the given interface handle.

Note that the alternate setting is parameterized by the same type variables as
the interface handle. This ensures you can never use an alternate setting
outside the region in which the interface handle was created.
-}
getAlternates ∷ RegionalInterfaceHandle sCfg r → [Alternate sCfg r]
getAlternates r@(RegionalInterfaceHandle (Interface _ _ alts) _ _) =
    map (Alternate r) alts


--------------------------------------------------------------------------------
-- ** Setting alternates
--------------------------------------------------------------------------------

{-| A handle to a setted alternate setting.

You get a handle to an alternate using
'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

The type variable @sAlt@ is used to ensure that
you can't return this handle from these functions.
-}
data AlternateHandle sAlt (r ∷ * → *) = AlternateHandle
                                          !USB.DeviceHandle
                                          !USB.InterfaceDesc

{-| Activate an alternate setting for an interface
and then apply the given continuation function to the resulting alternate handle.

Simillary to configurations, interfaces support multiple alternate settings of
which only one can be active at any given time. When an alternate is set using
'setAlternate', 'useActiveAlternate' or 'setAlternateWhich' no threads may set a
new alternate until the computation passed to these functions terminates. If you
do try to set one a 'SettingAlreadySet' exception will be thrown.

The operating system has always set an interface in one of the available
alternates. If you want to use the current active alternate:
'useActiveAlternate'.

This is a blocking function.

Exceptions:

 * 'USB.NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

 * Another 'USB.USBException'.
-}
setAlternate ∷ ∀ pr cr s sCfg α
             . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
             ⇒ Alternate sCfg pr -- ^ The alternate you wish to set.
             → (∀ sAlt. AlternateHandle sAlt pr → RegionT s cr α) -- ^ Continuation function.
             → RegionT s cr α
setAlternate (Alternate
              (RegionalInterfaceHandle (Interface internalDevHndl ifNum _) mv _)
              ifDesc) f =
  unsafeControlIO $ \runInIO → withUnsettedMVar mv $ do
    USB.setInterfaceAltSetting internalDevHndl
                                        ifNum
                                        (USB.interfaceAltSetting ifDesc)
    runInIO $ f $ AlternateHandle internalDevHndl ifDesc


{-| Apply the given function to the alternate handle of the current active
alternate of the given interface handle.

To determine the current active alternate this function will block while a
control transfer is submitted to retrieve the information.

Note that unlike configurations an interface is always set in one of the
available alternates, so unlike 'useActiveConfig' this function will never throw
an exception like 'NoActiveConfig'.

Exceptions:

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
useActiveAlternate ∷ ∀ pr cr s sCfg α
                   . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
                   ⇒ RegionalInterfaceHandle sCfg pr -- ^ Regional handle to the
                                              --   interface from which you want
                                              --   to use the active alternate.
                   → (∀ sAlt. AlternateHandle sAlt pr → RegionT s cr α) -- ^ Continuation function.
                   → RegionT s cr α
useActiveAlternate (RegionalInterfaceHandle (Interface internalDevHndl ifNum alts)
                                            mv _
                   ) f =
    unsafeControlIO $ \runInIO → withSettedMVar mv $ do
      let timeout = 5000 -- ms
      activeAltValue ← USB.getInterfaceAltSetting internalDevHndl
                                                  ifNum
                                                  timeout
      let activeAltHandle = AlternateHandle internalDevHndl activeAlt
          activeAlt = fromJust $ find isActive alts
          isActive = (activeAltValue ≡) ∘ USB.interfaceAltSetting
      runInIO $ f activeAltHandle


{-| Convenience function which finds the first alternate of the given interface
handle which satisfies the given predicate on its descriptor, then sets that
alternate and applies the given function to the resulting alternate handle.

This function calls 'setAlternate' so do see its documentation.

Exceptions:

 * 'USB.NotFoundException' if no alternate is found that satisfies the given
   predicate.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

 * Another 'USB.USBException'.
-}
setAlternateWhich ∷ ∀ pr cr sCfg s α
                  . (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
                  ⇒ RegionalInterfaceHandle sCfg pr -- ^ Regional handle to the
                                                    --   interface for which you want
                                                    --   to set an alternate.
                  → (USB.InterfaceDesc → Bool)      -- ^ Predicate on the interface
                                                    --   descriptor.
                  → (∀ sAlt. AlternateHandle sAlt pr → RegionT s cr α) -- ^ Continuation function
                  → RegionT s cr α
setAlternateWhich h = useWhich (getAlternates h) setAlternate


--------------------------------------------------------------------------------
-- * Endpoints
--------------------------------------------------------------------------------

{-| I/O operations on endpoints are type-safe. You can only read from an
endpoint with an 'In' transfer direction and you can only write to an endpoint
with an 'Out' transfer direction.

Reading and writing also have different implementations for the different
endpoint transfer types like: 'Bulk' and 'Interrupt'. I/O with endpoints of
other transfer types like 'Control' and 'Isochronous' is not possible.

This type lifts the transfer direction and transfer type information to the
type-level so that I/O operations like 'readEndpoint' and 'writeEndpoint' can
specify which endpoints they support.

You can retrieve the endpoints of an alternate using 'getEndpoints'.
-}
data Endpoint transDir transType sAlt (r ∷ * → *) =
     Endpoint !USB.DeviceHandle !USB.EndpointDesc

instance GetDescriptor (Endpoint transDir transType sAlt r) USB.EndpointDesc where
    getDesc (Endpoint _ endpointDesc) = endpointDesc

-- | Retrieve all the endpoints from the given alternate handle
-- which are of the given transfer direction and transfer type.
getEndpoints ∷ ∀ transDir transType sAlt r
             . AlternateHandle sAlt r     -- ^ Handle to the alternate from
                                          --   which you want to retrieve its
                                          --   endpoints.
             → TransferDirection transDir -- ^ Filter all endpoints which have
                                          --   this transfer direction.
             → TransferType transType     -- ^ Filter all endpoints which have
                                          --   this transfer type.
             → [Endpoint transDir transType sAlt r]
getEndpoints (AlternateHandle internalDevHndl ifDesc) transDir transType =
    map (Endpoint internalDevHndl) $ filter eqDirAndType
                                   $ USB.interfaceEndpoints ifDesc
    where
      eqDirAndType  endpointDesc =
         transDir  `eqDir`  transDirUSB
       ∧ transType `eqType` transTypeUSB
        where
         transDirUSB  = USB.transferDirection $ USB.endpointAddress endpointDesc
         transTypeUSB = USB.endpointAttribs endpointDesc

-- | Similar to 'getEndpoints' but will retrieve the endpoints based on the
-- inferred type of transfer direction and transfer type.
--
-- Note that:
-- @getEndpoints' altHndl = 'getEndpoints' altHndl 'mkTransferDirection' 'mkTransferType'@.
getEndpoints' ∷ ∀ transDir transType sAlt r
              . MkTransferDirection transDir
              ⇒ MkTransferType transType
              ⇒ AlternateHandle sAlt r
              → [Endpoint transDir transType sAlt r]
getEndpoints' altHndl = getEndpoints altHndl mkTransferDirection mkTransferType

--------------------------------------------------------------------------------
-- *** Transfer directions
--------------------------------------------------------------------------------

data TransferDirection transDir where
    Out ∷ TransferDirection Out
    In  ∷ TransferDirection In

-- | Out transfer direction (host -> device) used for writing.
data Out

-- | In transfer direction (device -> host) used for reading.
data In

class MkTransferDirection transDir where
    -- | An overloaded constructor function for transfer directions.
    mkTransferDirection ∷ TransferDirection transDir

instance MkTransferDirection Out where mkTransferDirection = Out
instance MkTransferDirection In  where mkTransferDirection = In

eqDir ∷ TransferDirection transDir → USB.TransferDirection → Bool
Out `eqDir` USB.Out = True
In  `eqDir` USB.In  = True
_   `eqDir` _       = False

--------------------------------------------------------------------------------
-- *** Transfer types
--------------------------------------------------------------------------------

data TransferType transType where
    Control     ∷ TransferType Control
    Isochronous ∷ TransferType Isochronous
    Bulk        ∷ TransferType Bulk
    Interrupt   ∷ TransferType Interrupt

data Control
data Isochronous
data Bulk
data Interrupt

class MkTransferType transType where
    -- | An overloaded constructor function for transfer types.
    mkTransferType ∷ TransferType transType

instance MkTransferType Control     where mkTransferType = Control
instance MkTransferType Isochronous where mkTransferType = Isochronous
instance MkTransferType Bulk        where mkTransferType = Bulk
instance MkTransferType Interrupt   where mkTransferType = Interrupt

eqType ∷ TransferType transType → USB.TransferType → Bool
Control     `eqType` USB.Control           = True
Isochronous `eqType` (USB.Isochronous _ _) = True
Bulk        `eqType` USB.Bulk              = True
Interrupt   `eqType` USB.Interrupt         = True
_           `eqType` _                     = False


--------------------------------------------------------------------------------
-- * Endpoint I/O
--------------------------------------------------------------------------------

{-| Clear the halt/stall condition for an endpoint.

Endpoints with halt status are unable to receive or transmit data
until the halt condition is stalled.

You should cancel all pending transfers before attempting to clear the halt condition.

This is a blocking function.

Exceptions:

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
clearHalt ∷ (pr `AncestorRegion` cr, MonadIO cr)
          ⇒ Endpoint transDir transType sAlt pr → cr ()
clearHalt (Endpoint internalDevHndl endpointDesc) =
    liftIO $ USB.clearHalt internalDevHndl $ USB.endpointAddress endpointDesc


--------------------------------------------------------------------------------

{-| Handy type synonym for read transfers.

A @ReadAction@ is a function which takes a size which defines how many bytes to
read and a timeout. The function returns an action which, when executed,
performs the actual read and returns the bytestring that was read paired with an
indication if the transfer timed out.
-}
type ReadAction r = USB.Size → USB.Timeout → r (ByteString, USB.Status)

-- | Class of transfer types that support reading.
class ReadEndpoint transType where
    {-| Read bytes from an 'In' endpoint
        with either a 'Bulk' or 'Interrupt' transfer type.

        Exceptions:

        * 'USB.PipeException' if the endpoint halted.

        * 'USB.OverflowException' if the device offered more data,
          see /Packets and overflows/ in the libusb documentation:
          <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

        * 'USB.NoDeviceException' if the device has been disconnected.

        * Another 'USB.USBException'.
    -}
    readEndpoint ∷ (pr `AncestorRegion` cr, MonadIO cr)
                 ⇒ Endpoint In transType sAlt pr
                 → ReadAction cr

instance ReadEndpoint Bulk      where readEndpoint = transferWith USB.readBulk
instance ReadEndpoint Interrupt where readEndpoint = transferWith USB.readInterrupt

transferWith ∷ (pr `AncestorRegion` cr, MonadIO cr)
             ⇒ (USB.DeviceHandle → USB.EndpointAddress → α → USB.Timeout → IO β)
             → (Endpoint transDir transType sAlt pr    → α → USB.Timeout → cr β)
transferWith f = \endpoint sbs timeout → liftIO $ wrap f endpoint sbs timeout

wrap ∷ (USB.DeviceHandle → USB.EndpointAddress → α)
     → (Endpoint transDir transType sAlt pr → α)
wrap f = \(Endpoint internalDevHndl endpointDesc) →
           f internalDevHndl $ USB.endpointAddress endpointDesc

--------------------------------------------------------------------------------

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes the bytestring to write and a
timeout. The function returns an action which, when exectued, returns the number
of bytes that were actually written paired with an indication if the transfer
timed out.
-}
type WriteAction r = ByteString → USB.Timeout → r (USB.Size, USB.Status)

-- | Class of transfer types that support writing
class WriteEndpoint transType where
    {-| Write bytes to an 'Out' endpoint
        with either a 'Bulk' or 'Interrupt' transfer type.

        Exceptions:

        * 'USB.PipeException' if the endpoint halted.

        * 'USB.NoDeviceException' if the device has been disconnected.

        * Another 'USB.USBException'.
    -}
    writeEndpoint ∷ (pr `AncestorRegion` cr, MonadIO cr)
                  ⇒ Endpoint Out transType sAlt pr
                  → WriteAction cr

instance WriteEndpoint Bulk      where writeEndpoint = transferWith USB.writeBulk
instance WriteEndpoint Interrupt where writeEndpoint = transferWith USB.writeInterrupt


--------------------------------------------------------------------------------

-- | Class of transfer types that support enumerating.
class EnumReadEndpoint transType where
    -- | An enumerator for an 'In' endpoint
    -- with either a 'Bulk' or 'Interrupt' transfer type.
    enumReadEndpoint ∷ ( pr `AncestorRegion` cr, MonadControlIO cr
                       , ReadableChunk s Word8, NullPoint s
                       )
                     ⇒ Endpoint In transType sAlt pr
                     → USB.Size    -- ^ Chunk size. A good value for this would be
                                   --   the @'maxPacketSize' . 'endpointMaxPacketSize'@.
                     → USB.Timeout -- ^ Timeout (in milliseconds) that this function
                                   --   should wait for each chunk before giving up
                                   --   due to no response being received.  For no
                                   --   timeout, use value 0.
                     → Enumerator s cr α

instance EnumReadEndpoint Bulk      where enumReadEndpoint = wrap USB.enumReadBulk
instance EnumReadEndpoint Interrupt where enumReadEndpoint = wrap USB.enumReadInterrupt


--------------------------------------------------------------------------------
-- ** Control transfers
--------------------------------------------------------------------------------

-- | Handy type synonym that names the parameters of a control transfer.
type ControlAction α = RequestType
                     → USB.Recipient
                     → USB.Request
                     → USB.Value
                     → USB.Index
                     → α

{-| Control transfers can have three request types: @Standard@, @Class@ and
@Vendor@. We disallow @Standard@ requests however because with them you can
destroy the safety guarantees that this module provides.
-}
data RequestType = Class | Vendor

reqTypeToInternal ∷ RequestType → USB.RequestType
reqTypeToInternal Class  = USB.Class
reqTypeToInternal Vendor = USB.Vendor

{-| Perform a USB /control/ request that does not transfer data.

Exceptions:

 * 'USB.TimeoutException' if the transfer timed out.

 * 'USB.PipeException' if the control request was not supported by the device

 * 'USB.NoDeviceException' if the device has been disconnected.

 *  Another 'USB.USBException'.
-}
control ∷ ∀ pr cr. (pr `AncestorRegion` cr, MonadIO cr)
        ⇒ RegionalDeviceHandle pr → ControlAction (USB.Timeout → cr ())
control regionalDevHndl = \reqType reqRecipient request value index → \timeout →
    liftIO $ USB.control (getInternalDevHndl regionalDevHndl)
                         (reqTypeToInternal reqType)
                         reqRecipient
                         request
                         value
                         index
                         timeout

{-| Perform a USB /control/ read.

Exceptions:

 * 'USB.PipeException' if the control request was not supported by the device

 * 'USB.NoDeviceException' if the device has been disconnected.

 *  Another 'USB.USBException'.
-}
readControl ∷ ∀ pr cr. (pr `AncestorRegion` cr, MonadIO cr)
            ⇒ RegionalDeviceHandle pr → ControlAction (ReadAction cr)
readControl regionalDevHndl = \reqType reqRecipient request value index → \timeout size →
    liftIO $ USB.readControl (getInternalDevHndl regionalDevHndl)
                             (reqTypeToInternal reqType)
                             reqRecipient
                             request
                             value
                             index
                             timeout
                             size

-- | A convenience function similar to 'readControl' which checks if the
-- specified number of bytes to read were actually read.
-- Throws an 'USB.IOException' if this is not the case.
readControlExact ∷ ∀ pr cr. (pr `AncestorRegion` cr, MonadIO cr)
                 ⇒ RegionalDeviceHandle pr → ControlAction
                     (USB.Size → USB.Timeout → cr ByteString)
readControlExact regionalDevHndl = \reqType reqRecipient request value index → \timeout size →
    liftIO $ USB.readControlExact (getInternalDevHndl regionalDevHndl)
                                  (reqTypeToInternal reqType)
                                  reqRecipient
                                  request
                                  value
                                  index
                                  timeout
                                  size

{-| Perform a USB /control/ write.

Exceptions:

 * 'USB.PipeException' if the control request was not supported by the device

 * 'USB.NoDeviceException' if the device has been disconnected.

 *  Another 'USB.USBException'.
-}
writeControl ∷ ∀ pr cr. (pr `AncestorRegion` cr, MonadIO cr)
             ⇒ RegionalDeviceHandle pr → ControlAction (WriteAction cr)
writeControl regionalDevHndl = \reqType reqRecipient request value index → \timeout input →
    liftIO $ USB.writeControl (getInternalDevHndl regionalDevHndl)
                              (reqTypeToInternal reqType)
                              reqRecipient
                              request
                              value
                              index
                              timeout
                              input


--------------------------------------------------------------------------------
-- *** Standard Device Requests
--------------------------------------------------------------------------------

{- TODO: Think about which of these to export:

setHalt ∷ DeviceHandle → EndpointAddress → Timeout → IO ()

clearRemoteWakeup ∷ DeviceHandle → Timeout → IO ()

setRemoteWakeup ∷ DeviceHandle → Timeout → IO ()

setStandardTestMode ∷ DeviceHandle → TestMode → Timeout → IO ()

getInterfaceAltSetting ∷ DeviceHandle
                       → InterfaceNumber
                       → Timeout
                       → IO InterfaceAltSetting

getDeviceStatus ∷ DeviceHandle → Timeout → IO DeviceStatus

getEndpointStatus ∷ DeviceHandle
                  → EndpointAddress
                  → Timeout
                  → IO Bool

setDeviceAddress ∷ DeviceHandle → Word16 → Timeout → IO ()

synchFrame ∷ DeviceHandle → EndpointAddress → Timeout → IO Int
-}

--------------------------------------------------------------------------------
-- * String descriptors
--------------------------------------------------------------------------------

{-| Retrieve a list of supported languages.

This function may throw 'USB.USBException's.
-}
getLanguages ∷ (pr `AncestorRegion` cr, MonadIO cr)
             ⇒ RegionalDeviceHandle pr → cr [USB.LangId]
getLanguages devHndl =
    liftIO $ USB.getLanguages (getInternalDevHndl devHndl)

{-| Retrieve a string descriptor from a device.

This function may throw 'USB.USBException's.

/TODO: The following can be made more type-safe!/

When I call 'getStrDesc' I would like the type system to guarantee that the
given @StrIx@ and @LangId@ actually belong to the given @Handle@. In other
words I would like to get a type error when they are some arbitrary number or
come from another device.
-}
getStrDesc ∷ (pr `AncestorRegion` cr, MonadIO cr)
           ⇒ RegionalDeviceHandle pr
           → USB.StrIx
           → USB.LangId
           → Int -- ^ Maximum number of characters in the requested string. An
                 --   'USB.IOException' will be thrown when the requested
                 --   string is larger than this number.
           → cr Text
getStrDesc devHndl strIx langId size =
    liftIO $ USB.getStrDesc (getInternalDevHndl devHndl) strIx langId size

{-| Retrieve a string descriptor from a device using the first supported language.

This function may throw 'USB.USBException's.
-}
getStrDescFirstLang ∷ (pr `AncestorRegion` cr, MonadIO cr)
                    ⇒ RegionalDeviceHandle pr
                    → USB.StrIx
                    → Int -- ^ Maximum number of characters in the requested
                          --   string. An 'USB.IOException' will be thrown when
                          --   the requested string is larger than this number.
                    → cr Text
getStrDescFirstLang devHndl descStrIx size =
    liftIO $ USB.getStrDescFirstLang (getInternalDevHndl devHndl) descStrIx size


--------------------------------------------------------------------------------
-- * USB kernel drivers
--------------------------------------------------------------------------------

{-| Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
kernelDriverActive ∷ (pr `AncestorRegion` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr Bool
kernelDriverActive regionalDevHndl =
    liftIO ∘ USB.kernelDriverActive (getInternalDevHndl regionalDevHndl)

{-| Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'USB.NotFoundException' if no kernel driver was active.

 * 'USB.InvalidParamException' if the interface does not exist.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
detachKernelDriver ∷ (pr `AncestorRegion` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr ()
detachKernelDriver regionalDevHndl =
    liftIO ∘ USB.detachKernelDriver (getInternalDevHndl regionalDevHndl)

{-| Re-attach an interface's kernel driver, which was previously
detached using 'detachKernelDriver'.

Exceptions:

 * 'USB.NotFoundException' if no kernel driver was active.

 * 'USB.InvalidParamException' if the interface does not exist.

 * 'USB.NoDeviceException' if the device has been disconnected.

 * 'USB.BusyException' if the driver cannot be attached because the interface
   is claimed by a program or driver.

 * Another 'USB.USBException'.
-}
attachKernelDriver ∷ (pr `AncestorRegion` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr ()
attachKernelDriver regionalDevHndl =
    liftIO ∘ USB.attachKernelDriver (getInternalDevHndl regionalDevHndl)

{-| If a kernel driver is active on the specified interface the driver is
detached and the given action is executed. If the action terminates, whether by
normal termination or by raising an exception, the kernel driver is attached
again. If a kernel driver is not active on the specified interface the action is
just executed.

Exceptions:

 * 'USB.NoDeviceException' if the device has been disconnected.

 * Another 'USB.USBException'.
-}
withDetachedKernelDriver ∷ (pr `AncestorRegion` RegionT s cr, MonadControlIO cr)
                         ⇒ RegionalDeviceHandle pr
                         → USB.InterfaceNumber
                         → RegionT s cr α
                         → RegionT s cr α
withDetachedKernelDriver regionalDevHndl ifNum =
    unsafeLiftIOOp_ $ USB.withDetachedKernelDriver
                        (getInternalDevHndl regionalDevHndl)
                        ifNum


-- The End ---------------------------------------------------------------------
