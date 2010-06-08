{-# LANGUAGE UnicodeSyntax
           , NoImplicitPrelude
           , DeriveDataTypeable
           , KindSignatures
           , RankNTypes
           , MultiParamTypeClasses
           , FunctionalDependencies
           , TypeSynonymInstances
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
      from the @regions@ package which allows you to:

      * Run regions using 'runRegionT'.

      * Concurrently run /top-level/ regions inside another region using
        'forkTopRegion'.

       * Duplicate a 'RegionalDeviceHandle' to a parent region using 'dup'.
      -}
      module Control.Monad.Trans.Region

      -- ** Regional device handles
    , RegionalDeviceHandle

    , openDevice
    , withDevice
    , withDeviceWhich, NotFound(NotFound)

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
    , getEndpoints
    , clearHalt

      -- *** Transfer directions
    , TransferDirection(..)
    , Out
    , In

      -- *** Transfer types
    , TransferType(..)
    , Control
    , Isochronous
    , Bulk
    , Interrupt

      -- * Endpoint I/O
    , ReadAction
    , WriteAction
    , readEndpoint
    , writeEndpoint

      -- ** Control transfers
    , RequestType(..)
    , control
    , readControl
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
import Prelude                    ( fromInteger )
import Control.Concurrent.MVar    ( MVar, newMVar, takeMVar, putMVar, withMVar )
import Control.Monad              ( Monad, return, (>>=), fail
                                  , (>>), when, liftM
                                  )
import Control.Exception          ( Exception, throwIO )
import Data.Typeable              ( Typeable )
import Data.Function              ( ($) )
import Data.Word                  ( Word8, Word16 )
import Data.Char                  ( String )
import Data.Bool                  ( Bool( True, False ) )
import Data.List                  ( map, head, filter, find )
import Data.Maybe                 ( Maybe( Nothing, Just ), fromJust )
import Text.Show                  ( Show )
import System.IO                  ( IO )

-- from base-unicode-symbols:
import Data.Bool.Unicode          ( (∧) )
import Data.Eq.Unicode            ( (≡) )
import Data.Function.Unicode      ( (∘) )

-- from bytestring:
import Data.ByteString            ( ByteString )

-- from transformers:
import Control.Monad.IO.Class     ( MonadIO, liftIO )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO      ( MonadCatchIO, bracket_, throw, block )

-- from regions:
import Control.Monad.Trans.Region.Close ( CloseHandle, register )
import Control.Monad.Trans.Region -- (re-exported entirely)

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
    , kernelDriverActive, detachKernelDriver, attachKernelDriver
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

import qualified System.USB.IO.Synchronous as USB
    ( Timeout, Size
    , RequestType(Class, Vendor)
    , Recipient
    , control, readControl, writeControl
    , getInterfaceAltSetting
    , readBulk,  readInterrupt
    , writeBulk, writeInterrupt
    )

#ifdef __HADDOCK__
import System.USB.Exceptions ( USBException(..) )
#endif


--------------------------------------------------------------------------------
-- ** Regional device handles
--------------------------------------------------------------------------------

{-| A regional handle to an opened USB device.

A regional handle to an opened USB device can be created by applying
'openDevice' or 'withDevice' to the USB device you wish to open.

Note that you can also /duplicate/ a regional device handle by applying 'dup' to
it.
-}
data RegionalDeviceHandle (r ∷ * → *) = RegionalDeviceHandle
                                          (USB.DeviceHandle)
                                          (MVar Bool)
                                          (CloseHandle r)

instance Dup RegionalDeviceHandle where
    dup (RegionalDeviceHandle h mv ch) =
      liftM (RegionalDeviceHandle h mv) $ dup ch

openDevice ∷ MonadCatchIO pr
           ⇒ USB.Device → RegionT s pr (RegionalDeviceHandle (RegionT s pr))
openDevice dev = block $ do
                   h  ← liftIO $ USB.openDevice dev
                   mv ← liftIO $ newMVar False
                   let closeAction = USB.closeDevice h
                   ch ← register closeAction
                   return $ RegionalDeviceHandle h mv ch

withDevice ∷ MonadCatchIO pr
           ⇒ USB.Device
           → (∀ s. RegionalDeviceHandle (RegionT s pr) → RegionT s pr α)
           → pr α
withDevice dev f = runRegionT $ openDevice dev >>= f

{-| Convenience function which finds the first device attached to the system
which satisfies the given predicate on its descriptor, then opens that device
and applies the given continuation function to the resulting device handle.

Exceptions:

 * 'NotFound' if no device is found which satisfies the given predicate.

 * 'NoMemException' if there is a memory allocation failure.

 * 'AccessException' if the user has insufficient permissions.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
withDeviceWhich ∷ ∀ pr α
                . MonadCatchIO pr
                ⇒ USB.Ctx
                → (USB.DeviceDesc → Bool) -- ^ Predicate on the device descriptor.
                → (∀ s. RegionalDeviceHandle (RegionT s pr) → RegionT s pr α)
                                          -- ^ Continuation function
                → pr α
withDeviceWhich ctx p f = do devs ← liftIO $ USB.getDevices ctx
                             useWhich devs withDevice p f

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
                      Nothing → throw NotFound
                      Just d  → w d f

-- | This exception can be thrown in 'withDeviceWhich', 'setConfigWhich',
-- 'withInterfaceWhich' or 'setAlternateWhich' to indicate that no value was
-- found which satisfied the given predicate.
data NotFound = NotFound deriving (Show, Typeable)

instance Exception NotFound

-- | Internally used function for getting the actual USB device handle from a
-- regional device handle.
getInternalDevHndl ∷ RegionalDeviceHandle r → USB.DeviceHandle
getInternalDevHndl (RegionalDeviceHandle h _ _) = h

-- | Convenience function for retrieving the device from the given regional
-- handle.
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

{-| Perform a USB port reset to reinitialize a device. The system will attempt
to restore the previous configuration and alternate settings after the reset has
completed.

Note the constraint: @pr \`ParentOf\` cr@. This allows this function to be
executed in any child region @cr@ of the parent region @pr@ in which the given
regional handle was created.

You can only reset a device when all computations passed to 'setConfig',
'useActiveConfig' and 'setConfigWhich' have been terminated. If you call
@resetDevice@ and such a computation is still running a 'SettingAlreadySet'
exception is thrown.

If the reset fails, the descriptors change, or the previous state cannot be
restored, the device will appear to be disconnected and reconnected. This means
that the device handle is no longer valid (you should close it) and rediscover
the device. A 'NotFoundException' is raised to indicate that this is the case.

/TODO: Think about how to handle the implications of the the previous paragraph!/

This is a blocking function which usually incurs a noticeable delay.

Exceptions:

 * 'SettingAlreadySet' if a configuration has been set using 'setConfig',
   'useActiveConfig' and 'setConfigWhich'.

 * 'NotFoundException' if re-enumeration is required, or if the
   device has been disconnected.

 * Another 'USBException'.
-}
resetDevice ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ RegionalDeviceHandle pr → cr ()
resetDevice (RegionalDeviceHandle internalDevHndl configAlreadySetMVar _) =
    liftIO $ withMVar configAlreadySetMVar $ \configAlreadySet →
               if configAlreadySet
                 then throwIO SettingAlreadySet
                 else USB.resetDevice internalDevHndl


--------------------------------------------------------------------------------
-- * Configurations
--------------------------------------------------------------------------------

{-| A supported configuration of a USB device parameterized by the region @r@ in
which it was created.

Note that, just like a regional device handle, a configuration can be duplicated
to a parent region using 'dup'.

Also note that you can get the descriptor of the configuration by applying
'getDesc' to it.
-}
data Config (r ∷ * → *) = Config (RegionalDeviceHandle r)
                                 USB.ConfigDesc

{-| Retrieve the supported configurations from the given regional handle.

Note that the configuration is parameterized by the same region @r@ in which the
regional handle was created. This ensures you can never use a configuration
outside that region.
-}
getConfigs ∷ RegionalDeviceHandle r → [Config r]
getConfigs regionalDevHndl = map (Config regionalDevHndl)
                           ∘ getConfigDescs
                           ∘ getInternalDevHndl
                           $ regionalDevHndl

-- | Internally used function for getting all the configuration descriptors of
-- the given device.
getConfigDescs ∷ USB.DeviceHandle → [USB.ConfigDesc]
getConfigDescs = USB.deviceConfigs ∘ USB.deviceDesc ∘ USB.getDevice

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


--------------------------------------------------------------------------------
-- ** Setting configurations
--------------------------------------------------------------------------------

{-| A handle to an active 'Config' which you can get using: 'setConfig',
'useActiveConfig' or 'setConfigWhich'.

The type variable @sCfg@ is used to ensure that you can't return this handle
from these functions.
-}
data ConfigHandle sCfg = ConfigHandle USB.DeviceHandle
                                      USB.ConfigDesc

{-| Set the active configuration for a device and then apply the given
continuation function to the resulting configuration handle.

USB devices support multiple configurations of which only one can be active at
any given time. When a configuration is set using 'setConfig', 'useActiveConfig'
or 'setConfigWhich' no threads can set a new configuration until the computation
passed to these functions terminates. If you do try to set one a
'SettingAlreadySet' exception will be thrown.

The operating system may or may not have already set an active configuration on
the device. It is up to your application to ensure the correct configuration is
selected before you attempt to claim interfaces and perform other operations. If
you want to use the current active configuration use 'useActiveConfig'.

If you call this function on a device already configured with the selected
configuration, then this function will act as a lightweight device reset: it
will issue a SET_CONFIGURATION request using the current configuration, causing
most USB-related device state to be reset (altsetting reset to zero, endpoint
halts cleared, toggles reset).

You cannot change/reset configuration if other applications or drivers have
claimed interfaces.

This is a blocking function.

Exceptions:

 * 'SettingAlreadySet' if a configuration has already been set using
   'setConfig', 'useActiveConfig' or 'setConfigWhich'.

 * 'BusyException' if interfaces are currently claimed.

 * 'NoDeviceException' if the device has been disconnected

 * Another 'USBException'.
-}
setConfig ∷ ∀ pr cr α
          . (pr `ParentOf` cr, MonadCatchIO cr)
          ⇒ Config pr                          -- ^ The configuration you wish to set.
          → (∀ sCfg. ConfigHandle sCfg → cr α) -- ^ Continuation function.
          → cr α
setConfig (Config (RegionalDeviceHandle internalDevHndl configAlreadySetMVar _)
                  configDesc)
          f =
    withUnsettedMVar configAlreadySetMVar $ do
      liftIO $ USB.setConfig internalDevHndl $ USB.configValue configDesc
      f $ ConfigHandle internalDevHndl configDesc

-- | Internally used function which throws a 'SettingAlreadySet' exception if
-- the given @MVar@ was set. If the given @MVar@ wasn't set it will be set
-- before the given computation is performed. When the computation terminates,
-- wheter normally or by raising an exception, the @MVar@ will be unset again.
withUnsettedMVar ∷ MonadCatchIO m ⇒ MVar Bool → m α → m α
withUnsettedMVar settingAlreadySetMVar =
    bracket_ (liftIO $ do settingAlreadySet ← takeMVar settingAlreadySetMVar
                          if settingAlreadySet
                            then do putMVar settingAlreadySetMVar settingAlreadySet
                                    throwIO SettingAlreadySet
                            else putMVar settingAlreadySetMVar True)
             (liftIO $ do _ ← takeMVar settingAlreadySetMVar
                          putMVar settingAlreadySetMVar False)

{-| This exception can be thrown in:

* 'resetDevice'

* 'setConfig', 'useActiveConfig' or 'setConfigWhich'

* 'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'

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

 * 'SettingAlreadySet' if a configuration has already been set using
   'setConfig', 'useActiveConfig' or 'setConfigWhich'.

 * 'NoActiveConfig' if the device is not configured.

 * 'NoDeviceException' if the device has been disconnected.

 * Aanother 'USBException'.
-}
useActiveConfig ∷ ∀ pr cr α
                . (pr `ParentOf` cr, MonadCatchIO cr)
                ⇒ RegionalDeviceHandle pr -- ^ Regional handle to the device
                                          --   from which you want to use the
                                          --   active configuration.
                → (∀ sCfg. ConfigHandle sCfg → cr α) -- ^ Continuation function
                → cr α
useActiveConfig (RegionalDeviceHandle internalDevHndl configAlreadySetMVar _) f =
    withUnsettedMVar configAlreadySetMVar $ do
      activeConfigValue ← liftIO $ USB.getConfig internalDevHndl
      when (activeConfigValue ≡ 0) $ throw NoActiveConfig
      let activeConfigDesc = fromJust $ find isActive $ getConfigDescs internalDevHndl
          isActive = (activeConfigValue ≡) ∘ USB.configValue
      f $ ConfigHandle internalDevHndl activeConfigDesc

{-| This exception can be thrown in 'useActiveConfig' to indicate that the
device is currently not configured.
-}
data NoActiveConfig = NoActiveConfig deriving (Show, Typeable)

instance Exception NoActiveConfig

{-| Convenience function which finds the first configuration of the given device
handle which satisfies the given predicate on its descriptor, then sets that
configuration and applies the given function to the resulting configuration
handle.

This function calls 'setConfig' so do see its documentation.

Exceptions:

 * 'SettingAlreadySet' if a configuration has already been set using
   'setConfig', 'useActiveConfig' or 'setConfigWhich'.

 * 'NotFound' if no configuration is found that satisfies the given
   predicate.

 * 'BusyException' if interfaces are currently claimed.

 * 'NoDeviceException' if the device has been disconnected

 * Another 'USBException'.
-}
setConfigWhich ∷ ∀ pr cr α
               . (pr `ParentOf` cr, MonadCatchIO cr)
               ⇒ RegionalDeviceHandle pr -- ^ Regional handle to the device for
                                         --   which you want to set a
                                         --   configuration.
               → (USB.ConfigDesc → Bool) -- ^ Predicate on the configuration
                                         --   descriptor.
               → (∀ sCfg. ConfigHandle sCfg → cr α) -- ^ Continuation function.
               → cr α
setConfigWhich h = useWhich (getConfigs h) setConfig


--------------------------------------------------------------------------------
-- * Interfaces
--------------------------------------------------------------------------------

{-| A supported interface of a configuration which you can retrieve using
 'getInterfaces'.

To retrieve the 'USB.Interface' descriptors of an interface use 'getDesc'.
-}
data Interface sCfg = Interface USB.DeviceHandle
                                USB.InterfaceNumber
                                USB.Interface

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

instance GetDescriptor (Interface sCfg) USB.Interface where
    getDesc (Interface _ _ ifDescs) = ifDescs


--------------------------------------------------------------------------------
-- ** Interface regions
--------------------------------------------------------------------------------

{-| A regional handle to a claimed interface.

A regional handle to a claimed interface can be created by applying 'claim' or
'withInterface' to the interface you wish to claim.
-}
data RegionalInterfaceHandle sCfg (r ∷ * → *) = RegionalInterfaceHandle
                                                  (Interface sCfg)
                                                  (MVar Bool)
                                                  (CloseHandle r)

instance Dup (RegionalInterfaceHandle sCfg) where
    dup (RegionalInterfaceHandle interface mv ch) =
      liftM (RegionalInterfaceHandle interface mv) $ dup ch

{-| Claim the given interface in the region. When the region terminates the
interface is released automatically.

Note that it is allowed to claim an already-claimed interface.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

This is a non-blocking function.

Exceptions:

 * 'BusyException' if another program or driver has claimed the interface.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
claim ∷ ∀ pr sCfg s
      . MonadCatchIO pr
      ⇒ Interface sCfg  -- ^ Interface you wish to claim
      → RegionT s pr
          (RegionalInterfaceHandle sCfg
            (RegionT s pr))
claim interface@(Interface internalDevHndl ifNum _) = block $ do
  mv ← liftIO $ newMVar False
  liftIO $ USB.claimInterface internalDevHndl ifNum
  let closeAction = USB.releaseInterface internalDevHndl ifNum
  ch ← register closeAction
  return $ RegionalInterfaceHandle interface mv ch

withInterface ∷ ∀ pr sCfg α
              . MonadCatchIO pr
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

 * 'NotFound' if no interface was found that satisfies the fiven predicate.

 * 'BusyException' if another program or driver has claimed the interface.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.

-}
withInterfaceWhich ∷ ∀ pr sCfg α
                   . MonadCatchIO pr
                   ⇒ ConfigHandle sCfg -- ^ Handle to a configuration of which
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

-- | A supported 'Interface' alternate setting which you can retrieve using
-- 'getAlternates'.
data Alternate sCfg (r ∷ * → *) = Alternate (RegionalInterfaceHandle sCfg r)
                                            USB.InterfaceDesc

{-| Retrieve the supported alternate settings from the given interface handle.

Note that the alternate setting is parameterized by the same type variables as
the interface handle. This ensures you can never use an alternate setting
outside the region in which the interface handle was created.
-}
getAlternates ∷ RegionalInterfaceHandle sCfg r → [Alternate sCfg r]
getAlternates regionalIfHandle@(RegionalInterfaceHandle (Interface _ _ alts)
                                                        _
                                                        _
                               ) =
    map (Alternate regionalIfHandle) alts

instance GetDescriptor (Alternate sIntrf r) USB.InterfaceDesc where
    getDesc (Alternate _ ifDesc) = ifDesc

instance Dup (Alternate sCfg) where
    dup (Alternate regionalIfHndlC ifDesc) = do
      regionalIfHndlP ← dup regionalIfHndlC
      return $ Alternate regionalIfHndlP ifDesc


--------------------------------------------------------------------------------
-- ** Setting alternates
--------------------------------------------------------------------------------

{-| A handle to a setted alternate setting.

You get a handle to an alternate using 'setAlternate', 'useActiveAlternate' or
'setAlternateWhich'. The type variable @sAlt@ is used to ensure that you can't
return this handle from these functions.
-}
data AlternateHandle sAlt (r ∷ * → *) = AlternateHandle
                                          USB.DeviceHandle
                                          USB.InterfaceDesc

{-| Activate an alternate setting for an interface and then apply the given
continuation function to the resulting alternate handle.

Simillary to configurations, interfaces support multiple alternate settings of
which only one can be active at any given time. When an alternate is set using
'setAlternate', 'useActiveAlternate' or 'setAlternateWhich' no threads can set a
new alternate until the computation passed to these functions terminates. If you
do try to set one a 'SettingAlreadySet' exception will be thrown.

The operating system may already have set an alternate for the interface. If you
want to use this current active alternate use 'useActiveAlternate'.

This is a blocking function.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

 * Another 'USBException'.
-}
setAlternate ∷ ∀ pr cr sCfg α
             . (pr `ParentOf` cr, MonadCatchIO cr)
             ⇒ Alternate sCfg pr -- ^ The alternate you wish to set.
             → (∀ sAlt. AlternateHandle sAlt pr → cr α) -- ^ Continuation function.
             → cr α
setAlternate (Alternate (RegionalInterfaceHandle (Interface internalDevHndl
                                                            ifNum
                                                            _
                                                 )
                                                 alternateAlreadySetMVar
                                                 _
                        )
                        ifDesc
             )
             f =
  withUnsettedMVar alternateAlreadySetMVar $ do
    liftIO $ USB.setInterfaceAltSetting internalDevHndl
                                        ifNum
                                        (USB.interfaceAltSetting ifDesc)
    f $ AlternateHandle internalDevHndl ifDesc


{-| Apply the given function to the alternate handle of the current active
alternate of the give interface handle.

To determine the current active alternate this function will block while a
control transfer is submitted to retrieve the information.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

 * Aanother 'USBException'.

-}
useActiveAlternate ∷ ∀ pr cr sCfg α
                   . (pr `ParentOf` cr, MonadCatchIO cr)
                   ⇒ RegionalInterfaceHandle sCfg pr -- ^ Regional handle to the
                                              --   interface from which you want
                                              --   to use the active alternate.
                   → (∀ sAlt. AlternateHandle sAlt pr → cr α) -- ^ Continuation function.
                   → cr α
useActiveAlternate (RegionalInterfaceHandle (Interface internalDevHndl
                                                       ifNum
                                                       alts
                                            )
                                            alternateAlreadySetMVar
                                            _
                   ) f =
    withUnsettedMVar alternateAlreadySetMVar $ do
      let timeout = 5000 -- ms
      activeAltValue ← liftIO $ USB.getInterfaceAltSetting internalDevHndl
                                                           ifNum
                                                           timeout
      let activeAlt = fromJust $ find isActive alts
          isActive  = (activeAltValue ≡) ∘ USB.interfaceAltSetting
      f $ AlternateHandle internalDevHndl activeAlt


{-| Convenience function which finds the first alternate of the given interface
handle which satisfies the given predicate on its descriptor, then sets that
alternate and applies the given function to the resulting alternate handle.

This function calls 'setAlternate' so do see its documentation.

Exceptions:

 * 'NotFound' if no alternate is found that satisfies the given
   predicate.

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'setAlternate', 'useActiveAlternate' or 'setAlternateWhich'.

 * Another 'USBException'.
-}
setAlternateWhich ∷ ∀ pr cr sCfg α
                  . (pr `ParentOf` cr, MonadCatchIO cr)
                  ⇒ RegionalInterfaceHandle sCfg pr -- ^ Regional handle to the
                                                    --   interface for which you want
                                                    --   to set an alternate.
                  → (USB.InterfaceDesc → Bool)      -- ^ Predicate on the interface
                                                    --   descriptor.
                  → (∀ sAlt. AlternateHandle sAlt pr → cr α) -- ^ Continuation function
                  → cr α
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
data Endpoint transDir
              transType
              sAlt
              (r ∷ * → *) = Endpoint USB.DeviceHandle
                                     USB.EndpointDesc

eqDir ∷ TransferDirection transDir → USB.TransferDirection → Bool
Out `eqDir` USB.Out = True
In  `eqDir` USB.In  = True
_   `eqDir` _       = False

eqType ∷ TransferType transType → USB.TransferType → Bool
Control     `eqType` USB.Control           = True
Isochronous `eqType` (USB.Isochronous _ _) = True
Bulk        `eqType` USB.Bulk              = True
Interrupt   `eqType` USB.Interrupt         = True
_           `eqType` _                     = False

-- | Retrieve all the endpoints from the given alternate handle which are of the
-- given transfer direction and transfer type.
getEndpoints ∷ ∀ transDir
                 transType
                 sAlt r
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

instance GetDescriptor (Endpoint transDir transType sAlt r)
                       USB.EndpointDesc where
    getDesc (Endpoint _ endpointDesc) = endpointDesc

{-| Clear the halt/stall condition for an endpoint.

Endpoints with halt status are unable to receive or transmit data until the halt
condition is stalled.

You should cancel all pending transfers before attempting to clear the halt
condition.

This is a blocking function.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
clearHalt ∷ (pr `ParentOf` cr, MonadIO cr)
          ⇒ Endpoint transDir transType sAlt pr → cr ()
clearHalt (Endpoint internalDevHndl endpointDesc) =
    liftIO $ USB.clearHalt internalDevHndl $ USB.endpointAddress endpointDesc

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


--------------------------------------------------------------------------------
-- * Endpoint I/O
--------------------------------------------------------------------------------

{-| Handy type synonym for read transfers.

A @ReadAction@ is a function which takes a timeout and a size which defines how
many bytes to read. The function returns an action which, when executed,
performs the actual read and returns the bytestring that was read paired with an
indication if the transfer timed out.
-}
type ReadAction r = USB.Timeout → USB.Size → r (ByteString, Bool)

-- | Class of transfer types that support reading.
class ReadEndpoint transType where
    {-| Read bytes from an 'In' endpoint with either a 'Bulk' or 'Interrupt'
        transfer type.

        Exceptions:

        * 'PipeException' if the endpoint halted.

        * 'OverflowException' if the device offered more data,
          see /Packets and overflows/ in the libusb documentation:
          <http://libusb.sourceforge.net/api-1.0/packetoverflow.html>.

        * 'NoDeviceException' if the device has been disconnected.

        * Another 'USBException'.
    -}
    readEndpoint ∷ (pr `ParentOf` cr, MonadIO cr)
                 ⇒ Endpoint In transType sAlt pr
                 → ReadAction cr

instance ReadEndpoint Bulk where
    readEndpoint = transferWith USB.readBulk

instance ReadEndpoint Interrupt where
    readEndpoint = transferWith USB.readInterrupt

transferWith ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ ( USB.DeviceHandle → USB.EndpointAddress
               → USB.Timeout → α → IO (β, Bool)
               )
             → ( Endpoint transDir transType sAlt pr
               → USB.Timeout → α → cr (β, Bool)
               )
transferWith f (Endpoint internalDevHndl endpointDesc) =
    \timeout sbs → liftIO $ f internalDevHndl
                              (USB.endpointAddress endpointDesc)
                              timeout
                              sbs

--------------------------------------------------------------------------------

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes a timeout and the bytestring to
write. The function returns an action which, when exectued, returns the number
of bytes that were actually written paired with an indication if the transfer
timed out.
-}
type WriteAction r = USB.Timeout → ByteString → r (USB.Size, Bool)

-- | Class of transfer types that support writing
class WriteEndpoint transType where
    {-| Write bytes to an 'Out' endpoint with either a 'Bulk' or 'Interrupt'
        transfer type.

        Exceptions:

        * 'PipeException' if the endpoint halted.

        * 'NoDeviceException' if the device has been disconnected.

        * Another 'USBException'.
    -}
    writeEndpoint ∷ (pr `ParentOf` cr, MonadIO cr)
                  ⇒ Endpoint Out transType sAlt pr
                  → WriteAction cr

instance WriteEndpoint Bulk where
    writeEndpoint = transferWith USB.writeBulk

instance WriteEndpoint Interrupt where
    writeEndpoint = transferWith USB.writeInterrupt


--------------------------------------------------------------------------------
-- ** Control transfers
--------------------------------------------------------------------------------

{-| Control transfers can have three request types: @Standard@, @Class@ and
@Vendor@. We disallow @Standard@ requests however because with them you can
destroy the safety guarantees that this module provides.
-}
data RequestType = Class | Vendor

reqTypeToInternal ∷ RequestType → USB.RequestType
reqTypeToInternal Class  = USB.Class
reqTypeToInternal Vendor = USB.Vendor

{-| Perform a USB /control/ request that does not transfer data.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'TimeoutException' if the transfer timed out.

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
control ∷ ∀ pr cr
        . (pr `ParentOf` cr, MonadIO cr)
        ⇒ RegionalDeviceHandle pr -- ^ A handle for the device to communicate
                                  --   with.
        → RequestType             -- ^ The type of request.
        → USB.Recipient           -- ^ The recipient of the request.
        → Word8                   -- ^ Request.
        → Word16                  -- ^ Value.
        → Word16                  -- ^ Index.
        → USB.Timeout             -- ^ Timeout (in milliseconds) that this
                                  --   function should wait before giving up due
                                  --   to no response being received. For no
                                  --   timeout, use value 0.
        → cr ()
control regionalDevHndl reqType reqRecipient request value index timeout =
    liftIO $ USB.control (getInternalDevHndl regionalDevHndl)
                         (reqTypeToInternal reqType)
                         reqRecipient
                         request
                         value
                         index
                         timeout

{-| Perform a USB /control/ read.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
readControl ∷ ∀ pr cr
            . (pr `ParentOf` cr, MonadIO cr)
            ⇒ RegionalDeviceHandle pr -- ^ A handle for the device to
                                      --   communicate with.
            → RequestType             -- ^ The type of request.
            → USB.Recipient           -- ^ The recipient of the request.
            → Word8                   -- ^ Request.
            → Word16                  -- ^ Value.
            → Word16                  -- ^ Index.
            → ReadAction cr
readControl regionalDevHndl reqType reqRecipient request value index = \timeout size →
    liftIO $ USB.readControl (getInternalDevHndl regionalDevHndl)
                             (reqTypeToInternal reqType)
                             reqRecipient
                             request
                             value
                             index
                             timeout
                             size

{-| Perform a USB /control/ write.

The /value/ and /index/ values should be given in host-endian byte order.

Exceptions:

 * 'PipeException' if the control request was not supported by the device

 * 'NoDeviceException' if the device has been disconnected.

 *  Another 'USBException'.
-}
writeControl ∷ ∀ pr cr
             . (pr `ParentOf` cr, MonadIO cr)
             ⇒ RegionalDeviceHandle pr -- ^ A handle for the device to
                                       --   communicate with.
             → RequestType             -- ^ The type of request.
             → USB.Recipient           -- ^ The recipient of the request.
             → Word8                   -- ^ Request.
             → Word16                  -- ^ Value.
             → Word16                  -- ^ Index.
             → WriteAction cr
writeControl regionalDevHndl reqType reqRecipient request value index = \timeout input →
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

This function may throw 'USBException's.
-}
getLanguages ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ RegionalDeviceHandle pr → cr [USB.LangId]
getLanguages devHndl =
    liftIO $ USB.getLanguages (getInternalDevHndl devHndl)

{-| Retrieve a string descriptor from a device.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.

/TODO: The following can be made more type-safe!/

When I call 'getStrDesc' I would like the type system to guarantee that the
given @StrIx@ and @LangId@ actually belong to the given @Handle@. In other
words I would like to get a type error when they are some arbitrary number or
come from another device.
-}
getStrDesc ∷ (pr `ParentOf` cr, MonadIO cr)
           ⇒ RegionalDeviceHandle pr
           → USB.StrIx
           → USB.LangId
           → USB.Size
           → cr String
getStrDesc devHndl strIx langId size =
    liftIO $ USB.getStrDesc (getInternalDevHndl devHndl)
                            strIx
                            langId
                            size

{-| Retrieve a string descriptor from a device using the first supported
language.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.
-}
getStrDescFirstLang ∷ (pr `ParentOf` cr, MonadIO cr)
                    ⇒ RegionalDeviceHandle pr
                    → USB.StrIx
                    → USB.Size
                    → cr String
getStrDescFirstLang devHndl descStrIx size =
    liftIO $ USB.getStrDescFirstLang (getInternalDevHndl devHndl)
                                     descStrIx
                                     size


--------------------------------------------------------------------------------
-- * USB kernel drivers
--------------------------------------------------------------------------------

{-| Determine if a kernel driver is active on an interface.

If a kernel driver is active, you cannot claim the interface, and libusb will be
unable to perform I/O.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
kernelDriverActive ∷ (pr `ParentOf` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr Bool
kernelDriverActive regionalDevHndl =
    liftIO ∘ USB.kernelDriverActive (getInternalDevHndl regionalDevHndl)

{-| Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
detachKernelDriver ∷ (pr `ParentOf` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr ()
detachKernelDriver regionalDevHndl =
    liftIO ∘ USB.detachKernelDriver (getInternalDevHndl regionalDevHndl)

{-| Re-attach an interface's kernel driver, which was previously
detached using 'detachKernelDriver'.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * 'BusyException' if the driver cannot be attached because the interface
   is claimed by a program or driver.

 * Another 'USBException'.
-}
attachKernelDriver ∷ (pr `ParentOf` cr, MonadIO cr)
                   ⇒ RegionalDeviceHandle pr → USB.InterfaceNumber → cr ()
attachKernelDriver regionalDevHndl =
    liftIO ∘ USB.attachKernelDriver (getInternalDevHndl regionalDevHndl)

{-| If a kernel driver is active on the specified interface the driver is
detached and the given action is executed. If the action terminates, whether by
normal termination or by raising an exception, the kernel driver is attached
again. If a kernel driver is not active on the specified interface the action is
just executed.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
withDetachedKernelDriver ∷ (pr `ParentOf` cr, MonadCatchIO cr)
                         ⇒ RegionalDeviceHandle pr
                         → USB.InterfaceNumber
                         → cr α
                         → cr α
withDetachedKernelDriver regionalDevHndl ifNum action =
    ifM (kernelDriverActive regionalDevHndl ifNum)
        (bracket_ (detachKernelDriver regionalDevHndl ifNum)
                  (attachKernelDriver regionalDevHndl ifNum)
                  action)
        action


--------------------------------------------------------------------------------
-- * Utils
--------------------------------------------------------------------------------

-- | Monadic @if ... then ... else ...@
ifM ∷ Monad m ⇒ m Bool → m α → m α → m α
ifM cM tM eM = do c ← cM
                  if c
                    then tM
                    else eM


-- The End ---------------------------------------------------------------------
