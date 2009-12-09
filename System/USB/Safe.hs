
-- For each language extension I explain why I need it at the first place I need
-- it. So just do a textual search for them and you will see why I need so many
-- of them:

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UnicodeSyntax #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Safe
-- Copyright   :  (c) 2009 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- This modules provides the following guarantees for working with USB devices:
--
-- * You can't reference handles to devices that are closed. In other words: no
--   I/O with closed handles is possible.
--
-- * The programmer specifies the /region/ in which devices should remain
--   open. On exit from the region the opened devices are automatically closed.
--
-- * You can't reference handles to configurations that have not been set.
--
-- * You can't reference handles to interfaces that have not been claimed.
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
--   types 'Control' and 'Isochronous'. Only I/O with endpoints with the 'Bulk'
--   and 'Interrupt' transfer types is allowed.
--
-- This modules makes use of a technique called /Lightweight monadic regions/
-- invented by Oleg Kiselyov and Chung-chieh Shan
--
-- See: <http://okmij.org/ftp/Haskell/regions.html#light-weight>
--
--------------------------------------------------------------------------------

module System.USB.Safe
    ( -- * Device regions
      DeviceRegionT
    , runDeviceRegionT

    , TopDeviceRegion
    , runTopDeviceRegion

    , forkTopDeviceRegion

    , mapDeviceRegionT
    , liftCatch

      -- * Opening devices
    , DeviceHandle
    , openDevice
    , dupDeviceHandle
    , withDevice
    , getDevice

    , resetDevice

      -- * Configurations
    , Config
    , getConfigs
    , getConfigDesc
    , dupConfig

      -- ** Setting configurations
    , ConfigHandle
    , SettingAlreadySet
    , withConfig
    , NoActiveConfig
    , withActiveConfig

      -- * Interfaces
    , Interface
    , getInterfaces
    , getInterfaceDescs

      -- ** Claiming and releasing interfaces
    , InterfaceHandle
    , withInterface

      -- * Alternates
    , Alternate
    , getAlternates
    , getInterfaceDesc

      -- ** Setting alternates
    , AlternateHandle
    , withAlternate
    , withActiveAlternate

      -- * Endpoints
    , Endpoint
    , getEndpoints

      -- ** Filtering endpoints
    , FilteredEndpoint
    , filterEndpoints

    , getEndpointDesc
    , clearHalt

      -- *** Transfer directions
    , In
    , Out

      -- *** Transfer types
    , Control
    , Isochronous
    , Bulk
    , Interrupt

      -- * Endpoint I/O
    , ReadAction
    , ReadEndpoint(..)

    , WriteAction
    , WriteEndpoint(..)

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
import Control.Concurrent         ( forkIO, ThreadId )
import Control.Concurrent.MVar    ( MVar, newMVar, takeMVar, putMVar, withMVar)

import Control.Monad              ( when, liftM4 )

import Control.Exception          ( Exception, throwIO )
import Data.Typeable              ( Typeable )

import Data.IORef                 ( IORef, newIORef
                                  , readIORef, modifyIORef, atomicModifyIORef
                                  )

import Data.Word                  ( Word8, Word16 )
import Data.List                  ( filter, find )
import Data.Maybe                 ( fromJust )

-- from bytestring:
import Data.ByteString            ( ByteString )

-- from transformers:
import Control.Monad.Trans        ( MonadTrans, lift, MonadIO, liftIO )

import qualified Control.Monad.Trans.Reader as Reader ( liftCatch )
import           Control.Monad.Trans.Reader           ( ReaderT, runReaderT
                                                      , mapReaderT
                                                      )
-- from monads-fd:
import Control.Monad.Cont.Class   ( MonadCont )
import Control.Monad.Error.Class  ( MonadError, throwError, catchError )
import Control.Monad.RWS.Class    ( MonadRWS )
import Control.Monad.Reader.Class ( MonadReader, ask, local )
import Control.Monad.State.Class  ( MonadState, get, put )
import Control.Monad.Writer.Class ( MonadWriter, tell, listen, pass )

-- from MonadCatchIO-transformers:
import Control.Monad.CatchIO      ( MonadCatchIO, block, bracket, bracket_ )

-- from unicode-symbols:
import Prelude.Unicode            ( (∘), (≡), (∧) )

-- from usb:
import qualified System.USB.Enumeration    as USB ( Device, deviceDesc )
import qualified System.USB.DeviceHandling as USB ( DeviceHandle
                                                  , openDevice, closeDevice
                                                  , getDevice

                                                  , getConfig, setConfig

                                                  , InterfaceNumber
                                                  , claimInterface, releaseInterface

                                                  , setInterfaceAltSetting

                                                  , clearHalt
                                                  , resetDevice

                                                  , kernelDriverActive
                                                  , detachKernelDriver
                                                  , attachKernelDriver
                                                  )
import qualified System.USB.Descriptors    as USB ( deviceConfigs

                                                  , ConfigDesc
                                                  , configValue
                                                  , configInterfaces

                                                  , Interface

                                                  , InterfaceDesc
                                                  , interfaceNumber
                                                  , interfaceAltSetting
                                                  , interfaceEndpoints

                                                  , EndpointDesc
                                                  , endpointAddress
                                                  , endpointAttribs

                                                  , EndpointAddress
                                                  , transferDirection

                                                  , TransferDirection(In, Out)
                                                  , TransferType( Control
                                                                , Isochronous
                                                                , Bulk
                                                                , Interrupt
                                                                )

                                                  , getLanguages
                                                  , LangId
                                                  , StrIx
                                                  , getStrDesc
                                                  , getStrDescFirstLang
                                                  )
import qualified System.USB.IO.Synchronous as USB ( ReadAction, WriteAction

                                                  , Timeout, Size

                                                  , RequestType(Class, Vendor)
                                                  , Recipient

                                                  , control
                                                  , readControl, writeControl

                                                  , getInterfaceAltSetting

                                                  , readBulk, readInterrupt

                                                  , writeBulk, writeInterrupt
                                                  )


--------------------------------------------------------------------------------
-- * Device regions
--------------------------------------------------------------------------------

{-| A monad transformer in which 'USB.Device's can be opened wich are
automatically closed on exit from the region.

Note that regions can be nested. @pr@ (for parent region) is a monad which is
usually the region which is running this region. However when you are running a
'TopDeviceRegion' the parent region will be 'IO'.
-}
-- Note that for the kind signature on 'pr' I need: 'KindSignatures'
-- Note that because I use unicode symbols I need: 'UnicodeSyntax'

newtype DeviceRegionT s (pr ∷ * → *) α = DeviceRegionT
    (ReaderT (IORef [OpenedDevice]) pr α)

    deriving ( Monad
             , MonadTrans
             , MonadIO
             , MonadCatchIO
             , MonadCont
             )

    -- Note to automatically derive these instances I need:
    -- 'GeneralizedNewtypeDeriving' which lets you derive instances of the type
    -- you've wrapped.

{-| Regions need to know the list of devices that have been opened in and
duplicated to the region. Regions also need to be able to update that list when
new devices are registered.

There are to logical choices for representing this @[OpenedDevice]@ state:

* @StateT [OpenedDevice]@

* @ReaderT (IORef [OpenedDevice])@

The former has the advantage of being able to be run in any monad while the
latter can only be executed in the @IO@ monad because of the @IORef@. The latter
however may be more efficient because the state doesn't need to be passed
around.

Because eventually all regions have to be run in the @IO@ monad anyway I have
choosen the latter for its better efficiency.
-}
unDeviceRegionT ∷ DeviceRegionT s pr α
                → ReaderT (IORef [OpenedDevice]) pr α
unDeviceRegionT (DeviceRegionT reader) = reader

data OpenedDevice = OpenedDevice USB.DeviceHandle
                                 RefCntIORef
                                 ConfigAlreadySetMVar
                                 AlternateAlreadySetMVar

{-| Because regions can be nested and device handles can be duplicated from a
child region to a parent region we need to keep a reference count per opened
device.

The reference count is:

* initialized at 1 when a device is created in 'openDevice',

* incremented when we duplicate a device handle in 'dupDeviceHandle',

* decremented on exit from a region in 'runWith'.

Only when the reference count reaches 0 the device is actually closed.

Note that the reference count @IORef@ is shared between threads. I make sure the
modifications happen atomically using: 'atomicModifyIORef'.
-}
type RefCntIORef = IORef Int

-- | @MVar@ which keeps track of wheter a configuration has been set.
-- See: 'withConfig'.
type ConfigAlreadySetMVar = MVar Bool

-- | @MVar@ which keeps track of wheter an alternate has been set.
-- See: 'withAlternate'.
type AlternateAlreadySetMVar = MVar Bool

{-| Execute a region inside its parent region @pr@.

All 'USB.Device's which have been opened in the given region using 'openDevice',
and which haven't been duplicated using 'dupDeviceHandle', will be closed on
exit from this function wether by normal termination or by raising an exception.

Also all devices which have been duplicated to this region from a child region
are closed on exit if they haven't been duplicated themselves.

Note the type variable @s@ of the region wich is only quantified over the region
itself. This ensures that no values, that have a type which has @s@ in it, can
be returned from this function. (Note the similarity with the @ST@ monad.)

'DeviceHandle's are parameterized by the region in which they were created.
So device handles which were created by @openDevice@ in the given region have
this @s@ in their type. This ensures that these device handles, which may have
been closed on exit from this function, can't be returned by this function. This
ensures you can never do any IO with closed device handles.

Note that it is possible to run a region inside another region.

/TODO: Say something more about this nesting of regions.../
-}
-- Note for the inner ∀ quantifier I need: 'RankNTypes'
runDeviceRegionT ∷ MonadCatchIO pr ⇒ (∀ s. DeviceRegionT s pr α) → pr α
runDeviceRegionT m = runWith [] m

{-| A region which has 'IO' as its parent region which enables it to be directly
executed in 'IO' by 'runTopDeviceRegion' or concurrently executed in another
region by 'forkTopDeviceRegion'.
-}
type TopDeviceRegion s = DeviceRegionT s IO

-- | Convenience funtion for running a /top-level/ region in 'IO'.
--
-- Note that: @runTopDeviceRegion = 'runDeviceRegionT'@
runTopDeviceRegion ∷ (∀ s. TopDeviceRegion s α) → IO α
runTopDeviceRegion = runDeviceRegionT

{-| Return a region which executes the given /top-level/ region in a new thread.

Note that the forked region has the same type variable @s@ as the resulting
region. This means that all 'DeviceHandle's which can be referenced in the
resulting region can also be referenced in the forked region.
-}
forkTopDeviceRegion ∷ MonadIO pr
                    ⇒ TopDeviceRegion s ()
                    → DeviceRegionT s pr ThreadId
forkTopDeviceRegion m = DeviceRegionT $ do
  openedDevicesIORef ← ask
  liftIO $ do openedDevices ← readIORef openedDevicesIORef
              block $ do mapM_ incrementRefCnt openedDevices
                         forkIO $ runWith openedDevices m

runWith ∷ MonadCatchIO pr ⇒ [OpenedDevice] → DeviceRegionT s pr α → pr α
runWith openedDevices m =
    bracket (liftIO $ newIORef openedDevices)
            (\openedDevicesIORef → liftIO $ readIORef openedDevicesIORef >>=
                                             mapM_ closeOpenedDevice)
            (runReaderT $ unDeviceRegionT m)
    where
      closeOpenedDevice (OpenedDevice devHndlI refCntIORef _ _) = do
        refCnt ← decrement refCntIORef
        when (refCnt ≡ 0) $ USB.closeDevice devHndlI

      decrement refCntIORef = atomicModifyIORef refCntIORef $ \refCnt →
                              let predRefCnt = pred refCnt
                              in (predRefCnt, predRefCnt)

incrementRefCnt ∷ OpenedDevice → IO ()
incrementRefCnt (OpenedDevice _ refCntIORef _ _) =
    atomicModifyIORef refCntIORef $ \refCnt →
                      (succ refCnt, ())

-- | Transform the computation inside a region.
mapDeviceRegionT ∷ (m α → n β) → DeviceRegionT s m α → DeviceRegionT s n β
mapDeviceRegionT f = DeviceRegionT ∘ mapReaderT f ∘ unDeviceRegionT

-- | Lift a 'catchError' operation to the new monad.
liftCatch ∷ (pr α → (e → pr α) → pr α) -- ^ @catch@ on the argument monad.
          → DeviceRegionT s pr α       -- ^ Computation to attempt.
          → (e → DeviceRegionT s pr α) -- ^ Exception handler.
          → DeviceRegionT s pr α
liftCatch f m h = DeviceRegionT $ Reader.liftCatch f
                                                   (unDeviceRegionT m)
                                                   (unDeviceRegionT ∘ h)


--------------------------------------------------------------------------------
-- * Opening devices
--------------------------------------------------------------------------------

-- | A handle to an opened 'USB.Device' parameterized by the region @r@ in which
-- it was created.
newtype DeviceHandle (r ∷ * → *) = DeviceHandle OpenedDevice

internalDeviceHandle ∷ DeviceHandle r → USB.DeviceHandle
internalDeviceHandle (DeviceHandle (OpenedDevice devHndlI _ _ _)) = devHndlI

{-| Open a device in a region.

Note that the returned device handle is parameterized by the region in which
it was created. This is to ensure that device handles can never escape their
region and to support operations on device handles that are used in a child
region of the region in which the device was created.

This is a non-blocking function; no requests are sent over the bus.

Exceptions:

 * 'NoMemException' if there is a memory allocation failure.

 * 'AccessException' if the user has insufficient permissions.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
openDevice ∷ MonadCatchIO pr
           ⇒ USB.Device
           → DeviceRegionT s pr
               (DeviceHandle (DeviceRegionT s pr))
openDevice dev = DeviceRegionT $ block $ newOpenedDevice >>= registerOpenedDevice
    where
      newOpenedDevice = liftIO $ liftM4 OpenedDevice (USB.openDevice dev)
                                                     (newIORef 1)
                                                     (newMVar False)
                                                     (newMVar False)

{-| Duplicate a device handle in the parent region.

For example, suppose you run the following region:

@
runDeviceRegionT $ do
@

Inside this region you run a nested /child/ region like:

@
    d1hDup <- runDeviceRegionT $ do
@

Now in this child region you open the device @d1@:

@
        d1h <- openDevice d1
@

Note that @d1h :: DeviceHandle (DeviceRegion cs (DeviceRegion ps ppr))@ where
@cs@ is bound by the inner (child) @runDeviceRegionT@ and @ps@ is bound by the
outer (parent) @runDeviceRegionT@.

Suppose you want to use the resulting device handle @d1h@ in the /parent/ device
region. You can't simply @return d1h@ because then the type variable @cs@,
escapes the inner region.

However, if you duplicate the device handle you can safely return it.

@
        d1hDup <- dupDeviceHandle d1h
        return d1hDup
@

Note that @d1hDup :: DeviceHandle (DeviceRegionT ps ppr)@

Back in the parent region you can safely operate on @d1hDup@.
-}
dupDeviceHandle ∷ MonadCatchIO ppr
                ⇒ DeviceHandle (DeviceRegionT cs (DeviceRegionT ps ppr))
                  -- ^ A device handle created in @DeviceRegionT cs ...@ which
                  --   must have a parent region @DeviceRegionT ps ppr@.
                → DeviceRegionT cs (DeviceRegionT ps ppr)
                     (DeviceHandle (DeviceRegionT ps ppr))
                  -- ^ Yield a computation in @DeviceRegionT cs@ that returns
                  --   the duplicated device handle that can now be used in the
                  --   parent region @DeviceRegionT ps ppr@.
dupDeviceHandle (DeviceHandle openedDevice) = DeviceRegionT $
    block $ do liftIO $ incrementRefCnt openedDevice
               lift $ DeviceRegionT $ registerOpenedDevice openedDevice

registerOpenedDevice ∷ MonadIO pr1
                     ⇒ OpenedDevice
                     → ReaderT (IORef [OpenedDevice])
                               pr1
                               (DeviceHandle pr2)
registerOpenedDevice openedDevice = do
  openedDevicesIORef ← ask
  liftIO $ modifyIORef openedDevicesIORef (openedDevice:)
  return $ DeviceHandle openedDevice

{-| A convenience function which opens the given device, applies the given
function to the resulting device handle and runs the resulting region.

Note that: @withDevice dev f = @'runDeviceRegionT'@ $ @'openDevice'@ dev >>= f@
-}
withDevice ∷ MonadCatchIO pr
           ⇒ USB.Device
           → (∀ s. DeviceHandle (DeviceRegionT s pr)
                 → DeviceRegionT s pr α
             ) → pr α
withDevice dev f = runDeviceRegionT $ openDevice dev >>= f

-- | Retrieve the device from the device handle.
getDevice ∷ DeviceHandle region → USB.Device
getDevice = USB.getDevice ∘ internalDeviceHandle


--------------------------------------------------------------------------------
-- * monads-fd instances
--------------------------------------------------------------------------------

-- TODO: Should I also provide monads-tf instances? Can I provide both?

instance MonadError e pr ⇒ MonadError e (DeviceRegionT s pr) where
    throwError = lift ∘ throwError
    catchError = liftCatch catchError

instance MonadRWS r w st pr ⇒ MonadRWS r w st (DeviceRegionT s pr)

instance MonadReader r pr ⇒ MonadReader r (DeviceRegionT s pr) where
    ask   = lift ask
    local = mapDeviceRegionT ∘ local

instance MonadState st pr ⇒ MonadState st (DeviceRegionT s pr) where
    get = lift get
    put = lift ∘ put

instance MonadWriter w pr ⇒ MonadWriter w (DeviceRegionT s pr) where
    tell   = lift ∘ tell
    listen = mapDeviceRegionT listen
    pass   = mapDeviceRegionT pass


--------------------------------------------------------------------------------
-- * ParentOf
--------------------------------------------------------------------------------

{-| The @ParentOf@ class declares the parent/child relationship between regions.

A region is the parent of another region if they're either equivalent like:

@
DeviceRegionT ps parenRegion  `ParentOf`  DeviceRegionT ps parenRegion
@

or if it is the parent of the parent of the child like:

@
DeviceRegionT ps ppr  `ParentOf`  DeviceRegionT cs
                                    (DeviceRegionT pcs
                                      (DeviceRegionT ppcs
                                        (DeviceRegionT ps ppr)))
@
-}
-- Note that to declare this class with multiple parameters I need:
-- 'MultiParamTypeClasses'

class (Monad pr, Monad cr) ⇒ pr `ParentOf` cr

-- Note that because 'r' apears twice in the instance's head I need:
-- 'FlexibleInstances'

instance Monad r ⇒ ParentOf r r

-- Note that because the type constructor 'DeviceRegionT' apears in the
-- instance's context I need: 'FlexibleContexts' or 'UndecidableInstances'

-- And note that because of the type variable 's' and 'pcr' not occuring in the
-- instance's head I need: 'UndecidableInstances'

instance ( Monad cr
         , cr `TypeCast2` (DeviceRegionT s pcr)
         , pr `ParentOf` pcr
         )
         ⇒ ParentOf pr cr


--------------------------------------------------------------------------------
-- Type casting
--------------------------------------------------------------------------------

-- Note that to declare these classes with functional dependencies I need:
-- 'FunctionalDependencies'

class TypeCast2     (a ∷ * → *) (b ∷ * → *) |   a → b,   b → a
class TypeCast2'  t (a ∷ * → *) (b ∷ * → *) | t a → b, t b → a
class TypeCast2'' t (a ∷ * → *) (b ∷ * → *) | t a → b, t b → a

instance TypeCast2'  () a b ⇒ TypeCast2    a b
instance TypeCast2'' t  a b ⇒ TypeCast2' t a b
instance TypeCast2'' () a a


--------------------------------------------------------------------------------
-- * Resetting devices
--------------------------------------------------------------------------------

{-| Perform a USB port reset to reinitialize a device.

The system will attempt to restore the previous configuration and alternate
settings after the reset has completed.

Note the @pr \`ParentOf\` cr@ which ensures that this function
can be executed in any child region of the region in which the given device
handle was created.

You can only reset a device when all computations passed to 'withConfig' or
'withActiveConfig' have been terminated. If you call @resetDevice@ and such a
computation is still running a 'SettingAlreadySet' exception is thrown.

If the reset fails, the descriptors change, or the previous state cannot be
restored, the device will appear to be disconnected and reconnected. This means
that the device handle is no longer valid (you should close it) and rediscover
the device. A 'NotFoundException' is raised to indicate that this is the case.

/TODO: Think about how to handle the implications of the the previous paragraph!/

This is a blocking function which usually incurs a noticeable delay.

Exceptions:

 * 'NotFoundException' if re-enumeration is required, or if the
   device has been disconnected.

 * 'SettingAlreadySet' if a configuration has been set using 'withConfig' or
   'withActiveConfig'.

 * Another 'USBException'.
-}
resetDevice ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ DeviceHandle pr → cr ()
resetDevice (DeviceHandle (OpenedDevice devHndlI _ configAlreadySetMVar _)) =
    liftIO $ withMVar configAlreadySetMVar $ \configAlreadySet →
               if configAlreadySet
                 then throwIO SettingAlreadySet
                 else USB.resetDevice devHndlI


--------------------------------------------------------------------------------
-- * Configurations
--------------------------------------------------------------------------------

-- | A supported configuration of a 'USB.Device' parameterized by the region @r@
-- in which it was created.
data Config (r ∷ * → *) = Config (DeviceHandle r) USB.ConfigDesc

{-| Retrieve the supported configurations from the device handle.

Note that the configuration is parameterized by the same region @r@ in which the
device handle was created. This ensures you can never use a configuration
outside that region.
-}
getConfigs ∷ DeviceHandle r → [Config r]
getConfigs devHndl = map (Config devHndl)
                   ∘ getConfigDescs
                   ∘ internalDeviceHandle
                   $ devHndl

getConfigDescs ∷ USB.DeviceHandle → [USB.ConfigDesc]
getConfigDescs = USB.deviceConfigs ∘ USB.deviceDesc ∘ USB.getDevice

-- | Retrieve the configuration descriptor from the given configuration.
getConfigDesc ∷ Config r → USB.ConfigDesc
getConfigDesc (Config _ configDesc) = configDesc

{-| Duplicate a configuration in the parent region.

Also see: 'dupDeviceHandle'.
-}
dupConfig ∷ MonadCatchIO ppr
          ⇒ Config (DeviceRegionT cs (DeviceRegionT ps ppr))
            -- ^ A configuration created in @DeviceRegionT cs ...@ which
            --   must have a parent region @DeviceRegionT ps ppr@.
          → DeviceRegionT cs (DeviceRegionT ps ppr)
               (Config (DeviceRegionT ps ppr))
            -- ^ Yield a computation in @DeviceRegionT cs@ that returns
            --   the duplicated configuration that can now be used in the
            --   parent region @DeviceRegionT ps ppr@.
dupConfig (Config devHndlC cfg) = do
  devHndlP ← dupDeviceHandle devHndlC
  return $ Config devHndlP cfg


--------------------------------------------------------------------------------
-- ** Setting configurations
--------------------------------------------------------------------------------

{-| A handle to an active 'Config' parameterized by the region @r@ in which it
was created.

You get a handle to a configuration using 'withConfig' or
'withActiveConfig'. The type variable @sCfg@ is used to ensure that you can't
return this handle from these functions.
-}
newtype ConfigHandle sCfg (r ∷ * → *) = ConfigHandle (Config r)

{-| This exception can be thrown in:

* 'resetDevice'

* 'withConfig'

* 'withActiveConfig'

* 'withAlternate'

* 'withActiveAlternate'

to indicate that the device was already configured with a setting.
-}
data SettingAlreadySet = SettingAlreadySet deriving (Show, Typeable)
-- Note to derive 'Typeable' I need: 'DeriveDataTypeable'

instance Exception SettingAlreadySet

{-| Set the active configuration for a device and then apply the given function
to the resulting configuration handle.

USB devices support multiple configurations of which only one can be active at
any given time. When a configuration is set using 'withConfig' or
'withActiveConfig' no threads can set a new configuration until the computation
passed to these functions terminates. If you do try to set one a
'SettingAlreadySet' exception will be thrown.

The operating system may or may not have already set an active configuration on
the device. It is up to your application to ensure the correct configuration is
selected before you attempt to claim interfaces and perform other operations. If
you want to use the current active configuration use 'withActiveConfig'.

If you call this function on a device already configured with the selected
configuration, then this function will act as a lightweight device reset: it
will issue a SET_CONFIGURATION request using the current configuration, causing
most USB-related device state to be reset (altsetting reset to zero, endpoint
halts cleared, toggles reset).

You cannot change/reset configuration if other applications or drivers have
claimed interfaces.

This is a blocking function.

Exceptions:

 * 'BusyException' if interfaces are currently claimed.

 * 'NoDeviceException' if the device has been disconnected

 * 'SettingAlreadySet' if a configuration has already been set using
   'withConfig' or 'withActiveConfig'.

 * Another 'USBException'.
-}
withConfig ∷ (pr `ParentOf` cr, MonadCatchIO cr)
           ⇒ Config pr
           → (∀ sCfg. ConfigHandle sCfg pr → cr α)
           → cr α
withConfig config@(Config
                   (DeviceHandle
                    (OpenedDevice devHndlI _ configAlreadySetMVar _))
                   configDesc) f =
    withUnsettedMVar configAlreadySetMVar $ do
      liftIO $ USB.setConfig devHndlI $ USB.configValue configDesc
      f $ ConfigHandle config

withUnsettedMVar ∷ MonadCatchIO m ⇒ MVar Bool → m α → m α
withUnsettedMVar settingAlreadySetMVar =
    bracket_ (liftIO $ do settingAlreadySet ← takeMVar settingAlreadySetMVar
                          if settingAlreadySet
                            then do putMVar settingAlreadySetMVar settingAlreadySet
                                    throwIO SettingAlreadySet
                            else putMVar settingAlreadySetMVar True)
             (liftIO $ do _ ← takeMVar settingAlreadySetMVar
                          putMVar settingAlreadySetMVar False)

{-| This exception can be thrown in 'withActiveConfig' to indicate that the
device is currently not configured.
-}
data NoActiveConfig = NoActiveConfig deriving (Show, Typeable)

instance Exception NoActiveConfig

{-| Apply the given function to the configuration handle of the current active
configuration of the given device handle.

This function needs to determine the current active configuration. This
information may be cached by the operating system. If it isn't cached this
function will block while a control transfer is submitted to retrieve the
information.

/TODO: I'm not yet sure if this is the best way of handling already configured devices./
/So this may change in the future!/

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if a configuration has already been set using
   'withConfig' or 'withActiveConfig'.

 * 'NoActiveConfig' if the device is not configured.

 * Aanother 'USBException'.
-}
withActiveConfig ∷ (pr `ParentOf` cr, MonadCatchIO cr)
                 ⇒ DeviceHandle pr
                 → (∀ sCfg. ConfigHandle sCfg pr → cr α)
                 → cr α
withActiveConfig devHndl@(DeviceHandle
                          (OpenedDevice devHndlI _ configAlreadySetMVar _ )) f =
    withUnsettedMVar configAlreadySetMVar $ do
      activeConfigDesc ← liftIO $ getActiveConfigDesc devHndlI
      f $ ConfigHandle $ Config devHndl activeConfigDesc

getActiveConfigDesc ∷ USB.DeviceHandle → IO USB.ConfigDesc
getActiveConfigDesc devHndlI =
    do activeConfigValue ← USB.getConfig devHndlI
       when (activeConfigValue ≡ 0) $ throwIO NoActiveConfig
       let isActive = (activeConfigValue ≡) ∘ USB.configValue
       return $ fromJust $ find isActive $ getConfigDescs devHndlI


--------------------------------------------------------------------------------
-- * Interfaces
--------------------------------------------------------------------------------

{-| A supported interface of a 'Config' parameterized by the region @r@ in which
it was created and the @sCfg@ of the configuration it is derived from.
-}
newtype Interface sCfg (r ∷ * → *) = Interface Intrf

data Intrf = Intrf USB.DeviceHandle
                   USB.InterfaceNumber
                   USB.Interface
                   AlternateAlreadySetMVar

{-| Retrieve the supported interfaces from the configuration handle.

Note that the interface is parameterized by the same type variables as the
configuration handle. This ensures you can never use an interface outside the
scope of the function passed to 'withConfig' or 'withActiveConfig'.
-}
getInterfaces ∷ ConfigHandle sCfg r → [Interface sCfg r]
getInterfaces (ConfigHandle
               (Config
                (DeviceHandle
                 (OpenedDevice devHndlI _ _ alternateAlreadySetMVar))
                configDesc)) =
    map newInterface $ USB.configInterfaces configDesc
        where
          newInterface alts = Interface
                            $ Intrf devHndlI
                                    (USB.interfaceNumber $ head alts)
                                    alts
                                    alternateAlreadySetMVar

-- | Retrieve the alternate interface descriptors of the interface.
--
-- Note that: @type 'USB.Interface' = ['USB.InterfaceDesc']@.
getInterfaceDescs ∷ Interface sCfg r → USB.Interface
getInterfaceDescs (Interface (Intrf _ _ alts _)) = alts


--------------------------------------------------------------------------------
-- ** Claiming and releasing interfaces
--------------------------------------------------------------------------------

{-| A handle to a /claimed/ 'Interface' parameterized with the region @r@ in
which it was created.

You get a handle to an interface using 'withInterface'. The type variable
@sIntrf@ is used to ensure that you can't return this handle from this function.
-}
newtype InterfaceHandle sIntrf (r ∷ * → *) = InterfaceHandle Intrf

{-| Claim the given interface, then apply the given function to the resulting
interface handle and finally release the interface on exit from the function
wether by normal termination or by raising an exception.

Note that it is allowed to claim an already-claimed interface.

Claiming of interfaces is a purely logical operation; it does not cause any
requests to be sent over the bus. Interface claiming is used to instruct the
underlying operating system that your application wishes to take ownership of
the interface.

This is a non-blocking function.

Exceptions:

 * 'BusyException' if the interface is already claimed.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
withInterface ∷ (pr `ParentOf` cr, MonadCatchIO cr)
              ⇒ Interface sCfg pr
              → (∀ sIntrf. InterfaceHandle sIntrf pr → cr α)
              → cr α
withInterface (Interface intrf@(Intrf devHndlI ifNum _ _)) f =
    bracket_ (liftIO $ USB.claimInterface   devHndlI ifNum)
             (liftIO $ USB.releaseInterface devHndlI ifNum)
             (f $ InterfaceHandle intrf)


--------------------------------------------------------------------------------
-- * Alternates
--------------------------------------------------------------------------------

-- | A supported 'Interface' alternate setting parameterized by the region @r@
-- in which it was created and the @sIntrf@ of the interface it is derived from.
newtype Alternate sIntrf (r ∷ * → *) = Alternate Alt

data Alt = Alt USB.DeviceHandle
               USB.InterfaceDesc
               AlternateAlreadySetMVar

{-| Retrieve the supported alternate settings from the interface handle.

Note that the alternate setting is parameterized by the same type variables as
the interface handle. This ensures you can never use an alternate setting
outside the scope of the function passed to 'withInterface'.
-}
getAlternates ∷ InterfaceHandle sIntrf r → [Alternate sIntrf r]
getAlternates (InterfaceHandle (Intrf devHndlI _ alts alternateAlreadySetMVar)) =
    map (\alt → Alternate $ Alt devHndlI alt alternateAlreadySetMVar) alts

-- | Retrieve the interface descriptor of this alternate setting.
getInterfaceDesc ∷ Alternate sIntrf r → USB.InterfaceDesc
getInterfaceDesc (Alternate (Alt _ ifDesc _)) = ifDesc


--------------------------------------------------------------------------------
-- ** Setting alternates
--------------------------------------------------------------------------------

{-| A handle to a setted alternate setting parameterized by the region @r@ in
which it was created.

You get a handle to an alternate using 'withAlternate' or
'withActiveAlternate'. The type variable @sAlt@ is used to ensure that you can't
return this handle from these functions.
-}
newtype AlternateHandle sAlt (r ∷ * → *) = AlternateHandle Alt

{-| Activate an alternate setting for an interface and then apply the given
function to the resulting alternate handle.

Simillary to configurations, interfaces support multiple alternate settings of
which only one can be active at any given time. When an alternate is set using
'withAlternate' or 'withActiveAlternate' no threads can set a new alternate
until the computation passed to these functions terminates. If you do try to set
one a 'SettingAlreadySet' exception will be thrown.

The operating system may already have set an alternate for the interface. If you
want to use this current active alternate use 'withActiveAlternate'.

This is a blocking function.

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'withAlternate' or 'withActiveAlternate'.

 * Another 'USBException'.
-}
withAlternate ∷ (pr `ParentOf` cr, MonadCatchIO cr)
              ⇒ Alternate sIntrf pr
              → (∀ sAlt. AlternateHandle sAlt pr → cr α) → cr α
withAlternate (Alternate alt@(Alt devHndlI
                                  ifDesc
                                  alternateAlreadySetMVar
                             )
              ) f =
  withUnsettedMVar alternateAlreadySetMVar $ do
    liftIO $ USB.setInterfaceAltSetting
               devHndlI
               (USB.interfaceNumber     ifDesc)
               (USB.interfaceAltSetting ifDesc)
    f $ AlternateHandle alt

{-| Apply the given function to the alternate handle of the current active
alternate of the give interface handle.

To determine the current active alternate this function will block while a
control transfer is submitted to retrieve the information.

/TODO: I'm not yet sure if this is the best way of handling already configured devices./
/So this may change in the future!/

Exceptions:

 * 'NoDeviceException' if the device has been disconnected.

 * 'SettingAlreadySet' if an alternate has already been set using
   'withAlternate' or 'withActiveAlternate'.

 * Aanother 'USBException'.

-}
withActiveAlternate ∷ (pr `ParentOf` cr, MonadCatchIO cr)
                    ⇒ InterfaceHandle sIntrf pr
                    → (∀ sAlt. AlternateHandle sAlt pr → cr α) → cr α
withActiveAlternate (InterfaceHandle
                     (Intrf devHndlI ifNum alts alternateAlreadySetMVar)) f =
    withUnsettedMVar alternateAlreadySetMVar $ do
      activeAltValue ← liftIO $ USB.getInterfaceAltSetting devHndlI ifNum 5000
      let isActive = (activeAltValue ≡) ∘ USB.interfaceAltSetting
      f $ AlternateHandle $ Alt devHndlI
                                (fromJust $ find isActive alts)
                                alternateAlreadySetMVar


--------------------------------------------------------------------------------
-- * Endpoints
--------------------------------------------------------------------------------

-- | A supported endpoint from an 'Alternate' parameterized by the region @r@ in
-- which it was created and the @sAlt@ of the alternate it is derived from.
data Endpoint sAlt (r ∷ * → *) = Endpoint USB.DeviceHandle
                                          USB.EndpointDesc

{-| Retrieve the supported endpoints from the alternate handle.

Note that the endpoint is parameterized by the same type variables as the
alternate handle. This ensures you can never use an endpoint outside the scope
of the function passed to 'withAlternate' or 'withActiveAlternate'.
-}
getEndpoints ∷ AlternateHandle sAlt r → [Endpoint sAlt r]
getEndpoints (AlternateHandle (Alt devHndlI ifDesc _)) =
    map (Endpoint devHndlI) $ USB.interfaceEndpoints ifDesc


--------------------------------------------------------------------------------
-- ** Filtering endpoints
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

The reason it is called a @FilteredEndpoint@ is that to create it you have to
/filter/ a list of 'Endpoint's with 'filterEndpoints'.
-}
newtype FilteredEndpoint transDir
                         transType
                         sAlt (r ∷ * → *) = FilteredEndpoint (Endpoint sAlt r)

{-| The 'Endpoint' type is not rich enough to encode the transfer direction and
transfer type. In order to introduce this type information we have to filter the
list of endpoints and get back a list of filtered endpoints which have the
specified transfer direction and transfer type and also expres this information
in their type.
-}
filterEndpoints ∷ ∀ transDir
                    transType
                    sAlt r. ( TransferDirection transDir
                            , TransferType      transType
                            )
                ⇒ [Endpoint sAlt r] → [FilteredEndpoint transDir
                                                        transType
                                                        sAlt r
                                      ]
filterEndpoints = map FilteredEndpoint ∘ filter eqTransDirAndTransType
    where
      eqTransDirAndTransType (Endpoint _ endpointDesc) =
         (undefined ∷ transDir)  `eqTransDir`  transDirUSB
       ∧ (undefined ∷ transType) `eqTransType` transTypeUSB
      -- Note that to use the 'transDir' and 'transType' types inside this
      -- function you need: 'ScopedTypeVariables'
        where
         transDirUSB  = USB.transferDirection $ USB.endpointAddress endpointDesc
         transTypeUSB = USB.endpointAttribs endpointDesc

-- | Retrieve the endpoint descriptor from the given endpoint handle.
getEndpointDesc ∷ FilteredEndpoint transDir transType sAlt r
                → USB.EndpointDesc
getEndpointDesc (FilteredEndpoint (Endpoint _ endpointDesc)) = endpointDesc

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
          ⇒ FilteredEndpoint transDir transType sAlt pr → cr ()
clearHalt (FilteredEndpoint (Endpoint devHndlI endpointDesc)) =
    liftIO $ USB.clearHalt devHndlI $ USB.endpointAddress endpointDesc


--------------------------------------------------------------------------------
-- *** Transfer directions
--------------------------------------------------------------------------------

class TransferDirection transDir where
    eqTransDir ∷ transDir → USB.TransferDirection → Bool

-- | Out transfer direction (host -> device) used for writing.
data Out; instance TransferDirection Out where eqTransDir _ = (≡ USB.Out)

-- | In transfer direction (device -> host) used for reading.
data In; instance TransferDirection In where eqTransDir _ = (≡ USB.In)

-- Note that because these data types don't have definitions I need:
-- 'EmptyDataDecls'


--------------------------------------------------------------------------------
-- *** Transfer types
--------------------------------------------------------------------------------

class TransferType transType where
    eqTransType ∷ transType → USB.TransferType → Bool

-- | @Control@ endpoints don't support read and write operations.
data Control

instance TransferType Control where
    eqTransType _ = (≡ USB.Control)

-- | @Isochronous@ endpoints don't support read and write operations.
data Isochronous

instance TransferType Isochronous where
    eqTransType _ (USB.Isochronous _ _) = True
    eqTransType _ _ = False

-- | @Bulk@ endpoints support read and write operations.
data Bulk

instance TransferType Bulk where
    eqTransType _ = (≡ USB.Bulk)

-- | @Interrupt@ endpoints support read and write operations.
data Interrupt

instance TransferType Interrupt where
    eqTransType _ = (≡ USB.Interrupt)


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

{-| Class of transfer types that support reading.

(Only 'Bulk' and 'Interrupt' transfer types are supported.)
-}
class TransferType transType => ReadEndpoint transType where
    -- | Read bytes from an 'In' endpoint.
    readEndpoint ∷ (pr `ParentOf` cr, MonadIO cr)
                 ⇒ FilteredEndpoint In transType sAlt pr → ReadAction cr

instance ReadEndpoint Bulk where
    readEndpoint = readEndpointWith USB.readBulk

instance ReadEndpoint Interrupt where
    readEndpoint = readEndpointWith USB.readInterrupt

readEndpointWith ∷ (pr `ParentOf` cr, MonadIO cr)
                 ⇒ (USB.DeviceHandle → USB.EndpointAddress → USB.ReadAction)
                 → FilteredEndpoint In transType sAlt pr → ReadAction cr
readEndpointWith f (FilteredEndpoint (Endpoint devHndlI endpointDesc)) =
    \timeout size → liftIO $ f devHndlI
                               (USB.endpointAddress endpointDesc)
                               timeout
                               size

--------------------------------------------------------------------------------

{-| Handy type synonym for write transfers.

A @WriteAction@ is a function which takes a timeout and the bytestring to
write. The function returns an action which, when exectued, returns the number
of bytes that were actually written paired with an indication if the transfer
timed out.
-}
type WriteAction r = USB.Timeout → ByteString → r (USB.Size, Bool)

{-| Class of transfer types that support writing

(Only 'Bulk' and 'Interrupt' transfer types are supported.)
-}
class TransferType transType => WriteEndpoint transType where
    -- | Write bytes to an 'Out' endpoint.
    writeEndpoint ∷ (pr `ParentOf` cr, MonadIO cr)
                     ⇒ FilteredEndpoint Out transType sAlt pr → WriteAction cr

instance WriteEndpoint Bulk where
    writeEndpoint = writeEndpointWith USB.writeBulk

instance WriteEndpoint Interrupt where
    writeEndpoint = writeEndpointWith USB.writeInterrupt

writeEndpointWith ∷ (pr `ParentOf` cr, MonadIO cr)
                  ⇒ (USB.DeviceHandle → USB.EndpointAddress → USB.WriteAction)
                  → FilteredEndpoint Out transType sAlt pr → WriteAction cr
writeEndpointWith f (FilteredEndpoint (Endpoint devHndlI endpointDesc)) =
    \timeout bs → liftIO $ f devHndlI
                             (USB.endpointAddress endpointDesc)
                             timeout
                             bs

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
control ∷ (pr `ParentOf` cr, MonadIO cr)
        ⇒ DeviceHandle pr -- ^ A handle for the device to communicate with.
        → RequestType     -- ^ The type of request.
        → USB.Recipient   -- ^ The recipient of the request.
        → Word8           -- ^ Request.
        → Word16          -- ^ Value.
        → Word16          -- ^ Index.
        → USB.Timeout     -- ^ Timeout (in milliseconds) that this function should
                          --   wait before giving up due to no response being
                          --   received.  For no timeout, use value 0.
        → cr ()
control devHndl reqType reqRecipient request value index timeout =
    liftIO $ USB.control (internalDeviceHandle devHndl)
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
readControl ∷ (pr `ParentOf` cr, MonadIO cr)
            ⇒ DeviceHandle pr -- ^ A handle for the device to communicate with.
            → RequestType     -- ^ The type of request.
            → USB.Recipient   -- ^ The recipient of the request.
            → Word8           -- ^ Request.
            → Word16          -- ^ Value.
            → Word16          -- ^ Index.
            → ReadAction cr
readControl devHndl reqType reqRecipient request value index = \timeout size →
    liftIO $ USB.readControl (internalDeviceHandle devHndl)
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
writeControl ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ DeviceHandle pr -- ^ A handle for the device to communicate with.
             → RequestType     -- ^ The type of request.
             → USB.Recipient   -- ^ The recipient of the request.
             → Word8           -- ^ Request.
             → Word16          -- ^ Value.
             → Word16          -- ^ Index.
             → WriteAction cr
writeControl devHndl reqType reqRecipient request value index = \timeout input →
    liftIO $ USB.writeControl (internalDeviceHandle devHndl)
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
             ⇒ DeviceHandle pr → cr [USB.LangId]
getLanguages devHndl =
    liftIO $ USB.getLanguages (internalDeviceHandle devHndl)

{-| Retrieve a string descriptor from a device.

This is a convenience function which formulates the appropriate control message
to retrieve the descriptor. The string returned is Unicode, as detailed in the
USB specifications.

This function may throw 'USBException's.

/TODO: The following can be made more type-safe!/

When I call 'getStrDesc' I would like the type system to guarantee that the
given @StrIx@ and @LangId@ actually belong to the given @DeviceHandle@. In other
words I would like to get a type error when they are some arbitrary number or
come from another device.
-}
getStrDesc ∷ (pr `ParentOf` cr, MonadIO cr)
             ⇒ DeviceHandle pr → USB.StrIx → USB.LangId → USB.Size → cr String
getStrDesc devHndl strIx langId size =
    liftIO $ USB.getStrDesc (internalDeviceHandle devHndl)
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
                    ⇒ DeviceHandle pr → USB.StrIx → USB.Size → cr String
getStrDescFirstLang devHndl descStrIx size =
    liftIO $ USB.getStrDescFirstLang (internalDeviceHandle devHndl)
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
                   ⇒ DeviceHandle pr → USB.InterfaceNumber → cr Bool
kernelDriverActive devHndl =
    liftIO ∘ USB.kernelDriverActive (internalDeviceHandle devHndl)

{-| Detach a kernel driver from an interface.

If successful, you will then be able to claim the interface and perform I/O.

Exceptions:

 * 'NotFoundException' if no kernel driver was active.

 * 'InvalidParamException' if the interface does not exist.

 * 'NoDeviceException' if the device has been disconnected.

 * Another 'USBException'.
-}
detachKernelDriver ∷ (pr `ParentOf` cr, MonadIO cr)
                   ⇒ DeviceHandle pr → USB.InterfaceNumber → cr ()
detachKernelDriver devHndl =
    liftIO ∘ USB.detachKernelDriver (internalDeviceHandle devHndl)

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
                   ⇒ DeviceHandle pr → USB.InterfaceNumber → cr ()
attachKernelDriver devHndl =
    liftIO ∘ USB.attachKernelDriver (internalDeviceHandle devHndl)

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
                         ⇒ DeviceHandle pr → USB.InterfaceNumber → cr α → cr α
withDetachedKernelDriver devHndl ifNum action =
    ifM (kernelDriverActive devHndl ifNum)
        (bracket_ (detachKernelDriver devHndl ifNum)
                  (attachKernelDriver devHndl ifNum)
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
