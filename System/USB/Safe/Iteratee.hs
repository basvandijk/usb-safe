{-# LANGUAGE CPP
           , UnicodeSyntax
           , NoImplicitPrelude
           , FlexibleContexts
           , ScopedTypeVariables
  #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  System.USB.Safe.Iteratee
-- Copyright   :  (c) 2011 Bas van Dijk
-- License     :  BSD3 (see the file LICENSE)
-- Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>
--
-- Iteratee enumerators for endpoints.
--
--------------------------------------------------------------------------------

module System.USB.Safe.Iteratee
    ( enumReadBulk
    , enumReadInterrupt
#ifdef HAS_EVENT_MANAGER
    , enumReadIsochronous
#endif
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Prelude                          ( fromIntegral )
import Data.Function                    ( ($) )
import Data.Word                        ( Word8 )
import Data.Maybe                       ( Maybe(Nothing, Just) )
import Control.Monad                    ( (>>=), return )
import Foreign.Storable                 ( peek )
import Foreign.Ptr                      ( castPtr )
import Foreign.Marshal.Alloc            ( alloca, allocaBytes )
import Control.Exception                ( toException )

#if __GLASGOW_HASKELL__ < 700
import Prelude                          ( fromInteger )
import Control.Monad                    ( fail )
#endif

-- from base-unicode-symbols:
import Data.Eq.Unicode                  ( (≡), (≢) )
import Data.Bool.Unicode                ( (∧) )
import Data.Function.Unicode            ( (∘) )

-- from bindings-libusb:
import Bindings.Libusb                  ( c'libusb_bulk_transfer
                                        , c'libusb_interrupt_transfer
                                        , c'LIBUSB_SUCCESS
                                        , c'LIBUSB_ERROR_TIMEOUT
                                        )

-- from usb:
import System.USB.DeviceHandling        ( DeviceHandle )
import System.USB.Descriptors           ( EndpointAddress )
import System.USB.IO                    ( Timeout, Size )

import System.USB.Internal              ( C'TransferFunc
                                        , withDevHndlPtr
                                        , marshalEndpointAddress
                                        , convertUSBException
                                        )

#ifdef __HADDOCK__
import System.USB.Descriptors           ( maxPacketSize, endpointMaxPacketSize
                                        , TransferDirection(In)
                                        , TransferType(Bulk, Interrupt)
                                        )
#endif

-- from iteratee:
import Data.Iteratee.Base               ( Stream(EOF, Chunk), runIter, idoneM )
import Data.Iteratee.Iteratee           ( Enumerator, throwErr )
import Data.Iteratee.Base.ReadableChunk ( ReadableChunk(readFromPtr) )
import Data.NullPoint                   ( NullPoint(empty) )

-- from regions:
import Control.Monad.Trans.Region        ( RegionControlIO )
import Control.Monad.Trans.Region.Unsafe ( unsafeControlIO ) 

#ifdef HAS_EVENT_MANAGER
--------------------------------------------------------------------------------
-- from base:
import Data.Bool         ( otherwise, not )
import Data.Int          ( Int )
import Data.Function     ( id )
import Data.List         ( (++), map )
import Foreign.Ptr       ( Ptr, nullPtr, plusPtr )
import Foreign.Storable  ( poke )
import System.IO         ( IO )
import Text.Show         ( show )
import Prelude           ( (*), error, String )
import Control.Exception ( SomeException, onException, mask, uninterruptibleMask_ )

import
#if MIN_VERSION_base(4,4,0)
 GHC.Event
#else
 System.Event
#endif
   ( registerTimeout, unregisterTimeout )

-- from iteratee:
import Data.Iteratee.Base ( Iteratee )

-- from bindings-libusb:
import Bindings.Libusb ( c'LIBUSB_TRANSFER_TYPE_BULK
                       , c'LIBUSB_TRANSFER_TYPE_INTERRUPT
                       , c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS

                       , c'LIBUSB_TRANSFER_COMPLETED
                       , c'LIBUSB_TRANSFER_TIMED_OUT
                       , c'LIBUSB_TRANSFER_ERROR
                       , c'LIBUSB_TRANSFER_NO_DEVICE
                       , c'LIBUSB_TRANSFER_OVERFLOW
                       , c'LIBUSB_TRANSFER_STALL
                       , c'LIBUSB_TRANSFER_CANCELLED

                       , C'libusb_iso_packet_descriptor(..)
                       , C'libusb_transfer(..)

                       , c'libusb_submit_transfer
                       , c'libusb_cancel_transfer

                       , p'libusb_transfer'status
                       , p'libusb_transfer'actual_length
                       )

-- from usb:
import System.USB.DeviceHandling ( getDevice )
import System.USB.Exceptions     ( USBException(..), ioException )
#ifdef __HADDOCK__
import System.USB.Descriptors    ( TransferType(Isochronous), maxIsoPacketSize )
#endif
import System.USB.IO             ( noTimeout )
import System.USB.Internal       ( threaded
                                 , C'TransferType
                                 , allocaTransfer, withCallback
                                 , newLock, acquire, release
                                 , SumLength(..), sumLength
                                 , peekIsoPacketDescs
                                 , initIsoPacketDesc
                                 , getCtx, getEventManager
                                 )
#endif

--------------------------------------------------------------------------------
-- Enumerators
--------------------------------------------------------------------------------

-- | Iteratee enumerator for reading /bulk/ endpoints.
enumReadBulk ∷ (ReadableChunk s Word8, NullPoint s, RegionControlIO m)
             ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
             → EndpointAddress -- ^ The address of a valid 'In' and 'Bulk'
                               --   endpoint to communicate with. Make sure the
                               --   endpoint belongs to the current alternate
                               --   setting of a claimed interface which belongs
                               --   to the device.
             → Size            -- ^ Chunk size. A good value for this would be
                               --   the @'maxPacketSize' . 'endpointMaxPacketSize'@.
             → Timeout         -- ^ Timeout (in milliseconds) that this function
                               --   should wait for each chunk before giving up
                               --   due to no response being received.
             → Enumerator s m α
enumReadBulk
#ifdef HAS_EVENT_MANAGER
    | threaded  = enumReadAsync c'LIBUSB_TRANSFER_TYPE_BULK
#endif
    | otherwise = enumReadSync c'libusb_bulk_transfer

-- | Iteratee enumerator for reading /interrupt/ endpoints.
enumReadInterrupt ∷ (ReadableChunk s Word8, NullPoint s, RegionControlIO m)
                  ⇒ DeviceHandle    -- ^ A handle for the device to communicate
                                    --   with.
                  → EndpointAddress -- ^ The address of a valid 'In' and
                                    --   'Interrupt' endpoint to communicate
                                    --   with. Make sure the endpoint belongs to
                                    --   the current alternate setting of a
                                    --   claimed interface which belongs to the
                                    --   device.
                  → Size            -- ^ Chunk size. A good value for this would
                                    --   be the @'maxPacketSize' . 'endpointMaxPacketSize'@.
                  → Timeout         -- ^ Timeout (in milliseconds) that this
                                    --   function should wait for each chunk
                                    --   before giving up due to no response
                                    --   being received.
                  → Enumerator s m α
enumReadInterrupt
#ifdef HAS_EVENT_MANAGER
    | threaded  = enumReadAsync c'LIBUSB_TRANSFER_TYPE_INTERRUPT
#endif
    | otherwise = enumReadSync c'libusb_interrupt_transfer

type Run s m α = Stream s → IO (Restore s m α)

type Restore s m α = m (Iteratee s m α)

#ifdef HAS_EVENT_MANAGER
--------------------------------------------------------------------------------
-- Asynchronous
--------------------------------------------------------------------------------

enumReadAsync ∷ ∀ s m α
              . (ReadableChunk s Word8, NullPoint s, RegionControlIO m)
              ⇒ C'TransferType
              → DeviceHandle → EndpointAddress → Size → Timeout
              → Enumerator s m α
enumReadAsync transType = \devHndl endpointAddr chunkSize timeout →
  enum transType
       0 []
       devHndl endpointAddr
       timeout
       chunkSize
       withResult withResult
    where
      withResult ∷ WithResult s m α
      withResult transPtr bufferPtr cont stop = do
         n ← peek $ p'libusb_transfer'actual_length transPtr
         if n ≡ 0
           then stop $ Chunk empty
           else readFromPtr bufferPtr (fromIntegral n) >>= cont ∘ Chunk

type WithResult s m α = Ptr C'libusb_transfer → Ptr Word8
                      → Run s m α -- To continue
                      → Run s m α -- To stop
                      → IO (Restore s m α)

enum ∷ ∀ m s α
     . RegionControlIO m
     ⇒ C'TransferType
     → Int → [C'libusb_iso_packet_descriptor]
     → DeviceHandle → EndpointAddress
     → Timeout
     → Size
     → WithResult s m α → WithResult s m α
     → Enumerator s m α
enum transType
     nrOfIsoPackets isoPackageDescs
     devHndl endpointAddr
     timeout
     chunkSize
     onCompletion onTimeout = \iter →
  unsafeControlIO $ \runInIO →
    withDevHndlPtr devHndl $ \devHndlPtr →
      allocaBytes chunkSize $ \bufferPtr →
        allocaTransfer nrOfIsoPackets $ \transPtr → do
          lock ← newLock
          let Just (evtMgr, mbHandleEvents) = getEventManager $
                                                getCtx $
                                                  getDevice devHndl
              waitForTermination =
                  case mbHandleEvents of
                    Just handleEvents | timeout ≢ noTimeout → do
                      tk ← registerTimeout evtMgr (timeout * 1000) handleEvents
                      acquire lock
                        `onException`
                          (uninterruptibleMask_ $ do
                             unregisterTimeout evtMgr tk
                             _err ← c'libusb_cancel_transfer transPtr
                             acquire lock)
                    _ → acquire lock
                          `onException`
                            (uninterruptibleMask_ $ do
                               _err ← c'libusb_cancel_transfer transPtr
                               acquire lock)
          withCallback (\_ → release lock) $ \cbPtr → do

            poke transPtr $ C'libusb_transfer
              { c'libusb_transfer'dev_handle      = devHndlPtr
              , c'libusb_transfer'flags           = 0 -- unused
              , c'libusb_transfer'endpoint        = marshalEndpointAddress endpointAddr
              , c'libusb_transfer'type            = transType
              , c'libusb_transfer'timeout         = fromIntegral timeout
              , c'libusb_transfer'status          = 0  -- output
              , c'libusb_transfer'length          = fromIntegral chunkSize
              , c'libusb_transfer'actual_length   = 0 -- output
              , c'libusb_transfer'callback        = cbPtr
              , c'libusb_transfer'user_data       = nullPtr -- unused
              , c'libusb_transfer'buffer          = castPtr bufferPtr
              , c'libusb_transfer'num_iso_packets = fromIntegral nrOfIsoPackets
              , c'libusb_transfer'iso_packet_desc = isoPackageDescs
              }

            let go ∷ Enumerator s m α
                go i = runIter i idoneM on_cont

                on_cont ∷ (Stream s → Iteratee s m α)
                        → Maybe SomeException
                        → m (Iteratee s m α)
                on_cont _ (Just e) = return $ throwErr e
                on_cont k Nothing  =
                  unsafeControlIO $ \runInIO' →
                    mask $ \restore → do

                      let stop, cont ∷ Run s m α
                          stop = return   ∘ return ∘ k
                          cont = runInIO' ∘ go     ∘ k

                          ex ∷ USBException → IO (Restore s m α)
                          ex = stop ∘ EOF ∘ Just ∘ toException

                      err ← c'libusb_submit_transfer transPtr

                      if err ≢ c'LIBUSB_SUCCESS
                        then ex $ convertUSBException err
                        else do
                          waitForTermination
                          restore $ do
                            status ← peek $ p'libusb_transfer'status transPtr
                            let run withResult = withResult transPtr bufferPtr cont stop
                            case status of
                              ts | ts ≡ c'LIBUSB_TRANSFER_COMPLETED → run onCompletion
                                 | ts ≡ c'LIBUSB_TRANSFER_TIMED_OUT → run onTimeout

                                 | ts ≡ c'LIBUSB_TRANSFER_ERROR     → ex ioException
                                 | ts ≡ c'LIBUSB_TRANSFER_NO_DEVICE → ex NoDeviceException
                                 | ts ≡ c'LIBUSB_TRANSFER_OVERFLOW  → ex OverflowException
                                 | ts ≡ c'LIBUSB_TRANSFER_STALL     → ex PipeException

                                 | ts ≡ c'LIBUSB_TRANSFER_CANCELLED →
                                     moduleError "transfer status can't be Cancelled!"

                                 | otherwise → moduleError $ "Unknown transfer status: "
                                                               ++ show ts ++ "!"
            runInIO $ go iter

moduleError ∷ String → error
moduleError msg = error $ thisModule ++ ": " ++ msg

thisModule ∷ String
thisModule = "System.USB.IO.Iteratee"

needThreadedRTSError ∷ String → error
needThreadedRTSError msg = moduleError $ msg ++
  " is only supported when using the threaded runtime. " ++
  "Please build your program with -threaded."

--------------------------------------------------------------------------------
-- Isochronous
--------------------------------------------------------------------------------

-- | Iteratee enumerator for reading /isochronous/ endpoints.
--
-- /WARNING:/ You need to enable the threaded runtime (@-threaded@) for this
-- function to work correctly. It throws a runtime error otherwise!
enumReadIsochronous ∷ ∀ s m α
                    . (ReadableChunk s Word8, RegionControlIO m)
                    ⇒ DeviceHandle    -- ^ A handle for the device to communicate with.
                    → EndpointAddress -- ^ The address of a valid 'In' and 'Isochronous'
                                      --   endpoint to communicate with. Make sure the
                                      --   endpoint belongs to the current alternate
                                      --   setting of a claimed interface which belongs
                                      --   to the device.
                    → [Size]          -- ^ Sizes of isochronous packets. A good
                                      --   value for these would be the
                                      --   'maxIsoPacketSize'
                    → Timeout         -- ^ Timeout (in milliseconds) that this
                                      --   function should wait for each chunk
                                      --   before giving up due to no response
                                      --   being received.
                    → Enumerator [s] m α
enumReadIsochronous devHndl endpointAddr sizes timeout
    | not threaded = needThreadedRTSError "enumReadIsochronous"
    | otherwise    = enum c'LIBUSB_TRANSFER_TYPE_ISOCHRONOUS
                          nrOfIsoPackets (map initIsoPacketDesc sizes)
                          devHndl endpointAddr
                          timeout
                          totalSize
                          onCompletion onTimeout
    where
      SumLength totalSize nrOfIsoPackets = sumLength sizes

      onCompletion, onTimeout ∷ WithResult [s] m α
      onCompletion transPtr bufferPtr cont _ =
        peekIsoPacketDescs nrOfIsoPackets transPtr >>= go bufferPtr id
            where
              go _   ss [] = cont $ Chunk $ ss []
              go ptr ss (C'libusb_iso_packet_descriptor l a _ : ds) = do
                s ← readFromPtr ptr (fromIntegral a)
                go (ptr `plusPtr` fromIntegral l) (ss ∘ (s:)) ds

      onTimeout _ _ _ stop = stop ∘ EOF ∘ Just ∘ toException $ TimeoutException
#endif

--------------------------------------------------------------------------------
-- Synchronous
--------------------------------------------------------------------------------

enumReadSync ∷ ∀ s m α
             . (ReadableChunk s Word8, NullPoint s, RegionControlIO m)
             ⇒ C'TransferFunc → ( DeviceHandle
                                → EndpointAddress
                                → Size
                                → Timeout
                                → Enumerator s m α
                                )
enumReadSync c'transfer = \devHndl
                           endpoint
                           chunkSize
                           timeout → \iter →
    unsafeControlIO $ \runInIO →
      withDevHndlPtr devHndl $ \devHndlPtr →
        alloca $ \transferredPtr →
          allocaBytes chunkSize $ \dataPtr →
            let go ∷ Enumerator s m α
                go i = runIter i idoneM on_cont

                on_cont ∷ (Stream s → Iteratee s m α) → Maybe SomeException → m (Iteratee s m α)
                on_cont _ (Just e) = return $ throwErr e
                on_cont k Nothing  =
                  unsafeControlIO $ \runInIO' → do

                    let stop, cont ∷ Run s m α
                        stop = return   ∘ return ∘ k
                        cont = runInIO' ∘ go     ∘ k

                    err ← c'transfer devHndlPtr
                                     (marshalEndpointAddress endpoint)
                                     (castPtr dataPtr)
                                     (fromIntegral chunkSize)
                                     transferredPtr
                                     (fromIntegral timeout)

                    if err ≢ c'LIBUSB_SUCCESS ∧
                       err ≢ c'LIBUSB_ERROR_TIMEOUT
                      then stop ∘ EOF ∘ Just ∘ toException $ convertUSBException err
                      else do
                        t ← peek transferredPtr
                        if t ≡ 0
                          then stop $ Chunk empty
                          else readFromPtr dataPtr (fromIntegral t) >>= cont ∘ Chunk
            in runInIO $ go iter
