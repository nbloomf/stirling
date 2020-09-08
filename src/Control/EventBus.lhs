> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE OverloadedStrings #-}
> 
> module Control.EventBus (
>     Signal(..)
>   , EventBus()
>   , Handler(..)
>   , newEventBus
>   , runEventBus
>   , publish
>   , subscribe
>   , ShowEvent(..)
> ) where
> 
> import qualified Data.Vector              as V
> import qualified Data.ByteString.Char8    as C8
> import           Control.Concurrent.STM
> import           Control.Concurrent.Async
> 
> import Show.Event



Let's build an event bus!
-------------------------

An _event bus_ is an architectural pattern for building programs out of loosely coupled parts.

adapted from https://stackoverflow.com/questions/32434339/how-to-write-an-event-bus-in-haskell

> data Signal e
>   = Init | Halt | Signal e
>   deriving (Eq, Show)
> 
> instance
>   ( ShowEvent e
>   ) => ShowEvent (Signal e)
>   where
>     showEvent event = case event of
>       Init -> "INIT"
>       Halt -> "HALT"
>       Signal e -> showEvent e
> 
> newtype Handler e = Handler
>   (IO () -> Signal e -> IO ())
> 
> type EventBus e = TVar
>   ( V.Vector (Handler e)
>   , TChan (Signal e)
>   )



> newEventBus
>   :: IO (EventBus e)
> newEventBus = do
>   chan <- newTChanIO
>   newTVarIO (V.empty, chan)



> runEventBus
>   :: forall e
>    . EventBus e
>   -> IO (Async ())
> runEventBus bus = do
>   let
>     unsubscribe :: Int -> IO ()
>     unsubscribe idx = do
>       let
>         removeHandler
>           :: Int
>           -> (V.Vector (Handler e), a)
>           -> (V.Vector (Handler e), a)
>         removeHandler idx (handlers, a) =
>           let (hd, tl) = V.splitAt idx handlers
>           in (hd <> (V.tail tl), a)
>       atomically $
>         modifyTVar' bus (removeHandler idx)
> 
>     dispatch
>       :: Signal e -> IO ()
>     dispatch signal = do
>       let
>         dispatch'
>           :: (Handler e, Int) -> IO ()
>         dispatch' (Handler h, idx) =
>           h (unsubscribe idx) signal
>       (handlers, _) <-
>         atomically $ readTVar bus
>       mapM_ dispatch' $ zip (V.toList handlers) [0..]
> 
>     loop :: IO ()
>     loop = do
>       e <- atomically $ do
>         (_, chan) <- readTVar bus
>         readTChan chan
>       promises <- dispatch e
>       case e of
>         Halt -> return ()
>         _    -> loop
> 
>   async loop



> publish
>   :: EventBus e -> Signal e -> IO ()
> publish bus signal =
>   atomically $ do
>     (_, chan) <- readTVar bus
>     writeTChan chan signal



> subscribe
>   :: EventBus e -> Handler e -> IO ()
> subscribe bus handler =
>   atomically $ modifyTVar' bus $
>     \(hs, chan) -> (V.snoc hs handler, chan)
