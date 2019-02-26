{-|
Module      : Stilish.IPC
Description : Inter-process communication via UNIX file sockets.
-}
module Stilish.IPC
   ( withUnixFileSocket
   , readMessage ) where

import Control.Exception ( catch, throwIO, bracket )
import System.Posix.Files ( removeLink )
import System.IO.Error ( isDoesNotExistError )
import Network.Socket ( socket, bind, listen, accept, close, maxListenQueue
                      , Socket, Family(AF_UNIX), SocketType(Stream)
                      , SockAddr(SockAddrUnix))
import Network.Socket.ByteString ( recv )
import qualified Data.ByteString as B


-- | Perform some IO action with a UNIX file socket. Automatically closes the
-- socket upon finishing, throwing errors or encountering interruptions.
withUnixFileSocket :: FilePath -> (Socket -> IO a) -> IO a
withUnixFileSocket path = do
      remove path
      sock <- socket AF_UNIX Stream 0
      bind sock $ SockAddrUnix path
      listen sock maxListenQueue
      return sock
   `bracket` \sock -> do
      close sock
      remove path


-- | Accept a message of arbitrary length on the file socket.
readMessage :: Socket -> IO B.ByteString
readMessage sock = do
   (conn, _) <- accept sock
   message <- readMessageParts conn
   close conn
   return message

   where

   readMessageParts :: Socket -> IO B.ByteString
   readMessageParts conn = do
      message <- recv conn 1024
      if B.null message 
      then return message
      else B.append message <$> readMessageParts conn


-- | Remove a hard link if it exists.
remove :: FilePath -> IO ()
remove path = removeLink path `catch` \e -> 
   if isDoesNotExistError e
   then return ()
   else throwIO e
