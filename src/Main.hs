module Main where

import Data.Monoid ( (<>) )
import Control.Monad ( forever )
import qualified Data.ByteString as B
import qualified Options.Applicative as O

import qualified Stilish.IPC as IPC


data Arguments = Arguments
   { fileSocket :: FilePath }


cli :: IO Arguments
cli = O.execParser $ O.info (O.helper <*> args) info

   where

   info :: O.InfoMod Arguments
   info = O.progDesc "stilish window manager assistant"

   args :: O.Parser Arguments
   args = Arguments
      <$>
         ( O.strOption $
            O.short 's' <> O.long "socket" <>
            O.value "/tmp/stilish.sock" <>
            O.help "UNIX file socket for inter-process communication"
         )


-- | Keep listening on a file socket and print the received messages.
main :: IO ()
main = do
   arg <- cli
   IPC.withUnixFileSocket (fileSocket arg) $ \sock ->
      forever (IPC.readMessage sock >>= B.putStrLn)
