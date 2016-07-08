-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
import Network (listenOn, withSocketsDo, accept, connectTo, sClose, PortID(..), Socket)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad

data State1 = State1 {
    proposalNumber :: Int,
    clientHandles :: [Handle]
}

data State = State {
    message :: Int
}

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    let state = State {message = 1}
    cfg <- newMVar state
    forever $ accept socket >>= forkIO . (handleClientRequest socket cfg)

handleClientRequest socket cfg (handle, host, portno) = do
    msg <- hGetLine handle
    state <- takeMVar cfg
    let id = message state
    putStrLn $ show id ++ host ++ " says: " ++ msg
    let newState = state { message = id + 1}
    putMVar cfg newState
    handleClientRequest socket cfg (handle, host, portno)
