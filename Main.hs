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
import Network (listenOn, withSocketsDo, accept, connectTo, sClose, PortID(..), Socket,PortNumber)
import System.Environment (getArgs)
import System.IO (hSetBuffering, hGetLine, hPutStrLn, hClose, BufferMode(..), Handle)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad

type ServerID = String

data Server = Server {
    serverID :: ServerID,
    serverHandle :: Handle,
    host :: String,
    port :: PortNumber
}

data State = State {
    proposalNumber :: Int,
    serverList :: [Server]
}

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    let port = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    let state = State {proposalNumber = 1, serverList = []}
    cfg <- newMVar state
    forever $ accept socket >>= forkIO . (handleClientConnection socket cfg)

handleClientConnection socket cfg (handle, host, portno) = do
    putStrLn "Client connected"
    state <- takeMVar cfg
    let servers = serverList state
    id <- hGetLine handle
    putStrLn $ "ID: " ++ id
    let server = Server {serverID = id, serverHandle = handle, host = host, port = portno}
    let newState = state { serverList = server : servers}
    putMVar cfg newState
    handleClientRequest socket cfg server

handleClientRequest socket cfg server = do
    let handle = serverHandle server
    msg <- hGetLine handle
    state <- takeMVar cfg
    let proposal = proposalNumber state
    let line = (serverID server) ++ " says: " ++ msg
    let servers = serverList state
    broadcast line (serverID server) servers
    let newState = state { proposalNumber = proposal + 1}
    putMVar cfg newState
    handleClientRequest socket cfg server

send msg s = do
    hPutStrLn (serverHandle s) msg
    putStrLn msg

broadcast _ _ [] = return ()
broadcast msg origin (s : servers) = do
    when (serverID s /= origin) $ send msg s
    broadcast msg origin servers
