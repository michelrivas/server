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
import Control.Exception
import Data.GUID

type ServerID = String

data Server = Server {
    serverID :: ServerID,
    serverHandle :: Handle,
    hostName :: String,
    portNumber :: PortNumber
}

data ServerState = ServerState {
    localID :: ServerID,
    proposalNumber :: Int,
    localPort :: PortNumber,
    serverList :: [Server]
}

newGUID :: IO String
newGUID = genString

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    id <- newGUID
    let port = fromIntegral (read $ head args :: Int)
    socket <- listenOn $ PortNumber port
    putStrLn $ "Listening on " ++ (head args)
    putStrLn $ "ServerID: " ++ id
    let state = ServerState {localID = id, proposalNumber = 1, localPort = port, serverList = []}
    config <- newMVar state
    forkIO $ connectServers config (tail args)
    forkIO $ mainProcess config
    forever $ accept socket >>= forkIO . (handleClientConnection config)

handleClientConnection config (handle, host, portno) = do
    putStrLn "Client connected"
    id <- hGetLine handle
    putStrLn $ "ID: " ++ id
    sendID config handle
    let server = Server {serverID = id, serverHandle = handle, hostName = host, portNumber = portno}
    saveServer config server
    handleClientRequest config server

handleClientRequest config server = do
    msg <- hGetLine $ serverHandle server
    state <- takeMVar config
    let proposal = proposalNumber state
    let line = (serverID server) ++ " says: " ++ msg
    putStrLn line
    let servers = serverList state
--    broadcast line (serverID server) servers
    let newState = state { proposalNumber = proposal + 1}
    putMVar config newState
    handleClientRequest config server

saveServer config server = do
    state <- takeMVar config
    let servers = serverList state
    let newState = state { serverList = server : servers}
    putMVar config newState
    putStrLn $ "Servers: " ++ show (1 + length servers)

send msg handle = do
    hPutStrLn handle msg
--    putStrLn $ "Sent: " ++ msg

broadcast _ _ [] = return ()
broadcast msg origin (server : servers) = do
    when (serverID server /= origin) $ send msg (serverHandle server)
    broadcast msg origin servers

connectServers _ [] = return ()
connectServers config (portno : ports) = do
    forkIO $ connectServer config "localhost" portno
    connectServers config ports

connectServer config host portno = do
    let port = fromIntegral (read portno :: Int)
    putStrLn $ "Connecting to " ++ host ++ ":" ++ portno
    result <- testAddress host (PortNumber port)
    case result of
        Just handle -> do
            id <- handShake config handle
            let server = Server {serverID = id, serverHandle = handle, hostName = host, portNumber = port}
            saveServer config server
            putStrLn $ "Connected to " ++ host ++ ":" ++ portno
            handleClientRequest config server
        Nothing -> do
            putStrLn "Error connecting"
            threadDelay 5000000
            connectServer config host portno

testAddress host port = do
    result <- try $ connectTo host port
    case result of
        Left (SomeException e) -> return Nothing
        Right h -> return $ Just h

sendID config handle = do
    state <- takeMVar config
    send (localID state) handle
    putMVar config state

handShake config handle = do
    sendID config handle
    hGetLine handle

mainProcess config = do
    line <- getLine
    state <- takeMVar config
    let servers = serverList state
    putMVar config state
    broadcast line "" servers
    mainProcess config
