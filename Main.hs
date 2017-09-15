{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Prelude hiding (getContents)
import Control.Exception
    (bracket, catch, finally, onException, throwIO, tryJust)
import Control.Monad (guard, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (intersperse, isPrefixOf)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.Socket.ByteString.Lazy (getContents)

import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (BufferMode(..), Handle, hPutStrLn, hSetBuffering)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Env.ByteString as BS
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.IO
    ( OpenMode(ReadWrite), closeFd, defaultFileFlags, dupTo, openFd, stdInput
    , stdOutput, stdError)
import System.Process hiding (runCommand)
import System.Timeout (timeout)

data Daemon = Daemon { numClients :: Int, maxTimeout :: Int }
    deriving (Show)

tryRemove :: FilePath -> IO ()
tryRemove = void . tryJust (guard . isDoesNotExistError) . removeFile

startServer :: FilePath -> [String] -> IO ()
startServer sockPath opts = withSocket serverLoop
  where
    serverLoop :: Socket -> IO ()
    serverLoop sock = do
        bind sock (SockAddrUnix sockPath)
        listen sock 10
        runDaemon (tryRemove sockPath) . withCreateProcess ghcModI $
            \(Just stdIn) (Just stdOut) (Just stdErr) _ -> do
                hSetBuffering stdIn LineBuffering
                let startState = Daemon { numClients = 0, maxTimeout = 1e6 }
                acceptLoop (handleRequest stdIn stdOut stdErr) startState
                hPutStrLn stdIn "quit"
      where
        runDaemon :: IO () -> IO () -> IO ()
        runDaemon act = (`onException` act) . daemonise . (`finally` act)

        ghcModI :: CreateProcess
        ghcModI = (proc "ghc-mod" (opts ++ ["legacy-interactive"]))
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }

        acceptLoop :: (Daemon -> Socket -> IO Daemon) -> Daemon -> IO ()
        acceptLoop requestHandler state@Daemon{..} = do
            mState <- bracket tryAccept tryClose $ mapM (requestHandler state)
            case mState of
                Nothing -> return ()
                Just s -> acceptLoop requestHandler s
          where
            tryClose :: Maybe Socket -> IO ()
            tryClose Nothing = return ()
            tryClose (Just s) = close s

            tryAccept :: IO (Maybe Socket)
            tryAccept
                | numClients == 0 = timeout maxTimeout doAccept
                | otherwise = Just <$> doAccept
              where
                doAccept = fst <$> accept sock

withSocket :: (Socket -> IO ()) -> IO ()
withSocket = bracket (socket AF_UNIX Stream defaultProtocol) close

daemonise :: IO () -> IO ()
daemonise program = void $ forkProcess child1
  where
    child1 = do
        createSession
        forkProcess child2
        exitImmediately ExitSuccess

    child2 = do
        mapM_ closeFd [stdInput, stdOutput, stdError]
        nullFd <- openFd "/dev/null" ReadWrite Nothing defaultFileFlags
        mapM_ (dupTo nullFd) [stdInput, stdOutput, stdError]
        closeFd nullFd
        program

handleRequest :: Handle -> Handle -> Handle -> Daemon -> Socket -> IO Daemon
handleRequest stdIn stdOut _stdErr s client = getContents client >>= runCommand
  where
    runCommand :: LBS.ByteString -> IO Daemon
    runCommand cmd
        | "daemon inc" `LBS.isPrefixOf` cmd
        = case LBS.stripPrefix "daemon inc " cmd >>= LBS8.readInt of
            Just (val, _) -> return incState{ maxTimeout = val * 1e6}
            Nothing -> return incState

        | "daemon dec" `LBS.isPrefixOf` cmd
        = return s{ numClients = numClients s - 1 }

        | Just (t, _) <- LBS8.readInt =<< LBS.stripPrefix "daemon timeout " cmd
        = return s{ maxTimeout = t * 1e6 }

        | otherwise = do
            LBS.hPut stdIn cmd
            getResult
            return s
      where
        incState = s{ numClients = numClients s + 1 }

    isNotDone :: BS.ByteString -> Bool
    isNotDone l = not $ "OK" `BS.isPrefixOf` l || "NG" `BS.isPrefixOf` l

    getResult :: IO ()
    getResult = do
        result <- BS.hGetLine stdOut
        when (isNotDone result) $ do
            sendAll client result
            sendAll client "\n"
            getResult

main :: IO ()
main = do
    opts <- filter (isPrefixOf "-") <$> getArgs
    command <- filter (not . BS.isPrefixOf "-") <$> BS.getArgs
    rootDir <- init <$> readProcess "ghc-mod" ["root"] ""
    let sockPath = rootDir </> ".ghc-modid"
        handleMissing act e
            | isDoesNotExistError e = act
            | otherwise = throwIO e

        doRequest = withSocket $ \sock -> do
            connect sock (SockAddrUnix sockPath)
            sendAll sock . mconcat . intersperse " " $ command
            sendAll sock "\n"
            shutdown sock ShutdownSend
            getContents sock >>= LBS.putStr

    doRequest `catch` handleMissing (startServer sockPath opts >> doRequest)
