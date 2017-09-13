{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (getContents)
import Control.Exception
    (bracket, catch, finally, onException, throwIO, tryJust)
import Control.Monad (forever, guard, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (intersperse, isPrefixOf)
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.Socket.ByteString.Lazy (getContents)

import System.Directory (removeFile)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitSuccess))
import System.FilePath ((</>))
import System.IO (Handle, BufferMode(..), hSetBuffering)
import System.IO.Error (isDoesNotExistError)
import qualified System.Posix.Env.ByteString as BS
import System.Posix.Process (createSession, exitImmediately, forkProcess)
import System.Posix.IO
    ( OpenMode(ReadWrite), closeFd, defaultFileFlags, dupTo, openFd, stdInput
    , stdOutput, stdError)
import System.Process

tryRemove :: FilePath -> IO ()
tryRemove = void . tryJust (guard . isDoesNotExistError) . removeFile

startServer :: FilePath -> [String] -> IO ()
startServer sockPath opts = withSocket $ \sock -> do
    bind sock (SockAddrUnix sockPath)
    listen sock 10
    runDaemon (tryRemove sockPath) . withCreateProcess ghcModI $
        \(Just stdIn) (Just stdOut) (Just stdErr) _ -> do
            hSetBuffering stdIn LineBuffering
            forever $ bracket (fst <$> accept sock) close $
                handleRequest stdIn stdOut stdErr
  where
    runDaemon :: IO () -> IO () -> IO ()
    runDaemon act = (`onException` act) . daemonise . (`finally` act)

    ghcModI :: CreateProcess
    ghcModI = (proc "ghc-mod" (opts ++ ["legacy-interactive"]))
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = CreatePipe
        }

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

handleRequest :: Handle -> Handle -> Handle -> Socket -> IO ()
handleRequest stdIn stdOut _stdErr sock = do
    getContents sock >>= LBS.hPut stdIn
    getResult
  where
    isNotDone :: BS.ByteString -> Bool
    isNotDone l = not $ "OK" `BS.isPrefixOf` l || "NG" `BS.isPrefixOf` l

    getResult :: IO ()
    getResult = do
        result <- BS.hGetLine stdOut
        when (isNotDone result) $ do
            sendAll sock result
            sendAll sock "\n"
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
