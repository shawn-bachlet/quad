module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Data.Time.Clock.POSIX as Time
import qualified Socket

newtype Volume = Volume { volumeToText :: Text }
  deriving (Eq, Show)

newtype Image = Image {imageToText :: Text}
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode {exitCodeToInt :: Int}
  deriving (Eq, Show)

data CreateContainerOptions
  = CreateContainerOptions
      { image :: Image
      , script :: Text
      , volume :: Volume
      }

parseResponse ::
  HTTP.Response ByteString ->
  (Aeson.Value -> Aeson.Types.Parser a) ->
  IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status

newtype ContainerId = ContainerId {containerIdToText :: Text}
  deriving (Eq, Show)

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
      bind = volumeToText options.volume <> ":/app"
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script]),
            ("WorkingDir", "/app"),
            ("HostConfig", Aeson.object [ ("Binds", Aeson.toJSON [bind]) ]),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
      parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId
      req = makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  res <- HTTP.httpBS req
  parseResponse res parser

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"
      req = makeReq path
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other
      req = makeReq
              $ "/containers/" <> containerIdToText container <> "/json"
  res <- HTTP.httpBS req
  parseResponse res parser

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body = Aeson.object
               [ ("Labels", Aeson.object [("quad", "")]) ]
      req = makeReq "/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
      parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name
  res <- HTTP.httpBS req
  parseResponse res parser

fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ makeReq options = do
  let timestampToText t = tshow (round t :: Int)
      url =
        "/containers/"
          <> containerIdToText options.container
          <> "/logs?stdout=true&stderr=true&since="
          <> timestampToText options.since
          <> "&until="
          <> timestampToText options.until
  res <- HTTP.httpBS $ makeReq url
  pure $ HTTP.getResponseBody res

data Service
  = Service
      { createContainer :: CreateContainerOptions -> IO ContainerId,
        startContainer :: ContainerId -> IO (),
        containerStatus :: ContainerId -> IO ContainerStatus,
        createVolume :: IO Volume,
        fetchLogs :: FetchLogsOptions -> IO ByteString
      }

createService :: IO Service
createService = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path)
          & HTTP.setRequestManager manager
  pure
    Service
      { createContainer = createContainer_ makeReq,
        startContainer = startContainer_ makeReq,
        containerStatus = containerStatus_ makeReq,
        createVolume = createVolume_ makeReq,
        fetchLogs = fetchLogs_ makeReq
      }

type RequestBuilder = Text -> HTTP.Request

data FetchLogsOptions
  = FetchLogsOptions
      { container :: ContainerId
      , since :: Time.POSIXTime
      , until :: Time.POSIXTime
      }

