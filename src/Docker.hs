module Docker where

import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified Socket

newtype Image = Image { imageToText :: Text }
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode { exitCodeToInt :: Int }
  deriving (Eq, Show)

data CreateContainerOptions
  = CreateContainerOptions
      { image :: Image
      }

parseResponse
    :: HTTP.Response ByteString
    -> (Aeson.Value -> Aeson.Types.Parser a)
    -> IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value
  case result of
    Left e -> throwString e
    Right status -> pure status

newtype ContainerId = ContainerId { containerIdToText :: Text }
    deriving (Eq, Show)

createContainer :: CreateContainerOptions -> IO ContainerId
createContainer options = do
  manager <- Socket.newManager "/var/run/docker.sock"
  let image = imageToText options.image
      body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Cmd", "echo hello"),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"])
          ]
      parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId
      req =
        HTTP.defaultRequest
          & HTTP.setRequestManager manager
          & HTTP.setRequestPath "/v1.40/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body
  res <- HTTP.httpBS req
  parseResponse res parser
