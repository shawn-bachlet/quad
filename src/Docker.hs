module Docker where

import RIO

newtype Image = Image Text
  deriving (Eq, Show)

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

imageToText :: Image -> Text
imageToText (Image image) = image

data CreateContainerOptions
  = CreateContainerOptions
      { image :: Image
      }

createContainer :: CreateContainerOptions -> IO ()
createContainer options = undefined
