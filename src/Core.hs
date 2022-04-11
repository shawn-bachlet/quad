module Core (someFunc) where

import RIO
import RIO.ByteString.Lazy

data Pipeline = Pipeline { steps :: NonEmpty Step }
  deriving (Eq, Show)

data Step =
  Step
    { name :: StepName
    , commands :: NonEmpty Text
    , image :: Image
    }
  deriving (Eq, Show)

data Build =
  Build
    { pipeline :: Pipeline
    , state :: BuildState
    }
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning
  | BuildFinished BuildResult
  deriving (Eq, Show)

data BuildResult
  = BuilSucceeded
  | BuildFailed
  deriving (Eq, Show)

newtype StepName = StepName Text
  deriving (Eq, Show)

newtype Image = Image Text
  deriving (Eq, Show)

stepNameToText :: StepName -> Text
stepNameToText (StepName step) = step

imageToText :: Image -> Text
imageToText (Image image) = image

someFunc :: IO ()
someFunc = putStrLn "someFunc"
