{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}

module Main (main) where

import Control.Monad.Freer
import Data.Aeson          (FromJSON, ToJSON)
import Data.Default        (Default (def))
import Data.OpenApi.Schema as OpenApi
import GHC.Generics        (Generic)
import Prettyprinter

import Playground.Types                    (FunctionSchema)
import Plutus.PAB.Effects.Contract.Builtin qualified as B   ( Builtin
                                                            , BuiltinHandler (..)
                                                            , HasDefinitions (..)
                                                            , SomeBuiltin (..)
                                                            , handleBuiltin
                                                            , endpointsToSchemas
                                                            )
import Plutus.PAB.Simulator                as S ( SimulatorEffectHandlers
                                                , mkSimulatorHandlers
                                                )
import qualified Schema                    (FormSchema)
import Plutus.PAB.Run                      (runWith)

import Contract as C

main :: IO ()
main = runWith $ B.handleBuiltin @Contract

data Contract = Init
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty Contract where
    pretty = viaShow

instance B.HasDefinitions Contract where
    getDefinitions = []
    getSchema      = \_ -> B.endpointsToSchemas @MySchema
    getContract    = \_ -> B.SomeBuiltin C.run

