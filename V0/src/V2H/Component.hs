
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -freduction-depth=500 #-}
module V2H.Component where

import              Control.Lens
import              Data.Function
import              Data.Generics.Product (types)
import              Data.Generics.Sum
import qualified    Data.Map              as Map
import              Debug.Trace
import              GHC.Generics          (Generic)
import qualified    V2H.Ast               as Ast
import V2H.Ast (DataTypeOrImplicit)
import Data.Either (fromRight)

type SensitivityList = [Ast.EventExpression]
data ProcessActivation = Comb | FF SensitivityList | Latch deriving (Show)
data TransitionFunction = TransitionFunction deriving (Show)

data Process =
    Process {
        processIdentifier   :: ProcessIdentifier,
        inputSignals        :: [ConnectionIdentifier],
        outputSignals       :: [ConnectionIdentifier],
        transitionFunctions :: [TransitionFunction]
    } deriving (Show)

data ConnectionProperties =
    CPNet {
        netType            :: Ast.NetType,
        dataType           :: Ast.DataType,
        unpackeDimensions  :: [Ast.UnpackedDimension],
        constantExpression :: Maybe Ast.ConstantExpression
    } | CPVariable {
        dataType           :: Ast.DataType,
        variableDimensions :: [Ast.VariableDimension],
        constantExpression :: Maybe Ast.ConstantExpression
    }

data Connection =
    Connection {
        heldInModule         :: Ast.ModuleIdentifier,
        connectionIdentifier :: ConnectionIdentifier,
        dataType             :: Ast.DataType
    } deriving (Show)

newtype ConnectionIdentifier = ConnectionIdentifier String deriving (Show)
newtype ProcessIdentifier = ProcessIdentifier String deriving (Show)

data Components =
    Components {
        processes   :: Map.Map ProcessIdentifier Process,
        connections :: Map.Map ConnectionIdentifier Connection
    } deriving (Show)

portDirectionRule :: Maybe Ast.PortDirection -> Ast.PortDirection
portDirectionRule Nothing = Ast.PDInout
portDirectionRule (Just x) = x

portDataTypeRule :: Either Ast.DataType Ast.ImplicitDataType -> Ast.DataType
portDataTypeRule (Left dt) = dt
portDataTypeRule (Right (Ast.ImplicitDataType signing packedDimensions)) =
    Ast.DTIntegerVector {
        integerVectorType = Ast.IVTLogic,
        signing = signing,
        packedDimensions = packedDimensions
    }

singletonToMaybe :: [a] -> Maybe a
singletonToMaybe [a] = Just a
singletonToMaybe _ = Nothing

maybeToPotentialComponent :: PotentialComponentError -> Maybe a -> PotentialComponent a
maybeToPotentialComponent err Nothing = Left err
maybeToPotentialComponent _ (Just c)  = Right c

singletonToPotentialComponent :: PotentialComponentError -> [a] -> PotentialComponent a
singletonToPotentialComponent err s =
    singletonToMaybe s
    & maybeToPotentialComponent err

app f1 f2 x = f1 (f2 x) x

data PotentialComponentError =
    MultipleOrNoPortIdentifier [Ast.PortIdentifier]
    | MultipleOrNoDataTypeOrImplicit [DataTypeOrImplicit] deriving Show

type PotentialComponent a = Either PotentialComponentError a

generateConnectionFromAnsiPortDeclaration :: Ast.ModuleIdentifier -> Ast.AnsiPortDeclaration -> PotentialComponent Connection
generateConnectionFromAnsiPortDeclaration heldInModule ansiPortDeclaration = do
    (Ast.PortIdentifier identifier) <-
        toListOf (types @Ast.PortIdentifier) ansiPortDeclaration
        & app singletonToPotentialComponent MultipleOrNoPortIdentifier

    dataTypeOrImplicit <-
        toListOf (types @Ast.DataTypeOrImplicit) ansiPortDeclaration
        & app singletonToPotentialComponent MultipleOrNoDataTypeOrImplicit

    let dataType = portDataTypeRule dataTypeOrImplicit

    return Connection {
        heldInModule = heldInModule,
        connectionIdentifier = ConnectionIdentifier identifier,
        dataType = dataType
    }

-- extractModulePortConnections :: Ast.ModuleDeclaration -> Connection
generateConnectionFromModuleAnsiHeader :: Ast.ModuleAnsiHeader -> PotentialComponent [Connection]
generateConnectionFromModuleAnsiHeader moduleAnsiHeader =
    toListOf (types @Ast.AnsiPortDeclaration) moduleAnsiHeader
    & traverse (generateConnectionFromAnsiPortDeclaration (moduleAnsiHeader.moduleIdentifier))

toComponents :: Ast.SourceText -> Components
toComponents x =
    let moduleDeclaration = head $ toListOf (types @Ast.ModuleAnsiHeader) x
        generateAnsiPortConnections = generateConnectionFromModuleAnsiHeader moduleDeclaration
    in traceShow generateAnsiPortConnections $ Components Map.empty Map.empty
