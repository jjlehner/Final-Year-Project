module V2H.ComponentGenerator where
import V2H.AstLenses (sourceTextToModuleAnsiHeader, moduleDeclarationToDataDeclarations, sourceTextToModuleDeclaration)

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
    | MultipleOrNoDataTypeOrImplicit [Ast.DataTypeOrImplicit]
    | MultipleOrNoVariableIdentifier [Ast.VariableIdentifier] deriving Show

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

-- generateConnectionFromDeclaration ::
-- extractModulePortConnections :: Ast.ModuleDeclaration -> Connection
generateConnectionsFromModuleAnsiHeader :: Ast.ModuleAnsiHeader -> PotentialComponent [Connection]
generateConnectionsFromModuleAnsiHeader moduleAnsiHeader =
    toListOf (types @Ast.AnsiPortDeclaration) moduleAnsiHeader
    & traverse (generateConnectionFromAnsiPortDeclaration (moduleAnsiHeader.moduleIdentifier))

generateConnectionFromDataDeclarations heldInModule dataDeclaration = do
    (Ast.VariableIdentifier identifier) <-
        toListOf (types @Ast.VariableIdentifier) dataDeclaration
        & app singletonToPotentialComponent MultipleOrNoVariableIdentifier

    dataTypeOrImplicit <-
        toListOf (types @Ast.DataTypeOrImplicit) dataDeclaration
        & app singletonToPotentialComponent MultipleOrNoDataTypeOrImplicit

    let dataType = dataDeclarationDataTypeRule dataTypeOrImplicit

    return Connection {
        heldInModule = heldInModule,
        connectionIdentifier = ConnectionIdentifier identifier,
        dataType = dataType
    }
-- generateConnectionsFromModuleItems :: [Ast.ModuleItem] -> PotentialComponent [Connection]
-- generateConnectionsFromModuleItems moduleItems
toComponents :: Ast.SourceText -> Components
toComponents sourceText =
    let moduleAnsiHeader = head $ sourceTextToModuleAnsiHeader sourceText
        moduleDeclaration = head $ sourceTextToModuleDeclaration sourceText
        dataDeclarations = head $ moduleDeclarationToDataDeclarations sourceText
        moduleAnsiHeaderConnections = generateConnectionsFromModuleAnsiHeader moduleAnsiHeader
        moduleDataDeclarationsConnections = traverse generateConnectionFromDataDeclarations dataDeclarations

    in traceShow moduleAnsiHeaderConnections $ Components Map.empty Map.empty
        & trace "-->"
                -- & traceShow x