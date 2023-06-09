{-# LANGUAGE TemplateHaskell #-}
module V2H.CodeGenerator where

import Data.Function
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Maybe qualified as Maybe
import Data.Text.Lazy (unpack)
import Data.List.Extra qualified as List
import Data.Either.Extra qualified as Either
import Control.Lens
import Control.Monad
import Text.Casing
import Text.Pretty.Simple
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.Simulator.Signal
import V2H.Simple.IRGenerator.Expressions
import V2H.Simulator.Simulate qualified as Simulate
import V2H.Simple.Transpile
import Debug.Trace
import Control.Monad.Extra (concatMapM)
import V2H.IR (getLeafFromHierachicalIdentifier)
import Data.ByteString.Lazy qualified as LazyByteString

defBang = Bang NoSourceUnpackedness NoSourceStrictness

-- This is repeated delete code
moduleIdenToModuleInstanceIden (IR.ModuleIdentifierIR iden) = IR.ModuleInstanceIdentifierIR iden

instanceHierarchyToString = foldl (\a (IR.ModuleInstanceIdentifierIR iden) -> a ++ (toPascal . fromAny) iden) ""

generateSumName instanceIdens (IR.VariableOrNetIdentifierIR iden) =
    instanceHierarchyToString instanceIdens ++ (toPascal . fromAny) iden

generateDecConPair instanceIdens var =
    let sumNameStr = generateSumName instanceIdens var.identifier
        sumName = mkName sumNameStr
        instanceType = AppT (ConT ''Simulate.FetchHierarchicalIdentifierIR) $ AppT (AppT (ConT ''SignalChange) (PromotedT sumName)) $ VarT (mkName "naturalNumberTypeMarker")
        body = NormalB $ VarE $ mkName $ "irIdentifier" ++ sumNameStr
        clause = Clause [WildP] body []
        instanceFunction = FunD 'Simulate.fetchHierarchicalIdentifierIR [clause]
    in (InstanceD Nothing [] instanceType [instanceFunction],NormalC sumName [])

generateSignalSumConstructors ::
    [IR.ModuleInstanceIdentifierIR]
    -> [IR.IR]
    -> IR.IR
    -> [(Dec,Con)]
generateSignalSumConstructors instanceIdens irs topLevelModule =
    let variables = (generateDecConPair instanceIdens <$> topLevelModule.variables)
        nets = (generateDecConPair instanceIdens <$> topLevelModule.nets)
        applyCasing f (IR.ModuleInstanceIdentifierIR iden) =
            IR.ModuleInstanceIdentifierIR $ f iden
        newInstanceName submodule = instanceIdens ++ [applyCasing (toPascal . fromAny) submodule.submoduleInstanceIdentifier]
        fetchIR s = Maybe.fromJust $ List.find (\elem -> elem.moduleIdentifier == s.submoduleIdentifier) irs
        submodules = concatMap (\sub -> generateSignalSumConstructors (newInstanceName sub) irs (fetchIR sub)) topLevelModule.submodules
    in variables ++ nets ++ submodules


generateSignalSumTypesFromIRs ::
    [IR.IR]
    -> IR.IR
    -> IR.ModuleInstanceIdentifierIR
    -> Q [Dec]
generateSignalSumTypesFromIRs irs topLevel moduleInstanceIdentifier =
    let (a,b) = unzip $ generateSignalSumConstructors [moduleInstanceIdentifier] irs topLevel
    in pure $ DataD [] (mkName "SignalSumTypes") [] Nothing b [DerivClause Nothing [ConT ''Show]] : a

generateStringFromHierarchicalIdentifier ::
    IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR
    -> String
generateStringFromHierarchicalIdentifier (IR.H (IR.ModuleInstanceIdentifierIR iden) sub) =
    (toPascal . fromAny) iden ++ generateStringFromHierarchicalIdentifier sub
generateStringFromHierarchicalIdentifier (IR.I (IR.VariableOrNetIdentifierIR iden)) = (toPascal . fromAny ) iden

generateSignalIdentifier ::
    IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR
    -> Q Dec
generateSignalIdentifier h = do
    let varName = "irIdentifier" ++ generateStringFromHierarchicalIdentifier h
    value <- [e| h |]
    pure $ ValD (VarP $ mkName varName) (NormalB value) []

generateSignalIdentifiers ::
    IR.ExpandedIR
    -> Q [Dec]
generateSignalIdentifiers expandedIR =
    traverse generateSignalIdentifier (Map.keys expandedIR.variables)

generateSignalType ::
    IR.DataTypeIR
    -> Type
generateSignalType dataType =
    AppT (ConT ''Signal) (LitT $ NumTyLit $ IR.getBitWidth dataType)
-- generateSignal ::
--     IR.VariableOrNetIdentifierIR
--     -> IR.DataTypeIR
--     -> Q [Dec]
-- generateSignal (IR.VariableOrNetIdentifierIR iden) (IR.DTSingular (IR.STScalar IR.SIVTLogic)) =
--     (pure.pure) $ TySundD
generateModuleName = concatMap (\(IR.ModuleInstanceIdentifierIR instIden) -> (toPascal . fromAny) instIden)

generateVariableFieldName (IR.VariableOrNetIdentifierIR iden) = "_" ++ (toCamel . fromAny) iden
generateSubmoduleFieldName (IR.ModuleInstanceIdentifierIR iden) = "_" ++ (toCamel . fromAny) iden

generateVariableSignal ::
    [IR.ModuleInstanceIdentifierIR]
    -> IR.VariableIR
    -> Type
generateVariableSignal instanceIdens variable =
    AppT (AppT (ConT ''SignalChange) $ PromotedT $ mkName $ generateSumName instanceIdens variable.identifier)
    $ generateSignalType variable.dataType

generateSubmoduleFieldType ::
    [IR.ModuleInstanceIdentifierIR]
    -> IR.SubmoduleIR
    -> Type
generateSubmoduleFieldType idens submodule =
    ConT $ mkName $ generateModuleName (idens ++ [submodule.submoduleInstanceIdentifier])

generateSubmoduleNameFromHierarchicalIdentifier hIden =
    generateModuleName (IR.flattenHierarchicalIdentifier hIden)

generateCircuitRecord ::
    [IR.ModuleInstanceIdentifierIR]
    -> [IR.IR]
    -> IR.IR
    -> Q [Dec]
generateCircuitRecord instanceIdens irs ir =
    let name = generateModuleName instanceIdens
        mkVarBangType names types = (\(n,t) -> (mkName n, defBang, t)) <$> zip names types
        variableFieldNames  = (\v -> generateVariableFieldName v.identifier) <$> ir.variables
        variableFieldTypes = generateVariableSignal instanceIdens <$> ir.variables
        variableVarBangType = mkVarBangType variableFieldNames variableFieldTypes
        submoduleFieldNames = generateSubmoduleFieldName . IR.submoduleInstanceIdentifier <$> ir.submodules
        submoduleFieldTypes = generateSubmoduleFieldType instanceIdens <$> ir.submodules
        submoduleVarBangTypes = mkVarBangType submoduleFieldNames submoduleFieldTypes
        con = RecC (mkName name) $ variableVarBangType ++ submoduleVarBangTypes
        genSubmodules submodule = generateCircuitRecord (instanceIdens ++ [submodule.submoduleInstanceIdentifier]) irs (Maybe.fromJust $ IR.findIRFromSubmodule irs submodule)
    in (:) (DataD [] (mkName name) [] Nothing [con] [DerivClause Nothing [ConT ''Show]]) <$> concatMapM genSubmodules ir.submodules
        -- (++) <$> concatMapM genSubmodules ir.submodules <*>

data X =
    HVoni (IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR)
    | HDec (IR.HierarchicalIdentifierIR (Name, Exp)) deriving (Show, Eq, Ord)

cutHierarchyTailInner ::
    Name
    -> (forall a .  (Ord a, Show a, Eq a) => IR.HierarchicalIdentifierIR a -> IR.HierarchicalIdentifierIR a)
    -> IR.HierarchicalIdentifierIR a
    ->  (Exp -> X)
cutHierarchyTailInner _ gen (IR.H mi (IR.I _)) = \exp -> HDec $ gen $ IR.I (mkName $ generateSubmoduleFieldName mi,exp)
cutHierarchyTailInner m gen (IR.H m1 (IR.H m2 h)) = cutHierarchyTailInner m (gen . IR.H m1) (IR.H m2 h)
cutHierarchyTailInner m gen (IR.I _) = \exp -> HDec $ gen $ IR.I (m,exp)

cutHierarchyTail :: (Ord a, Show a, Eq a) => Name -> IR.HierarchicalIdentifierIR a -> Exp -> X
cutHierarchyTail m (IR.I _) = \exp -> HDec $ IR.I (m,exp)
cutHierarchyTail _ (IR.H iden@(IR.ModuleInstanceIdentifierIR m) h) = cutHierarchyTailInner (mkName m) (IR.H iden) h

cutHierarchyTailTop m (HVoni a) = cutHierarchyTail m a
cutHierarchyTailTop m (HDec a) = cutHierarchyTail m a

unwrap ::
    (forall x. IR.HierarchicalIdentifierIR x -> t)
    -> X -> t
unwrap f (HVoni a) = f a
unwrap f (HDec a) = f a

-- instance Show X where
--     show (VONI a) = show a
--     show (Dec d) = pprint d
-- instance Eq X where
--     (==) a b = show a == show b
-- instance Ord X where
--     (<=) a b = show a <= show b

generateBody :: Name -> [X] -> Q Exp
generateBody dynamicCircuitRecordName [HDec a] = return $ snd $ IR.getLeafFromHierachicalIdentifier a
generateBody dynamicCircuitRecordName hierarchyAsList = do
    let mkRecordField (HVoni hVoni) = do
            hAsExp <- lift $ peel hVoni
            mkStableAsExpr <- [e|mkStableFromSignalValue|]
            let name = mkName $ generateVariableFieldName $ IR.getLeafFromHierachicalIdentifier hVoni
            mapIndex <- [e|(Map.!)|]
            let dcv = VarE dynamicCircuitRecordName
            let field = AppE mkStableAsExpr $ AppE (AppE mapIndex dcv) hAsExp
            pure (name, field)
        mkRecordField (HDec dec) = do
            pure $ IR.getLeafFromHierachicalIdentifier dec
    let hierarchyGroup = List.maximumOn (unwrap IR.depthOfHierarchicalIdentifier) hierarchyAsList
    let (h1, h2) = List.partition (unwrap $ unwrap IR.sameRootHierarchicalIdentifier hierarchyGroup) hierarchyAsList
    templateRec <- do
            c <- mkRecordField hierarchyGroup
            example <- traverse mkRecordField h1
            pure $ RecConE (mkName $ unwrap generateSubmoduleNameFromHierarchicalIdentifier hierarchyGroup) example
    let h3 = cutHierarchyTailTop (mkName "top") hierarchyGroup templateRec
    generateBody dynamicCircuitRecordName (h3 : h2)

generateConvertFromDynamicFunction ::
    Name
    -> IR.ExpandedIR
    -> Q [Dec]
generateConvertFromDynamicFunction dynamicCircuitRecordName expandedIR = do
    let h =
            Map.keys expandedIR.nets ++ Map.keys expandedIR.variables
            & fmap HVoni
    let circuitDynamic = mkName "circuitDynamic"
    bodyExpr <- generateBody circuitDynamic h
    a <- (pure . pure) $ FunD (mkName "convertFromDynamic") [Clause [VarP circuitDynamic] (NormalB bodyExpr) []]
    trace (pprint a) return a


generateAccessorTail ::
    Exp
    -> IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR
    -> Exp
generateAccessorTail a (IR.H s h) =
    generateAccessorTail (GetFieldE a (generateSubmoduleFieldName s)) h
generateAccessorTail a (IR.I (IR.VariableOrNetIdentifierIR str)) =
    GetFieldE a $ generateSubmoduleFieldName $ IR.ModuleInstanceIdentifierIR str

generateAccessor ::
    Name
    -> IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR
    -> Exp
generateAccessor name = generateAccessorTail (VarE name)

generateMapElem ::
    Name
    -> IR.HierarchicalIdentifierIR IR.VariableOrNetIdentifierIR
    -> Q Exp
generateMapElem name key = do
    keyExp <- [e| key|]
    signalChangeToDynamicExp <- [e|signalChangeToDynamic|]
    return $ TupE [Just keyExp, Just $ AppE signalChangeToDynamicExp $ generateAccessor name key]

peel (IR.H _ h) = h
generateConvertToDynamicFunction ::
    Name
    -> IR.ExpandedIR
    -> Q [Dec]
generateConvertToDynamicFunction name expandedIR = do
    a <- traverse (generateMapElem name . peel) (Map.keys expandedIR.variables)
    toMapExp <- [e|Map.fromList|]
    let body = NormalB $ AppE toMapExp $ ListE a
    (pure . pure) $ FunD (mkName "convertToDynamic") [Clause [VarP name] body []]


generateZeoredElem (key, var) = do
    keyExp <- [e| (peel key)|]
    signalChangeToDynamicExp <- [e|signalChangeToDynamic|]
    let dataType = var.dataType
    dataTypeAsExpr <- [e|dataType|]
    signalValueConstructorAsExpr <- [e|IR.SignalValue|]
    return $ TupE [Just keyExp, Just $ AppE (AppE signalValueConstructorAsExpr dataTypeAsExpr) (AppE (VarE 'mkSignalValueDataObjectFromInteger) (LitE $ IntegerL 0)) ]

generateEmptyValue ::
    IR.ExpandedIR
    -> Q [Dec]
generateEmptyValue expandedIR = do
    zeroedVariables <- traverse generateZeoredElem $ Map.toList expandedIR.variables
    zeroedNets <- traverse generateZeoredElem $ Map.toList expandedIR.nets
    appendedAsExpr <- [e|(++)|]
    fromListAsExpr <- [e|Map.fromList|]
    let joinedList = AppE (AppE appendedAsExpr (ListE zeroedNets)) (ListE zeroedVariables)
    (pure . pure) $ ValD (VarP $ mkName "empty") (NormalB $ AppE fromListAsExpr joinedList) []


generateExpandedIRValue toplevelModuleName sourceFiles = do
    generateIRsFromSourceCodesAsExp <- [e|uncurry generateExpandedIR $ Either.fromRight' $ generateIRsFromSourceCodes toplevelModuleName sourceFiles|]
    (pure . pure) $ ValD (VarP $ mkName "expandedIR") (NormalB generateIRsFromSourceCodesAsExp) []