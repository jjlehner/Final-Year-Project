{-# LANGUAGE TemplateHaskell #-}
module V2H.CodeGenerator where

import Data.Function
import Data.Map qualified as Map
import Data.List qualified as List
import Data.Maybe qualified as Maybe

import Control.Lens

import Text.Casing

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.Simulator.Signal
import V2H.Simulator.SignalDynamic qualified as Simulate

import Debug.Trace
import Control.Monad.Extra (concatMapM)

defBang = Bang NoSourceUnpackedness NoSourceStrictness
signalDynamicType = ConT ''Simulate.SignalDynamic

moduleIdenToModuleInstanceIden (IR.ModuleIdentifierIR iden) = IR.ModuleInstanceIdentifierIR iden

instanceHierarchyToString = foldl (\a (IR.ModuleInstanceIdentifierIR iden) -> a ++ (toPascal . fromAny) iden) ""

generateSumName instanceIdens (IR.VariableOrNetIdentifierIR iden) =
    mkName $ instanceHierarchyToString instanceIdens ++ (toPascal . fromAny) iden

generateSignalSumConstructors ::
    [IR.ModuleInstanceIdentifierIR]
    -> [IR.IR]
    -> IR.IR
    -> [Con]
generateSignalSumConstructors instanceIdens irs topLevelModule =
    let extract iden = NormalC (generateSumName instanceIdens iden) []
        variables = (extract <$> Map.keys topLevelModule.variables)
        nets = (extract <$> Map.keys topLevelModule.nets)
        applyCasing f (IR.ModuleInstanceIdentifierIR iden) =
            IR.ModuleInstanceIdentifierIR $ f iden
        newInstanceName submodule = instanceIdens ++ [applyCasing (toPascal . fromAny) submodule.submoduleInstanceIdentifier]
        fetchIR s = traceShow s.submoduleIdentifier $ Maybe.fromJust $ List.find (\elem -> elem.moduleIdentifier == s.submoduleIdentifier) irs
        submodules = concatMap (\sub -> generateSignalSumConstructors (newInstanceName sub) irs (fetchIR sub)) topLevelModule.submodules
    in variables ++ nets ++ submodules


generateSignalSumTypesFromIRs ::
    [IR.IR]
    -> IR.IR
    -> Q [Dec]
generateSignalSumTypesFromIRs irs topLevel =
    (pure . pure) $ DataD [] (mkName "SignalSumTypes") [] Nothing (generateSignalSumConstructors [] irs topLevel ) [DerivClause Nothing [ConT ''Show]]

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
generateModuleName (IR.ModuleIdentifierIR iden) = (toPascal . fromAny) iden
generateSubmoduleName (IR.ModuleInstanceIdentifierIR iden) = (toPascal . fromAny) iden
generateVariableFieldName (IR.VariableOrNetIdentifierIR iden) = "_" ++ (toCamel . fromAny) iden
generateSubmoduleFieldName (IR.ModuleInstanceIdentifierIR iden) = "_" ++ (toCamel . fromAny) iden

generateVariableSignal ::
    [IR.ModuleInstanceIdentifierIR]
    -> IR.VariableIR
    -> Type
generateVariableSignal instanceIdens variable =
    AppT (AppT (ConT ''SignalChange) $ PromotedT $ generateSumName instanceIdens variable.identifier)
    $ generateSignalType variable.dataType

generateSubmoduleFieldType ::
    IR.SubmoduleIR
    -> Type
generateSubmoduleFieldType submodule =
    ConT $ mkName $ generateSubmoduleName submodule.submoduleInstanceIdentifier

generateCircuitRecord ::
    [IR.ModuleInstanceIdentifierIR]
    -> [IR.IR]
    -> IR.IR
    -> Maybe Name
    -> Q [Dec]
generateCircuitRecord instanceIdens irs ir instanceName =
    let name = case instanceName of
                    Just n -> n
                    Nothing -> mkName $ generateModuleName ir.moduleIdentifier
        mkVarBangType names types = (\(n,t) -> (mkName n, defBang, t)) <$> zip names types
        variableFieldNames  = (\v -> generateVariableFieldName v.identifier) <$> Map.elems ir.variables
        variableFieldTypes = generateVariableSignal instanceIdens <$> Map.elems ir.variables
        variableVarBangType = mkVarBangType variableFieldNames variableFieldTypes
        submoduleFieldNames = generateSubmoduleFieldName . IR.submoduleInstanceIdentifier <$> ir.submodules
        submoduleFieldTypes = generateSubmoduleFieldType <$> ir.submodules
        submoduleVarBangTypes = mkVarBangType submoduleFieldNames submoduleFieldTypes
        con = RecC name $ variableVarBangType ++ submoduleVarBangTypes
        genSubmodules submodule = generateCircuitRecord (instanceIdens ++ [submodule.submoduleInstanceIdentifier]) irs (Maybe.fromJust $ IR.findIRFromSubmodule irs submodule) (Just $ mkName $ generateSubmoduleName submodule.submoduleInstanceIdentifier)
    in (:) (DataD [] name [] Nothing [con] [DerivClause Nothing [ConT ''Show]]) <$> concatMapM genSubmodules ir.submodules
        -- (++) <$> concatMapM genSubmodules ir.submodules <*>

generateNameFromVariableOrNetIdentifier (IR.VariableOrNetIdentifierIR iden) = mkName iden
generateNameFromSubmoduleInstanceIdentifier (IR.ModuleInstanceIdentifierIR iden) = mkName iden

generateDynamicCircuitRecordFields ::
    IR.IR -> [VarBangType]
generateDynamicCircuitRecordFields topLevelModuleIR =
    let mkField name = (name, defBang, signalDynamicType)
        mkFieldVoni voni = mkField $ generateNameFromVariableOrNetIdentifier voni
        mkFieldSubmodule submodule = mkField $ generateNameFromSubmoduleInstanceIdentifier submodule.submoduleInstanceIdentifier
        voniFields = (mkFieldVoni <$> Map.keys topLevelModuleIR.variables)
        submoduleFields = fmap mkFieldSubmodule topLevelModuleIR.submodules
    in  voniFields ++ submoduleFields

generateDynamicCircuitRecord ::
    IR.IR -> Q [Dec]
generateDynamicCircuitRecord topLevelModuleIR =
    let typeName =
            generateModuleName topLevelModuleIR.moduleIdentifier
            & flip (<>) "Dynamic"
            & mkName
        constr = RecC typeName $ generateDynamicCircuitRecordFields topLevelModuleIR
    in (pure . pure) $ DataD [] typeName [] Nothing [constr] [DerivClause Nothing [ConT ''Show]]