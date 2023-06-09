module V2H.Simple.IRGenerator.Identifiers where
import V2H.IR qualified as IR
import V2H.Simple.Ast qualified as SimpleAst
generatePortIdentifier (SimpleAst.PortIdentifier iden) = IR.PortIdentifierIR iden
generateModuleIdentifier (SimpleAst.ModuleIdentifier iden) = IR.ModuleIdentifierIR iden
generateModuleInstanceIdentifier (SimpleAst.ModuleInstanceIdentifier iden) = IR.ModuleInstanceIdentifierIR iden
generateVariableOrNetIdentifier (SimpleAst.VariableIdentifier iden) = IR.VariableOrNetIdentifierIR iden