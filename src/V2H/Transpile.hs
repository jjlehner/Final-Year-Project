module V2H.Transpile where
import Control.Exception (Exception)
import Data.Function
import qualified V2H.Alex.Lexer as Lexer
import qualified V2H.Parser as Parser
import qualified V2H.IRGenerator as IR
import qualified V2H.IR as IR
import Data.ByteString.Lazy qualified as LazyByteString

newtype V2HCodeGenError = V2HCodeGenError String
instance Show V2HCodeGenError where
    show (V2HCodeGenError str) = str

instance Exception V2HCodeGenError

generateIRFromSourceCode sourceCode =
    Lexer.runLexer sourceCode
    >>= Parser.runParser
    >>= IR.generateIR

generateIRsFromSourceCodes ::
    String
    -> [LazyByteString.ByteString]
    -> Either String (IR.IR, [IR.IR])
generateIRsFromSourceCodes toplevelModuleName x =
    traverse generateIRFromSourceCode x
    & fmap concat
    & (=<<) (findToplevelModule toplevelModuleName)

prettyPrintFoundModules :: [IR.IR] -> [String]
prettyPrintFoundModules =
    fmap (\ir -> "\t\t\"" ++ extract ir.moduleIdentifier ++ "\"")
        where extract (IR.ModuleIdentifierIR str) = str

findToplevelModule :: String -> [IR.IR] -> Either String (IR.IR, [IR.IR])
findToplevelModule toplevelModuleName irs =
    case IR.findIRFromModuleIdentifier irs $ IR.ModuleIdentifierIR toplevelModuleName of
        Just ir -> Right (ir, irs)
        Nothing -> Left $ unlines $ [
                                    "------- !!!Verilog To Haskell Error!!! -------",
                                    "\tCould not find toplevel module \"" ++ toplevelModuleName ++ "\"",
                                    "\tFound modules:"
                                    ] ++ prettyPrintFoundModules irs