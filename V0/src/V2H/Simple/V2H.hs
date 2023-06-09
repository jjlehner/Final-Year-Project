
module V2H.Simple.V2H where


import Control.Exception
import Control.Monad
import Control.Monad.Except qualified as Mtl
import Data.Function
import Data.ByteString.Lazy.Char8 qualified as LazyByteString
import              Data.Generics.Product
import V2H.Alex.Lexer qualified as Lexer
import V2H.Simple.Parser qualified as Parser
import V2H.IR qualified as IR
import V2H.IR.DataTypes qualified as IR
import V2H.Simple.IRGenerator as IR
import V2H.CodeGenerator
import V2H.Simulator.Signal
import Text.Pretty.Simple
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.Either.Extra qualified as Either

import V2H.Simple.Transpile



safeReadFile filePath =
    Either.mapLeft show <$> readFileEither filePath
    where
        readFileEither :: FilePath -> IO (Either IOException LazyByteString.ByteString)
        readFileEither x = try $ LazyByteString.readFile x

setup :: String -> [FilePath] -> Q [Dec]
setup toplevelModuleName sourceFilePaths = do
    mapM_ addDependentFile sourceFilePaths
    eitherErrOrSourceCodes <- runIO $ traverse safeReadFile sourceFilePaths
    let eitherErrOrIRs = generateIRsFromSourceCodes toplevelModuleName =<< sequence eitherErrOrSourceCodes
    case eitherErrOrIRs of
        Left err -> do
                        runIO $ print err
                        runIO $ throwIO $ V2HCodeGenError err
                        return []
        Right (toplevelIR, irs) ->
            do
                let expandedIR = generateExpandedIR toplevelIR irs
                code2Gen <- generateSignalSumTypesFromIRs irs toplevelIR (IR.ModuleInstanceIdentifierIR "top")
                            & liftM2 (++) (generateSignalIdentifiers expandedIR)
                            & liftM2 (++) (generateCircuitRecord [IR.ModuleInstanceIdentifierIR "top"] irs toplevelIR)
                            & liftM2 (++) (generateConvertFromDynamicFunction (mkName "top") expandedIR )
                            & liftM2 (++) (generateConvertToDynamicFunction (mkName "fish") expandedIR)
                            & liftM2 (++) (generateEmptyValue expandedIR)
                            & liftM2 (++) (generateExpandedIRValue toplevelModuleName $ Either.fromRight' $ sequence eitherErrOrSourceCodes)
                runIO $ putStrLn $ pprint $ reverse code2Gen
                return code2Gen