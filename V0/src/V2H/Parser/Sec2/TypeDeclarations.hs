module V2H.Parser.Sec2.TypeDeclarations where

import V2H.Ast.Sec2.TypeDeclarations
import V2H.Parser.Sec9.Identifiers
import V2H.Lexer
import Text.Parsec
-- | Incomplete production rule
lifetimeNT :: ParserSV Lifetime
lifetimeNT =  svLexeme (string "Static") *> pure Static
              <|> svLexeme (string "Automatic") *> pure Automatic

-- | Incomplete production rule
-- This is just incorrect TODO
packageImportDeclarationNT :: ParserSV PackageImportDeclaration
packageImportDeclarationNT = svLexeme (string "packageImport") *> pure PackageImportDeclaration