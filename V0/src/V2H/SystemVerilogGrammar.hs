module V2H.SystemVerilogGrammar where

import V2H.Ast
import V2H.Parser

import Text.Parsec


-- | Incomplete production rule
descriptionNT =
    DModuleDeclaration <$> moduleDeclarationNT
    -- | DUdpDeclaration <$> udpDeclarationNT
    -- | DInterfaceDeclaration <$> interfaceDeclarationNT
    -- | DProgramDeclaration <$> programDeclarationNT
    -- | DPackageDeclaration <$> many AttributeInstanceNT <*> packageItemNT
    -- | DPackageItem <$> many attributeInstanceNT <*> packageItemNT
    -- | DBindDirective <$> many attributeInstanceNT <*> bindDirectiveNT
    -- | DConfigDeclaration <$> configDeclarationNT

