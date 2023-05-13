{-# OPTIONS_GHC -freduction-depth=500 #-}
module              V2H.AstLenses where
import              Control.Lens
import              Data.Generics.Product
import              V2H.Ast qualified as Ast

sourceTextToModuleAnsiHeader :: Ast.SourceText -> [Ast.ModuleAnsiHeader]
sourceTextToModuleAnsiHeader =
    toListOf (types @Ast.ModuleAnsiHeader)

sourceTextToModuleDeclaration :: Ast.SourceText -> [Ast.ModuleDeclaration]
sourceTextToModuleDeclaration =
    toListOf (types @Ast.ModuleDeclaration)

moduleDeclarationToDataDeclarations :: Ast.ModuleDeclaration -> [Ast.DataDeclaration]
moduleDeclarationToDataDeclarations =
    toListOf (types @Ast.DataDeclaration)