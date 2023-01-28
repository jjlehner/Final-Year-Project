module V2H.Ast.Sec2.TypeDeclarations where

data Lifetime = Static | Automatic deriving (Show)

-- | Incomplete production rule
data PackageImportDeclaration = PackageImportDeclaration deriving (Show)