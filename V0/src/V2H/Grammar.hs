module V2H.Grammar where


descriptionNT =
    DModuleDeclaration <$> moduleDeclarationNT
    -- | DUdpDeclaration <$> udpDeclarationNT
    -- | DInterfaceDeclaration <$> interfaceDeclarationNT
    -- | DProgramDeclaration <$> programDeclarationNT
    -- | DPackageDeclaration <$> many AttributeInstanceNT <*> packageItemNT
    -- | DPackageItem <$> many attributeInstanceNT <*> packageItemNT
    -- | DBindDirective <$> many attributeInstanceNT <*> bindDirectiveNT
    -- | DConfigDeclaration <$> configDeclarationNT