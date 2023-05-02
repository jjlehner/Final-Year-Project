containsToken token rangedToken = token == rtToken rangedToken

unsigned_number = fmap L.unTokDecimal $ satisfy $ (\token -> case (rtToken token) of
                                UnsignedNumberT _ -> True
                                _ -> False )
eof = satisfy $ containsToken L.EOF
identifier = fmap L.unTokIdentifier $ satisfy $ (\token -> case (rtToken token) of
                                                    L.Identifier _ -> True
                                                    _ -> False)
fullstop = satisfy $ containsToken L.FullStop
osb = satisfy $ containsToken L.OpenSquareBracket
csb = satisfy $ containsToken L.CloseSquareBracket
colon = satisfy $ containsToken L.Colon

constant_primary = AST.CPLiteral <$> primary_literal <?> "constant_primary"
constant_expression = AST.CEPrimary <$> constant_primary <?> "constant_expression"
constant_bit_select = AST.ConstantBitSelect <$> many( osb *> constant_expression <* csb) <?> "constant_bit_select"
hierarchical_variable_identifier = AST.HierarchicalVariableIdentifier <$> hierarchical_identifier <?> "hierarchical_variable_identifier"
hierarchical_identifier = AST.HierarchicalIdentifier <$> pure False <*> many ((,) <$> identifier <*> constant_bit_select <* fullstop) <*> identifier <?> "hierarchical_identifier"

constant_range :: Prod r String RangedToken AST.ConstantRange
constant_range = AST.CRExpression <$> constant_expression <* colon <*> constant_expression

part_select_range = AST.PSRConstant <$> constant_range <?> "part_select_range"
bit_select = AST.BitSelect <$> many(osb *> expression <* csb) <?> "bit_select"
member_identifier = AST.MemberIdentifier <$> identifier <?> "identifier"
select = AST.Select <$> optional( (,) <$> many((,) <$> member_identifier <*> bit_select) <*> member_identifier) <*> bit_select <*> optional ( osb *> part_select_range <* csb) <?> "select"
variable_lvalue = AST.VLHierarchical <$> pure Nothing <*> hierarchical_variable_identifier <*> select <?> "variable_lvalue"
expression = AST.EPrimary <$> primary <?> "expression"
primary = AST.PLiteral <$> primary_literal <?> "primary"
primary_literal = AST.PLNumber <$> number <?> "primary_literal"
number = AST.NIntegral <$> integral_number <?> "integral_number"
integral_number = AST.INDecimal <$> decimal_number <?> "integral_number"
decimal_number = AST.DNUnsigned <$> unsigned_number <?> "decimal_number"

