{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module V2H.Simple.Parser where
import Text.Earley
import Data.Maybe
import Data.Functor
import Control.Applicative
import qualified V2H.Alex.Lexer as Lexer
import qualified V2H.Simple.Ast as SimpleAst

runParser :: [Lexer.RangedToken] -> Either String SimpleAst.SourceText
runParser tokens =
    case fullParses (parser grammar) tokens of
        ([parse], _) -> Right parse
        (parse:_, _ ) -> Left "Ambiguous Grammar, multiple parses"
        (_, report) -> Left $ "Failed to parse -- " ++ show report

isPresent a = fmap isJust (optional a)
eitherProd a b =
    Left <$> a
    <|> Right <$> b

containsToken token rangedToken = token == Lexer.rtToken rangedToken
unsigned_number = Lexer.unTokDecimal <$> satisfy (\token -> case Lexer.rtToken token of
                                Lexer.UnsignedNumberT _ -> True
                                _ -> False )

asterisk = satisfy $ containsToken Lexer.Asterisk
eof = satisfy $ containsToken Lexer.EOF
identifier :: Prod r e Lexer.RangedToken [Char]
identifier = fmap Lexer.unTokIdentifier $ satisfy $ (\token -> case Lexer.rtToken token of
                                                    Lexer.Identifier _ -> True
                                                    _ -> False)
fullstop = satisfy $ containsToken Lexer.FullStop
ob = satisfy $ containsToken Lexer.OpenBracket
cb = satisfy $ containsToken Lexer.CloseBracket
osb = satisfy $ containsToken Lexer.OpenSquareBracket
csb = satisfy $ containsToken Lexer.CloseSquareBracket
colon = satisfy $ containsToken Lexer.Colon
semicolon = satisfy $ containsToken Lexer.Semicolon
static = satisfy $ containsToken Lexer.Static
hashtag = satisfy $ containsToken Lexer.Hashtag
comma = satisfy $ containsToken Lexer.Comma
edge = satisfy $ containsToken Lexer.Edge
forward_slash = satisfy $ containsToken Lexer.Forwardslash

plus_equal = satisfy $  containsToken Lexer.PlusEqual
minus_equal = satisfy $ containsToken Lexer.MinusEqual
asterisk_equal = satisfy $ containsToken Lexer.AsteriskEqual
forwardslash_equal= satisfy $ containsToken Lexer.ForwardslashEqual
percentage_equal = satisfy $ containsToken Lexer.PercentageEqual
ampersand_equal = satisfy $ containsToken Lexer.AmpersandEqual
pipe_equal = satisfy $ containsToken Lexer.PipeEqual
caret_equal = satisfy $ containsToken Lexer.CaretEqual
lesser_Lesser_equal = satisfy $ containsToken Lexer.LesserLesserEqual
greater_greater_equal = satisfy $ containsToken Lexer.GreaterGreaterEqual
lesser_esser_lesser_equal = satisfy $ containsToken Lexer.LesserLesserLesserEqual
greater_greater_greater_equal = satisfy $ containsToken Lexer.GreaterGreaterGreaterEqual
lesser_equal = satisfy $ containsToken Lexer.LesserEqual
ocb = satisfy $ containsToken Lexer.OpenCurlyBracket
ccb = satisfy $ containsToken Lexer.CloseCurlyBracket

greater_greater = satisfy $ containsToken Lexer.GreaterGreater
lesser_lesser = satisfy $ containsToken Lexer.LesserLesser
at_sign = satisfy $ containsToken Lexer.AtSign
dollar = satisfy $ containsToken Lexer.Dollar
plus_colon = satisfy $ containsToken Lexer.PlusColon
minus_colon = satisfy $ containsToken Lexer.MinusColon

ampersand_ampersand_ampersand = satisfy $ containsToken Lexer.AmpersandAmpersandAmpersand
always = satisfy $ containsToken Lexer.Always
always_comb = satisfy $ containsToken Lexer.AlwaysComb
always_ff = satisfy $ containsToken Lexer.AlwaysFf
always_latch = satisfy $ containsToken Lexer.AlwaysLatch
assign = satisfy $ containsToken Lexer.Assign
automatic = satisfy $ containsToken Lexer.Automatic
begin = satisfy $ containsToken Lexer.Begin
colon_colon = satisfy $ containsToken Lexer.ColonColon
local_colon_colon = satisfy $ containsToken Lexer.LocalColonColon
bit = satisfy $ containsToken Lexer.Bit
byte = satisfy $ containsToken Lexer.Byte
const' = satisfy $ containsToken Lexer.Const
else' = satisfy $ containsToken Lexer.Else
end = satisfy $ containsToken Lexer.End
endmodule = satisfy $ containsToken Lexer.Endmodule
equal = satisfy $ containsToken Lexer.Equal
fs = satisfy $ containsToken Lexer.Femtosecond
longint = satisfy $ containsToken Lexer.Longint
macromodule = satisfy $ containsToken Lexer.Macromodule
module' = satisfy $ containsToken Lexer.Module
if' = satisfy $ containsToken Lexer.If
iff = satisfy $ containsToken Lexer.Iff
import' = satisfy $ containsToken Lexer.Import
int = satisfy $ containsToken Lexer.Int
input = satisfy $ containsToken Lexer.Input
inout = satisfy $ containsToken Lexer.Inout
integer = satisfy $ containsToken Lexer.Integer
interface = satisfy $ containsToken Lexer.Interface
ms = satisfy $ containsToken Lexer.Millisecond
ns = satisfy $ containsToken Lexer.Nanosecond
parameter = satisfy $ containsToken Lexer.Parameter
picosecond = satisfy $ containsToken Lexer.Picosecond
real = satisfy $ containsToken Lexer.Real
realtime = satisfy $ containsToken Lexer.Realtime
ref = satisfy $ containsToken Lexer.Ref
localparam = satisfy $ containsToken Lexer.Localparam
logic = satisfy $ containsToken Lexer.Logic
negedge = satisfy $ containsToken Lexer.Negedge
output = satisfy $ containsToken Lexer.Output
posedge = satisfy $ containsToken Lexer.Posedge
priority = satisfy $ containsToken Lexer.Priority
ps = satisfy $ containsToken Lexer.Picosecond
reg = satisfy $ containsToken Lexer.Reg
s = satisfy $ containsToken Lexer.Second
signed = satisfy $ containsToken Lexer.Signed
shortint= satisfy $ containsToken Lexer.Shortint
shortreal = satisfy $ containsToken Lexer.Shortreal
super = satisfy $ containsToken Lexer.Super
supply0 = satisfy $ containsToken Lexer.Supply0
supply1 = satisfy $ containsToken Lexer.Supply1
this = satisfy $ containsToken Lexer.This
time = satisfy $ containsToken Lexer.Time
tri     = satisfy $ containsToken Lexer.Tri
triand  = satisfy $ containsToken Lexer.Triand
trior   = satisfy $ containsToken Lexer.Trior
trireg  = satisfy $ containsToken Lexer.Trireg
tri0    = satisfy $ containsToken Lexer.Tri0
tri1    = satisfy $ containsToken Lexer.Tri1
timeunit = satisfy $ containsToken Lexer.Timeunit
us = satisfy $ containsToken Lexer.Microsecond
uwire   = satisfy $ containsToken Lexer.Uwire
unsigned = satisfy $ containsToken Lexer.Unsigned
unique = satisfy $ containsToken Lexer.Unique
unique0 = satisfy $ containsToken Lexer.Unique0
type' = satisfy $ containsToken Lexer.Type
var = satisfy $ containsToken Lexer.Var
wand    = satisfy $ containsToken Lexer.Wand
wire    = satisfy $ containsToken Lexer.Wire
with    = satisfy $ containsToken Lexer.With
wor     = satisfy $ containsToken Lexer.Wor
--- Sec 1 -----
-- 1.1 - Library Source Text ----
---- 1.2 - SystemVerilog Source Text ----

grammar = mdo
    source_text <- rule $
        SimpleAst.SourceText <$> many description <?> "source_text"

    description <- rule $
        SimpleAst.DModuleDeclaration
        <$> module_declaration
        <?> "description"
    module_header <- rule $ SimpleAst.ModuleHeader
                            <$> (module_keyword *> module_identifier)
                            <*> optional port_declarations <* semicolon
                            <?> "module_ansi_header"

    module_declaration <- rule $ SimpleAst.ModuleDeclaration
                            <$> module_header
                            <*> many non_port_module_item
                            <*  endmodule
                            <?> "module_declaration"

    module_keyword <- rule $ (module' $> SimpleAst.MKModule)
                            <|> macromodule $> SimpleAst.MKMacromodule <?> "module_keyword"

    port_declarations <- rule $ (:)
                                <$> (ob *> port_declaration)
                                <*> many (comma *> port_declaration) <* cb
                                <?> "port_declarations"

    port_declaration <- rule $
                                SimpleAst.PortDeclaration SimpleAst.PDInput
                                <$> (input *> optional net_type)
                                <*> optional data_type
                                <*> port_identifier
                                <|> SimpleAst.PortDeclaration SimpleAst.PDOutput
                                <$> (output *> optional net_type)
                                <*> optional data_type
                                <*> port_identifier <?> "port_declaration"

    non_port_module_item <- rule $ SimpleAst.NPMIDataDeclaration <$> data_declaration
                                   <|> SimpleAst.NPMIAlwaysConstruct <$> always_construct
                                   <|> SimpleAst.NPMIModuleInstantiation <$> module_instantiation <?> "non_port_module_item"

    always_construct <- rule $ SimpleAst.ACComb  <$> (always_comb *> statement_item) <?> "always_construct"
    statement_item <- rule $ SimpleAst.SIBlockingAssignment <$> blocking_assignment
                             <|> SimpleAst.SISeqBlock <$> seq_block <?> "statement_item"

    blocking_assignment <- rule $ SimpleAst.BlockingAssignment <$> variable_lvalue <* equal <*> expression <* semicolon <?> "blocking_assignment"
    seq_block <- rule $ SimpleAst.SeqBlock <$> (begin *> many statement_item) <* end <?> "seq_block"
    expression <- rule $ SimpleAst.ELiteral <$> unsigned_number
                            <|> SimpleAst.EConnection <$> variable_identifier <*> optional bit_select <*> optional part_select_range <?> "expression"
    data_declaration <- rule $ SimpleAst.DataDeclaration <$> data_type <*> variable_identifier <* semicolon <?> "data_declaration"
    net_type <- rule $ (wire $> SimpleAst.NTWire) <?> "net_type"
    data_type <- rule $ SimpleAst.DTIntegerVector <$> integer_vector_type <?> "dataType"
    integer_vector_type <- rule $
                                bit $> SimpleAst.IVTBit
                                <|> logic $> SimpleAst.IVTLogic
                                <|> reg $> SimpleAst.IVTReg
                                <?> "integer_vector_type"
    module_identifier <- rule $ SimpleAst.ModuleIdentifier <$> identifier <?> "module_identifier"
    port_identifier <- rule $ SimpleAst.PortIdentifier <$> identifier <?> "port_identifier"
    variable_identifier <- rule $ SimpleAst.VariableIdentifier <$> identifier <?> "variable_identifier"
    variable_lvalue <- rule $ SimpleAst.VariableLvalue
                                <$> variable_identifier
                                <*> optional bit_select
                                <*> optional part_select_range <?> "variable_lvalue"
    bit_select <- rule $ SimpleAst.BitSelect <$> (osb *> expression <* csb) <?> "bit_select"
    part_select_range <- rule $ SimpleAst.PartSelectRange <$> (osb *> unsigned_number) <*> (colon *> unsigned_number) <*csb <?> "part_select_range"
    module_instantiation <- rule $ SimpleAst.ModuleInstantiation <$> module_identifier <*> hierarchical_instance <* semicolon <?> "module_instantiation"
    hierarchical_instance <- rule $ SimpleAst.HierarchicalInstance <$> (module_instance_identifier <* ob) <*> ((:) <$> port_connection <*> many (comma *> port_connection)) <* cb
    port_connection <- rule $ SimpleAst.PCNamed <$> (fullstop *> port_identifier) <*> (ob *> expression <* cb) <?> "port_connection"
    module_instance_identifier <- rule $ SimpleAst.ModuleInstanceIdentifier <$> identifier <?> "instance_identifier"
    return (source_text <* eof)