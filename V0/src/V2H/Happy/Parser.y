{
module V2H.Happy.Parser
  ( parseSV
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))
import Data.List
import qualified V2H.Alex.Lexer as L

import V2H.Ast

}

%name parseSV source_text
%tokentype { L.RangedToken }
%error { parseError }
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }
%errorhandlertype explist

%token
    -- Identifiers
    identifier_rt                  { L.RangedToken (L.Identifier _) _ }
    -- Keywords
    accept_on                   { L.RangedToken L.AcceptOn _ }
    alias                       { L.RangedToken L.Alias _ }
    always                      { L.RangedToken L.Always _ }
    always_comb                 { L.RangedToken L.AlwaysComb _ }
    always_latch                { L.RangedToken L.AlwaysFf _ }


    assign                      { L.RangedToken L.Assign _ }
    automatic                   { L.RangedToken L.Automatic _ }

    endmodule                   { L.RangedToken L.Endmodule _ }

    module                      { L.RangedToken L.Module _ }
    macromodule                 { L.RangedToken L.Macromodule _ }

    static                      { L.RangedToken L.Static _ }

    timeunit                    { L.RangedToken L.Timeunit _ }
    time_unit                   { L.RangedToken (L.TimeUnitOperator _ ) _ }
    type                        { L.RangedToken L.Type}
    unary_operator              { L.RangedToken (L.UnaryOperator _) _ }
    binary_operator             { L.RangedToken (L.BinaryOperator _) _ }
    inc_or_dec_operator         { L.RangedToken (L.IncOrDecOperator _) _ }
    unary_module_path_operator  { L.RangedToken (L.UnaryModulePathOperator _) _ }
    binary_module_path_operator { L.RangedToken (L.BinaryModulePathOperator _) _ }

    '('                         { L.RangedToken L.OpenBracket _ }
    ')'                         { L.RangedToken L.CloseBracket _ }
    '{'                         { L.RangedToken L.OpenCurlyBracket _ }
    '}'                         { L.RangedToken L.CloseCurlyBracket _ }
    '['                         { L.RangedToken L.OpenSquareBracket _ }
    ']'                         { L.RangedToken L.CloseSquareBracket _ }
    ';'                         { L.RangedToken L.Semicolon _ }
    ':'                         { L.RangedToken L.Colon _ }
    '.'                         { L.RangedToken L.FullStop _ }
    '*'                         { L.RangedToken L.Asterisk _ }
    "::"                        { L.RangedToken L.ColonColon _ }
    '='                         { L.RangedToken L.Equals _ }
    ','                         { L.RangedToken L.Comma _ }
    '/'                         { L.RangedToken L.Backslash _ }
    unsigned_number             { L.RangedToken (L.UnsignedNumberT _) _}
%%
many_rev(p)
    :               { [] }
    | many_rev(p) p { $2 : $1 }

many(p)
    : many_rev(p) { reverse $1 }

optional(p)
    :   { Nothing }
    | p { Just $1 }

fst(p,q)
    : p q                 { $1 }
snd(p,q)
    : p q                 { $2 }
both(p,q)
    : p q                 { ($1,$2) }

sep1(p,q)
    : p many(snd(q,p))    { $1 : $2 }
identifier :: { Identifier }
    : identifier_rt { L.unTokIdentifier $1}
source_text :: { SourceText }
    : optional(timeunits_declaration) many(description) { SourceText $1 $2 }


description :: { Description }
    : module_declaration { DModuleDeclaration $1 }

module_declaration :: { ModuleDeclaration }
    : module_ansi_header optional(timeunits_declaration) many(non_port_module_item) endmodule optional(snd(':', module_identifier)) { MDAnsiHeader {
        moduleAnsiHeader=$1,
        timeunitsDeclarations=$2,
        nonPortModuleItems=$3
    }}

-- | Incorrect production rule
timeunits_declaration :: { TimeunitsDeclaration }
    : timeunit time_literal optional(snd('/', time_literal)) ';' { TimeunitsDeclaration }

time_literal :: { TimeLiteral }
    : unsigned_number time_unit                                 { TLUnsigned (L.unTokDecimal $1) (L.unTokTimeUnit $2) }
    | unsigned_number '.' unsigned_number time_unit             { TLFixedPoint (L.unTokDecimal $1) (L.unTokDecimal $3) (L.unTokTimeUnit $4) }

-- Incomplete production rule
module_ansi_header :: { ModuleAnsiHeader }
    : many(attribute_instance) module_keyword optional(lifetime) module_identifier many(package_import_declaration) optional(parameter_ports) optional(port_declarations) ';' {
        ModuleAnsiHeader {
            attributeInstances=$1,
            moduleKeyword=$2,
            lifetime=$3,
            moduleIdentifier=$4,
            packageImportDeclarations = $5,
            parameterPorts = $6,
            portDeclarations = $7
        }
    }

-- | Referred to as parameter_port_list in Standard
parameter_ports :: { ParameterPorts }
    : '#' '(' param_assignments many(snd(',', parameter_port_declaration)) ')' {
        ParameterPorts {
            parameterAssignments = $1,
            parameterPortDeclaration = $2
        }}
    | '#' '(' parameter_port_declaration many(snd(',' parameter_port_declaration)) ')' {
        ParameterPorts {
            parameterAssignments = [],
            parameterPortDeclaration = $1:$2
        }}
    | '#' '(' ')' {
        ParameterPorts {
            parameterAssignments = [],
            parameterPortDeclaration = []
        }}

-- | Referred to as list_of_param_assignments in Standard
param_assignments :: {[ParameterAssignment]}
    : param_assignment many(snd(',',param_assignment)) { $1:$2 }

parameter_port_declaration :: { ParameterPortDeclaration }
    : parameter_declaration         { PPD $1 }
    | local_parameter_declaration   { PPDLocal $1 }
    | data_type param_assignments   { PPDDataTypeParamAssignments $1 $2 }
    | type type_assignments         { PPDTypeAssignments $2 }

attribute_instance :: { AttributeInstance }
    : '(' '*' attr_spec many(snd(',', attr_spec)) '*' ')' { AttributeInstance ($3:$4) }

-- | Incorrect production rule (Constant Expression should not be Identifier)
attr_spec :: { AttrSpec }
    : attr_name optional(snd('=', identifier))          { AttrSpec{ attrName=$1, constantExpression=$2} }

attr_name :: { AttrName }
    : identifier { AttrName $1 }

module_keyword :: { ModuleKeyword }
    : module                                            { Module }
    | macromodule                                       { Macromodule }

lifetime :: { Lifetime }
    : automatic                                         { Automatic }
    | static                                            { Static }

module_identifier :: { ModuleIdentifier }
    : identifier { ModuleIdentifier $1 }

-- Incomplete Production Rule
non_port_module_item :: { NonPortModuleItem }
    : module_or_generate_item                           { NPMIModuleOrGenerateItem $1 }

-- Incomplete Production Rule
module_or_generate_item :: { ModuleOrGenerateItem }
    : many(attribute_instance) module_common_item   { MOGIModuleCommonItem $1 $2 }

-- Incomplete Production Rule
module_common_item :: { ModuleCommonItem }
    : continuous_assign                                 { MCIContinuousAssign $1 }

-- Incomplete and incorrect (delay3) ignored Production Rule
continuous_assign :: { ContinuousAssign }
    : assign optional(drive_strength) sep1(net_assignment, ',') { ContinuousAssign };

net_assignment :: { NetAssignment }
    : net_lvalue '=' expression                            { NetAssignment }

-- | Incorrect production rule
expression :: { Expression }
    : '[' ']'                                               { Expression }

-- Incomplete production rule
net_lvalue :: { NetLValue }
    : ps_or_hierarchical_net_identifier constant_select     { NetLValue $1 $2 }

-- Incomplete production rule
ps_or_hierarchical_net_identifier :: { PsOrHierachicalNetIdentifier }
    : optional(package_scope) net_identifier                { POHNINet $1 $2 }

-- Incorrect production rule
constant_select :: { ConstantSelect }
    : '[' ']'                                               { ConstantSelect }

-- Incomplete production rule
package_scope :: { PackageScope }
    : package_identifier "::"                               { PSIdentifier $1 }

package_identifier :: { PackageIdentifier }
    : identifier                                            { PackageIdentifier $1 }

net_identifier :: { NetIdentifier }
    : identifier                                            { NetIdentifier $1 }

-- | Incorrect Production Rule
drive_strength :: { DriveStrength }
    : '[' ']'                       { DriveStrength }

{
parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (_,b) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column <> " -- Possible tokens " <> (intercalate ", " b)

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}