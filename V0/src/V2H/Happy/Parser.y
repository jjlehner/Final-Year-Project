{
module Parser
  ( parseSV
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified V2H.Alex.Lexer as L

import V2H.Ast.Sec1.ConfigurationSourceText
import V2H.Ast.Sec1.LibrarySourceText
import V2H.Ast.Sec1.ModuleItems
import V2H.Ast.Sec1.ModuleParametersAndPorts
import V2H.Ast.Sec1.PackageItems
import V2H.Ast.Sec1.SourceText

import V2H.Ast.Sec2.DeclarationAssignments
import V2H.Ast.Sec2.TypeDeclarations

import V2H.Ast.Sec4.GenerateInstantiation
import V2H.Ast.Sec4.InterfaceInstantiation
import V2H.Ast.Sec4.ProgramInstantiation

import V2H.Ast.Sec5.UdpDeclaration

import V2H.Ast.Sec6.AssertionStatements
import V2H.Ast.Sec6.ContinuousAssignmentsAndNetAliasStatements
import V2H.Ast.Sec6.ProceduralBlocksAndAssignments
import V2H.Ast.Sec6.Statements

import V2H.Ast.Sec8.Numbers
import V2H.Ast.Sec8.Primaries

import V2H.Ast.Sec8.Identifiers
}

%name parseSV
%error parseError
%monad { L.Alex } { >>= } { pure }
%lexer { lexer } { L.RangedToken L.EOF _ }


%token
    -- Identifiers
    identifier                  { L.RangedToken (L.Identifier _) _ }
    -- Keywords
    accept_on                   { L.RangedToken L.AcceptOn _ }
    alias                       { L.RangedToken L.Alias _ }
    always                      { L.RangedToken L.Always _ }
    always_comb                 { L.RangedToken L.AlwaysComb _ }
    always_latch                { L.RangedToken L.AlwaysFF _ }


    assign                      { L.RangedToken L.Assign _ }
    automatic                   { L.RangedToken L.Automatic _ }

    endmodule                   { L.RangedToken L.Endmodule _ }

    module                      { L.RangedToken L.Module _ }
    macromodule                 { L.RangedToken L.Macromodule _ }

    static                      { L.RangedToken L.Static _ }

    timeunit                    { L.RangedToken L.Timeunit _ }
    time_unit                   { L.RangedToken (L.TimeUnitOperator _ ) _ }

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

source_text :: { SourceText }
    : optional(timeunits_declaration) many(description) {SourceText $1 $2}


description :: { Description }
    : module_declaration { DModuleDeclaration $1 }

module_declaration :: { ModuleDeclaration }
    : module_ansi_header optional(timeunits_declaration) many(non_port_module_item) endmodule optional(snd(':', endmodule))

timeunits_declaration :: { TimeunitsDeclaration }
    : timeunit time_literal optional(snd('/', time_literal));

time_literal :: { TimeLiteral }
    : unsigned_number time_unit                                 { UnsignedTL $1 $2 }
    : unsigned_number '.' unsigned_number time_unit             { FixedPointTL $1 $2 $3}

-- Incomplete production rule
module_ansi_header :: { ModuleAnsiHeader }
    : many(attribute_instance) module_keyword optional(lifetime) module_identifier ';' { ModuleAnsiHeader { attributeInstances=$1, moduleKeyword=$2, lifetime=$3, moduleIdentifier=$4, packageImportDeclarations = [], parameterPorts = Nothing, ports = []} }

attribute_instance :: { AttributeInstance }
    : '(' '*' attr_spec many(snd(',', attr_spec)) '*' ')'

-- | Incorrect production rule (Constant Expression should not be Identifier)
attr_spec :: { AttrSpec }
    : attr_name optional(snd('=', identifier))          { AttrSpec{ attrName=$1, constantExpression=$2} }

attr_name :: { AttrName }
    : identifier { AttrName $1 }

module_keyword :: { ModuleKeyword }
    : module                                            { Module }
    : macromodule                                       { Macromodule }

lifetime :: { Lifetime }
    : automatic                                         { Automatic }
    : static                                            { Static }

module_identifier :: { ModuleIdentifier }
    : identifier { ModuleIdentifier $1 }

-- Incomplete Production Rule
non_port_module_item :: { NonPortModuleItem }
    : module_or_generate_item                           { NPMIModuleOrGenerateItem $1 }

-- Incomplete Production Rule
module_or_generate_item :: { ModuleOrGenerateItem }
    : optional(attribute_instance) module_common_item   { MOGIModuleCommonItem $1 $2 }

-- Incomplete Production Rule
module_common_item :: { ModuleCommonItem }
    : continuous_assign                                 {  $1 }

-- Incomplete and incorrect (delay3) ignored Production Rule
continuous_assign :: { ContinuousAssign }
    : assign optional(drive_strength) sep1(net_assignment) ;

net_assignment :: { NetAssignment }
    : net_l_value '=' expression

-- Incomplete production rule
net_lvalue :: { NetLValue }
    : ps_or_hierarchical_net_identifier constant_select {}

-- Incomplete production rule
ps_or_hierarchical_net_identifier :: { AssignmentPatternNetLvalue}
    : optional(package_scope) net_identifier            {  }

constant_select :: { ConstantSelect }
    :

[{.member_identifier constant_bit_select}.member_identifier]constant_bit_select [ [ constant_part_select_range ] ]
-- Incomplete production rule
package_scope :: { PackageScope }
    : package_identifier "::"                   { PackageScope $1 }

package_identifier :: { PackageIdentifier }
    : identifier                                { PackageIdentifier $1 }

net_identifier :: { NetIdentifier }
    : identifier                                { NetIdentifier $1 }