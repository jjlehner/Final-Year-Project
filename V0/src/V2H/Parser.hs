{-# LANGUAGE RecursiveDo #-}

module V2H.Parser where
import Text.Earley
import Data.Maybe
import Control.Applicative
import qualified V2H.Alex.Lexer as L
import qualified V2H.Ast as Ast
import V2H.Ast (StreamConcatenation)
isPresent a = fmap isJust (optional a)
eitherProd a b =
    Left <$> a
    <|> Right <$> b

containsToken token rangedToken = token == L.rtToken rangedToken
unsigned_number = fmap L.unTokDecimal $ satisfy $ (\token -> case (L.rtToken token) of
                                L.UnsignedNumberT _ -> True
                                _ -> False )

asterisk = satisfy $ containsToken L.Asterisk
eof = satisfy $ containsToken L.EOF
identifier = fmap L.unTokIdentifier $ satisfy $ (\token -> case (L.rtToken token) of
                                                    L.Identifier _ -> True
                                                    _ -> False)
fullstop = satisfy $ containsToken L.FullStop
ob = satisfy $ containsToken L.OpenBracket
cb = satisfy $ containsToken L.CloseBracket
osb = satisfy $ containsToken L.OpenSquareBracket
csb = satisfy $ containsToken L.CloseSquareBracket
colon = satisfy $ containsToken L.Colon
semicolon = satisfy $ containsToken L.Semicolon
static = satisfy $ containsToken L.Static
hashtag = satisfy $ containsToken L.Hashtag
comma = satisfy $ containsToken L.Comma
edge = satisfy $ containsToken L.Edge
forward_slash = satisfy $ containsToken L.Forwardslash

plus_equal = satisfy $  containsToken L.PlusEqual
minus_equal = satisfy $ containsToken L.MinusEqual
asterisk_equal = satisfy $ containsToken L.AsteriskEqual
forwardslash_equal= satisfy $ containsToken L.ForwardslashEqual
percentage_equal = satisfy $ containsToken L.PercentageEqual
ampersand_equal = satisfy $ containsToken L.AmpersandEqual
pipe_equal = satisfy $ containsToken L.PipeEqual
caret_equal = satisfy $ containsToken L.CaretEqual
lesser_Lesser_equal = satisfy $ containsToken L.LesserLesserEqual
greater_greater_equal = satisfy $ containsToken L.GreaterGreaterEqual
lesser_esser_lesser_equal = satisfy $ containsToken L.LesserLesserLesserEqual
greater_greater_greater_equal = satisfy $ containsToken L.GreaterGreaterGreaterEqual
lesser_equal = satisfy $ containsToken L.LesserEqual
ocb = satisfy $ containsToken L.OpenCurlyBracket
ccb = satisfy $ containsToken L.CloseCurlyBracket

greater_greater = satisfy $ containsToken L.GreaterGreater
lesser_lesser = satisfy $ containsToken L.LesserLesser
at_sign = satisfy $ containsToken L.AtSign
dollar = satisfy $ containsToken L.Dollar
plus_colon = satisfy $ containsToken L.PlusColon
minus_colon = satisfy $ containsToken L.MinusColon

ampersand_ampersand_ampersand = satisfy $ containsToken L.AmpersandAmpersandAmpersand
always = satisfy $ containsToken L.Always
always_comb = satisfy $ containsToken L.AlwaysComb
always_ff = satisfy $ containsToken L.AlwaysFf
always_latch = satisfy $ containsToken L.AlwaysLatch
assign = satisfy $ containsToken L.Assign
automatic = satisfy $ containsToken L.Automatic
begin = satisfy $ containsToken L.Begin
colon_colon = satisfy $ containsToken L.ColonColon
local_colon_colon = satisfy $ containsToken L.LocalColonColon
bit = satisfy $ containsToken L.Bit
byte = satisfy $ containsToken L.Byte
const' = satisfy $ containsToken L.Const
else' = satisfy $ containsToken L.Else
end = satisfy $ containsToken L.End
endmodule = satisfy $ containsToken L.Endmodule
equal = satisfy $ containsToken L.Equal
fs = satisfy $ containsToken L.Femtosecond
longint = satisfy $ containsToken L.Longint
macromodule = satisfy $ containsToken L.Macromodule
module' = satisfy $ containsToken L.Module
if' = satisfy $ containsToken L.If
iff = satisfy $ containsToken L.Iff
import' = satisfy $ containsToken L.Import
int = satisfy $ containsToken L.Int
input = satisfy $ containsToken L.Input
inout = satisfy $ containsToken L.Inout
integer = satisfy $ containsToken L.Integer
interface = satisfy $ containsToken L.Interface
ms = satisfy $ containsToken L.Millisecond
ns = satisfy $ containsToken L.Nanosecond
parameter = satisfy $ containsToken L.Parameter
picosecond = satisfy $ containsToken L.Picosecond
real = satisfy $ containsToken L.Real
realtime = satisfy $ containsToken L.Realtime
ref = satisfy $ containsToken L.Ref
localparam = satisfy $ containsToken L.Localparam
logic = satisfy $ containsToken L.Logic
negedge = satisfy $ containsToken L.Negedge
output = satisfy $ containsToken L.Output
posedge = satisfy $ containsToken L.Posedge
priority = satisfy $ containsToken L.Priority
ps = satisfy $ containsToken L.Picosecond
reg = satisfy $ containsToken L.Reg
s = satisfy $ containsToken L.Second
signed = satisfy $ containsToken L.Signed
shortint= satisfy $ containsToken L.Shortint
shortreal = satisfy $ containsToken L.Shortreal
super = satisfy $ containsToken L.Super
supply0 = satisfy $ containsToken L.Supply0
supply1 = satisfy $ containsToken L.Supply1
this = satisfy $ containsToken L.This
time = satisfy $ containsToken L.Time
tri     = satisfy $ containsToken L.Tri
triand  = satisfy $ containsToken L.Triand
trior   = satisfy $ containsToken L.Trior
trireg  = satisfy $ containsToken L.Trireg
tri0    = satisfy $ containsToken L.Tri0
tri1    = satisfy $ containsToken L.Tri1
timeunit = satisfy $ containsToken L.Timeunit
us = satisfy $ containsToken L.Microsecond
uwire   = satisfy $ containsToken L.Uwire
unsigned = satisfy $ containsToken L.Unsigned
unique = satisfy $ containsToken L.Unique
unique0 = satisfy $ containsToken L.Unique0
type' = satisfy $ containsToken L.Type
var = satisfy $ containsToken L.Var
wand    = satisfy $ containsToken L.Wand
wire    = satisfy $ containsToken L.Wire
with    = satisfy $ containsToken L.With
wor     = satisfy $ containsToken L.Wor
--- Sec 1 -----
-- 1.1 - Library Source Text ----
---- 1.2 - SystemVerilog Source Text ----

grams = mdo
    source_text <- rule $
        Ast.SourceText
        <$> optional timeunits_declaration
        <*> many(description)
        <?> "source_text"

    description <- rule $
        Ast.DModuleDeclaration
        <$> module_declaration
        <?> "description"
    -- module_nonansi_header :: { ModuleNonansiHeader }

    -- Incomplete production rule
    module_ansi_header <- rule $ Ast.ModuleAnsiHeader
                            <$> many attribute_instance
                            <*> module_keyword
                            <*> optional lifetime
                            <*> module_identifier
                            <*> many package_import_declaration
                            <*> optional parameter_ports
                            <*> optional port_declarations <* semicolon
                            <?> "module_ansi_header"

    module_declaration <- rule $ Ast.MDAnsiHeader
                            <$> module_ansi_header
                            <*> optional timeunits_declaration
                            <*> many non_port_module_item
                            <* endmodule
                            <*> optional (colon *> module_identifier)
                            <?> "module_declaration"

    module_keyword <- rule $    module' *> (pure Ast.MKModule)
                        <|> (macromodule *> pure Ast.MKMacromodule)
                        <?> "module_keyword"

    -- interface_declaration :: { InterfaceDeclaration }
    -- interface_nonansi_header :: { InterfaceNonansiHeader }
    -- interface_ansi_header :: { InterfaceAnsiHeader }
    -- program_declaration :: { ProgramDeclaration }
    -- program_nonansi_header :: { ProgramNonansiHeader }
    -- program_ansi_header :: { ProgramAnsiHeader }
    -- checker_declaration :: { CheckerDeclaration }
    -- class_declaration :: { ClassDeclaration }
    -- interface_class_type :: { InterfaceClassType }
    -- interface_class_declaration :: { InterfaceClassDeclaration }
    -- interface_class_item :: { InterfaceClassItem }
    -- interface_class_method :: { InterfaceClassMethod }
    -- package_declaration :: { PackageDeclaration }

    -- | Incorrect production rule
    timeunits_declaration <- rule $ timeunit
                            *> time_literal
                            *> optional (forward_slash *> time_literal)
                            *> pure Ast.TimeunitsDeclaration
                            *> semicolon
                            *> pure Ast.TimeunitsDeclaration
                            <?> "timeunits_declaration"

    ---- 1.3 - Module parameters and Ports ----
    -- | Referred to as parameter_port_list in Standard
    parameter_ports <- rule $   Ast.ParameterPorts
                        <$> (hashtag *> ob *> param_assignments)
                        <*> many (comma *> parameter_port_declaration) <* cb
                        <|> Ast.ParameterPorts []
                        <$> (hashtag *> ob *> ((:) <$> parameter_port_declaration <*> many (comma *> parameter_port_declaration)) <* cb)
                        <|> hashtag *> ob *> cb *> pure (Ast.ParameterPorts [] [])
                        <?> "parameter_ports"

    parameter_port_declaration <- rule $
        Ast.PPD <$> parameter_declaration
        <|> Ast.PPDLocal <$> local_parameter_declaration
        <|> Ast.PPDDataTypeParamAssignments <$> data_type <*> param_assignments
        <|> Ast.PPDTypeAssignments <$> (type' *> type_assignments)
        <?> "parameter_port_declaration"

    port_declarations <- rule $
        ob *> cb *> pure []
        <|> (:) <$> (ob *> port_declarations_item) <*> many(comma *> port_declarations_item) <* cb
        <?> "port_declarations"

    port_declarations_item <- rule $
        Ast.PortDeclarationsItem <$> many attribute_instance <*> ansi_port_declaration <?> "port_declarations_item"
    -- port_declaration :: { PortDeclaration }
    -- port :: { Port }
    -- port_expression :: { PortExpression }
    -- port_reference :: { PortReference }

    port_direction <- rule $
        input *> pure Ast.PDInput
        <|> output *> pure Ast.PDOutput
        <|> inout *> pure Ast.PDInout
        <|> ref *> pure Ast.PDRef
        <?> "port_direction"

    net_port_header <- rule $ Ast.NetPortHeader <$> optional port_direction <*> net_port_type <?> "net_port_header"
    variable_port_header <- rule $ Ast.VariablePortHeader <$> optional port_direction <*> variable_port_type <?> "variabler_port_header"
    interface_port_header <- rule $ Ast.IPDNamed <$> interface_identifier <*> optional (fullstop *> modport_identifier)
                            <|> Ast.IPDAnonymous <$> (interface *> optional (fullstop *> modport_identifier)) <?> "interface_port_header"

    -- Incomplete Production Rule
    ansi_port_declaration <- rule $
        Ast.APDNetOrInterfaceHeader
        <$> optional net_or_interface_port_header
        <*> port_identifier
        <*> many unpacked_dimension
        <*> optional(equal *> constant_expression)
        <|> Ast.APDVariableHeader
        <$> optional variable_port_header
        <*> port_identifier
        <*> many variable_dimension
        <*> optional (equal *> constant_expression)
        <?> "ansi_port_declaration"

    net_or_interface_port_header <- rule $
        Left <$> net_port_header
        <|> Right <$> interface_port_header
        <?> "net_or_interface_port_header"

    ---- 1.4 - Module Items ----
    -- elaboration_system_task :: { ElaborationSystemTask }
    -- finish_number :: { FinishNumber }

    -- Incomplete Production Rule
    module_common_item <- rule $
        Ast.MCIModuleOrGenerateItemDeclaration <$> module_or_generate_item_declaration
        <|> Ast.MCIContinuousAssign <$> continuous_assign
        <|> Ast.MCIAlwaysConstruct <$> always_construct
        <?> "module_common_item"
    -- module_item :: { ModuleItem }
    -- Incomplete Production Rule
    module_or_generate_item <- rule $
        Ast.MOGIModuleInstantiation <$> many attribute_instance <*> module_instantiation
        <|> Ast.MOGIModuleCommonItem <$> many attribute_instance <*> module_common_item <?> "module_or_generate_item"

    -- Incomplete Production Rule
    module_or_generate_item_declaration <- rule $
        Ast.MOGIDPackageOrGenerateItemDeclaration <$> package_or_generate_item_declaration <?> "module_or_generate_item_declaration"

    -- Incomplete Production Rule
    non_port_module_item <- rule $
        Ast.NPMIModuleOrGenerateItem <$> module_or_generate_item <?> "non_port_module_item"
    -- parameter_override :: { ParameterOverride }
    -- bind_directive :: { BindDirective }
    -- bind_target_scope :: { BindTargetScope }
    -- bind_target_instance :: { BindTargetInstance }
    -- bind_instantiation :: { BindInstantiation }

    ---- 1.5 - Configuration Source Text ----
    -- config_declaration :: { ConfigDeclaration }
    -- design_statement :: { DesignStatement }
    -- config_rule_statement :: { ConfigRuleStatement }
    -- default_clause :: { DefaultClause }
    -- inst_clause :: { InstClause }
    -- inst_name :: { InstName }
    -- cell_clause :: { CellClause }
    -- liblist_clause :: { LiblistClause }
    -- use_clause :: { UseClause }

    ---- 1.6 - Interface Items ----
    -- interface_or_generate_item :: { InterfaceOrGenerateItem }
    -- extern_tf_declaration :: { ExternTfDeclaration }
    -- interface_item :: { InterfaceItem }
    -- non_port_interface_item :: { NonPortInterfaceItem }

    ---- 1.7 - Program Items ----
    -- program_item :: { ProgramItem }
    -- non_port_program_item :: { NonPortProgramItem }
    -- program_generate_item :: { ProgramGenerateItem }

    ---- 1.8 - Checker Items ----
    -- checker_port_list :: { CheckerPortList }
    -- checker_port_item :: { CheckerPortItem }
    -- checker_port_direction :: { CheckerPortDirection }
    -- checker_or_generate_item :: { CheckerOrGenerateItem }
    -- checker_or_generate_item_declaration :: { CheckerOrGenerateItemDeclaration }
    -- checker_generate_item :: { CheckerGenerateItem }

    ---- 1.9 - Class Items ----
    -- class_item :: { ClassItem }
    -- class_property :: { ClassProperty }
    -- class_method :: { ClassMethod }
    -- class_constructor_prototype :: { ClassConstructorPrototype }
    -- class_constraint :: { ClassConstraint }
    -- class_item_qualifier :: { ClassItemQualifier }
    -- property_qualifier :: { PropertyQualifier }
    -- random_qualifier :: { RandomQualifier }
    -- method_qualifier :: { MethodQualifier }
    -- method_prototype :: { MethodPrototype }
    -- class_constructor_declaration :: { ClassConstructorDeclaration }
    ---- 1.10 - Constraints ----
    -- constraint_declaration :: { ConstraintDeclaration }
    -- constraint_block :: { ConstraintBlock }
    -- constraint_block_item :: { ConstraintBlockItem }
    -- solve_before_list :: { SolveBeforeList }
    -- constraint_primary :: { ConstraintPrimary }
    -- constraint_expression :: { ConstraintExpression }
    -- uniqueness_constraint :: { UniquenessConstraint }
    -- constraint_set :: { ConstraintSet }
    -- dist_list :: { DistList }
    -- dist_item :: { DistItem }
    -- dist_weight :: { DistWeight }
    -- constraint_prototype :: { ConstraintPrototype }
    -- constraint_prototype_qualifier :: { ConstraintPrototypeQualifier }
    -- extern_constraint_declaration :: { ExternConstraintDeclaration }
    -- identifier_list :: { IdentifierList }

    ---- 1.11 - Package Items ----
    -- packageItem :: { PackageItem }

    -- | Incomplete Production Rule
    package_or_generate_item_declaration <- rule $
        Ast.POGIDData <$> data_declaration <?> "package_or_generate_item_declaration"
    -- anonymous_program :: { AnonymousProgram }
    -- anonymous_program_item :: { AnonymousProgramItem }

    ----- Sec 2 -----
    ---- 2.1.1 - Module Parameter Declarations ----
    local_parameter_declaration <- rule $
        Ast.LPDParamAssignments <$> (localparam *> data_type_or_implicit) <*> param_assignments
        <|> Ast.LPDTypeAssignments <$> (localparam *> type'*> type_assignments)
        <?> "local_parameter_declaration"

    parameter_declaration <- rule $
        Ast.PDDataTypeOrImplicit <$> (parameter *> data_type_or_implicit) <*> param_assignments
        <|> Ast.PDTypeAssignments <$> (parameter *> type'*> type_assignments)
        <?> "parameter_declaration"
    -- specparam_declaration :: { SpecparamDeclaration }

    ---- 2.1.2 - Port Declarations ----
    -- inout_declaration :: { InoutDeclaration }
    -- input_declaration :: { InputDeclaration }
    -- output_declaration :: { OutputDeclaration }
    -- interface_port_declaration :: { InterfacePortDeclaration }
    -- ref_declaration :: { RefDeclaration }

    ---- 2.1.3 - Type Declarations ----
    data_declaration <- rule $
        Ast.DD <$> isPresent const'
        <*> isPresent var
        <*> optional lifetime
        <*> data_type_or_implicit
        <*> variable_decl_assignments
        <* semicolon
        <?> "data_declaration"

    package_import_declaration <- rule $
        Ast.PackageImportDeclaration
        <$> (import' *> ((:) <$> package_import_item <*> many (comma *> package_import_item)))
        <?> "package_import_declaration"

    package_import_item <- rule $
        Ast.PIIIdentifier <$> package_identifier <* colon_colon <*> identifier
        <|> Ast.PIIWildcard <$> package_identifier <* colon_colon <* asterisk
        <?> "package_import_item"

    -- package_export_declaration :: { PackageExportDeclaration }
    -- genvar_declaration :: { GenvarDeclaration }
    -- net_declaration :: { NetDeclaration }
    -- type_declaration :: { TypeDeclaration }
    -- net_type_declaration :: { NetTypeDeclaration }
    lifetime <- rule $
        automatic *> pure Ast.LAutomatic
        <|> static *> pure Ast.LStatic
        <?> "lifetime"

    ---- 2.2.1 - Net and Variable Types ----
    -- casting_type :: { CastingType }
    -- data_type :: { DataType }

    -- | Incomplete Production Rule
    data_type <- rule $
        Ast.DTIntegerVector <$> integer_vector_type <*> optional signing <*> many packed_dimension <?> "data_type"

    data_type_or_implicit <- rule $
        Left <$> data_type
        <|> Right <$> implicit_data_type
        <?> "data_type_or_implicit"
    implicit_data_type <- rule $
        Ast.ImplicitDataType <$> optional signing <*> many packed_dimension <?> "implicit_data_type"
    -- enum_base_type :: { EnumBaseType }
    -- enum_name_declaration :: { EnumNameDeclaration }
    class_scope <- rule $
        Ast.ClassScope <$> class_type <* colon_colon
    class_type <- rule $
        Ast.ClassType <$> ps_class_identifier <*> optional parameter_value_assignment <*> many class_identifier_parameter_value_assignment <?> "class_type"

    class_identifier_parameter_value_assignment <- rule $
        Ast.ClassIdentifierParameterValueAssignment
        <$> (colon_colon *> class_identifier)
        <*> optional parameter_value_assignment
        <?> "class_identifier_parameter_value_assignment"

    integer_type <- rule $
        Ast.ITVector <$> integer_vector_type
        <|> Ast.ITAtom <$> integer_atom_type
        <?> "integer_type"

    integer_atom_type <- rule $
        byte *> pure Ast.IATByte
        <|> shortint *> pure Ast.IATShortint
        <|> int *> pure Ast.IATInt
        <|> longint *> pure Ast.IATLongint
        <|> integer *> pure Ast.IATInteger
        <|> time *> pure Ast.IATTime
        <?> "integer_atom_type"

    integer_vector_type <- rule $
        bit *> pure Ast.IVTBit
        <|> logic *> pure Ast.IVTLogic
        <|> reg *> pure Ast.IVTReg
        <?> "integer_vector_type"

    non_integer_type <- rule $
        shortreal *> pure Ast.NITShortreal
        <|> real *> pure Ast.NITReal
        <|> realtime *> pure Ast.NITRealtime
        <?> "non_integer_type"

    net_type <- rule $
        supply0 *> pure Ast.Supply0
        <|> supply1 *> pure Ast.Supply1
        <|> tri     *> pure Ast.Tri
        <|> triand  *> pure Ast.Triand
        <|> trior   *> pure Ast.Trior
        <|> trireg  *> pure Ast.Trireg
        <|> tri0    *> pure Ast.Tri0
        <|> tri1    *> pure Ast.Tri1
        <|> uwire   *> pure Ast.Uwire
        <|> wire    *> pure Ast.Wire
        <|> wand    *> pure Ast.Wand
        <|> wor     *> pure Ast.Wor
        <?> "net_type"

    net_port_type <- rule $
        Ast.NPTDataOrImplicit
        <$> optional net_type
        <*> data_type_or_implicit
        <?> "net_port_type"

    variable_port_type <- rule $
        Ast.VariablePortType <$> var_data_type <?> "variable_port_type"

    var_data_type <- rule $
        Ast.VDT <$> data_type
        <|> Ast.VDTOrImplicit <$> (var *> data_type_or_implicit)
        <?> "var_data_type"

    signing <- rule $
        signed *> pure Ast.SSigned
        <|> unsigned *> pure Ast.SUnsigned
        <?> "signing"

    simple_type <- rule $
        Ast.STInteger <$> integer_type
        <|> Ast.STNonInteger <$> non_integer_type
        <|> Ast.STPsIdentifier <$> ps_type_identifier
        <|> Ast.STPsParameterIdentifier <$> ps_parameter_identifier

    -- struct_union_member :: { StructUnionMember }
    -- data_type_or_void :: { DataTypeOrVoid }
    -- struct_union :: { StructUnion }
    type_reference <- rule $
        Ast.TRExpression <$> (type' *> ob *> expression) <* cb
        <|> Ast.TRDataType <$> (type' *>  ob *> data_type) <* cb
        <?> "type_reference"

    ---- 2.2.2 - Strengths ----
    -- | Incorrect Production Rule
    drive_strength <- rule $
        osb *> csb *> pure Ast.DriveStrength <?> "drive_strength"

    -- strength0 :: { Strength0 }
    -- strength1 :: { Strength1 }
    -- charge_strength :: { ChargeStrength }

    ---- 2.2.3 - Delays ----
    delay3 <- rule $ Ast.D3Value <$> (hashtag *> delay_value)
            <?> "delay3"

    -- delay2 :: { Delay2 }
    delay_value <- rule $
        Ast.DVUnsigned <$> unsigned_number <?> "delay_value"

    ---- 2.3 - Declarations Lists ----
    param_assignments <- rule $
        (:) <$> param_assignment <*> many (comma *> param_assignment) <?> "param_assignments"
    type_assignments <- rule $
        (:) <$> type_assignment <*> many (comma *> type_assignment) <?> "type_assignments"
    variable_decl_assignments <- rule $
        (:) <$> variable_decl_assignment <*> many ( comma *> variable_decl_assignment) <?> "variable_decl_assignments"

    ---- 2.4 - Declarations Assignments ----
    -- defparam_assignment :: { DefparamAssignment }
    -- net_decl_assignment :: { NetDeclAssignment }

    param_assignment <- rule $
        Ast.ParamAssignment
        <$> parameter_identifier
        <*> many unpacked_dimension
        <*> optional constant_param_expression
        <?> "param_assignment"

    -- specparam_assignment :: { SpecparamAssignment }
    type_assignment <- rule $
        Ast.TypeAssignment <$> type_identifier <*> optional ( equal *> data_type )
        <?> "type_assignment"
    -- pulse_control_specparam :: { PulseControlSpecparam }
    -- error_limit_value :: { ErrorLimitValue }
    -- reject_limit_value :: { RejectLimitValue }
    -- limit_value :: { LimitValue }
    variable_decl_assignment <- rule $
        Ast.VDA
        <$> variable_identifier
        <*> many variable_dimension
        <*> optional expression
        <?> "variable_decl_assignment"

    -- class_new :: { ClassNew }
    -- dynamic_array_new :: { DynamicArrayNew }

    ---- 2.5 - Declaration Ranges ----
    unpacked_dimension <- rule $
        Ast.UDConstantRange <$> (osb *> constant_range) <* csb
        <|> Ast.UDConstantExpression <$> (osb *> constant_expression) <* csb
        <?> "unpacked_dimension"

    packed_dimension <- rule $
        Ast.PDConstantRange <$> (osb *> constant_range) <* csb
        <|> Ast.PDUnsized <$> unsized_dimension
        <?> "packed_dimension"
    -- associative_dimension :: { AssociativeDimension }
    variable_dimension <- rule $
        Ast.VDUnsized <$> unsized_dimension
        <|> Ast.VDUnpacked <$> unpacked_dimension
        <?> "variable_dimension"
    -- queue_dimension :: { QueueDimension }
    unsized_dimension <- rule $
        osb *> csb *> pure Ast.UnsizedDimension
    ---- 2.6 - Function Declarations ----
    -- function_data_type_or_implicit :: { FunctionDataTypeOrImplicit }
    -- function_declaration :: { FunctionDeclaration }
    -- function_body_declaration :: { FunctionBodyDeclaration }
    -- function_prototype :: { FunctionPrototype }
    -- dpi_import_export :: { DpiImportExport }
    -- dpi_spec_string :: { DpiSpecString }
    -- dpi_function_import_property :: { DpiFunctionImportProperty }
    -- dpi_function_proto :: { DpiFunctionProto }
    -- dpi_task_proto :: { DpiTaskProto }

    ---- 2.7 - Task Declarations ----
    -- task_declaration :: { TaskDeclaration }
    -- task_body_declaration :: { TaskBodyDeclaration }
    -- tf_item_declaration :: { TfItemDeclaration }
    -- tf_port_list :: { TfPortList }
    -- tf_port_item :: { TfPortItem }
    -- tf_port_direction :: { TfPortDirection }
    -- tf_port_declaration :: { TfPortDeclaration }
    -- task_prototype :: { TaskPrototype }

    ---- 2.8 - Block Item Declarations ----
    -- | Incomplete production rule
    block_item_declaration <- rule $
        Ast.BIDData <$> many attribute_instance <*> data_declaration <?> "block_item_declaration"
    -- overload_declaration :: { OverloadDeclaration }
    -- overload_operator :: { OverloadOperator }

    ---- 2.9 - Interface Declarations ----
    -- modport_declaration :: { ModportDeclaration }
    -- modport_item :: { ModportItem }
    -- modport_ports_declaration :: { ModportPortsDeclaration }
    -- modport_clocking_declaration :: { ModportClockingDeclaration }
    -- modport_simple_ports_declaration :: { ModportSimplePortsDeclaration }
    -- modport_simple_port :: { ModportSimplePort }
    -- modport_tf_ports_declaration :: { ModportTfPortsDeclaration }
    -- modport_tf_port :: { ModportTfPort }
    -- import_export :: { ImportExport }

    ---- 2.10 - Assertion Declarations ----
    -- concurrent_assertion_item :: { ConcurrentAssertionItem }
    -- concurrent_assertion_statement :: { ConcurrentAssertionStatement }
    -- assert_property_statement :: { AssertPropertyStatement }
    -- assume_property_statement ::{ AssumePropertyStatement }
    -- cover_property_statement :: { CoverPropertyStatement }
    -- expect_property_statement :: { ExpectPropertyStatement }
    -- cover_sequence_statement :: { CoverSequenceStatement }
    -- restrict_property_statement :: { RestrictPropertyStatement }
    -- property_instance :: { PropertyInstance }
    -- property_list_of_arguments :: { PropertyListOfArguments }
    -- property_actual_arg :: { PropertyActualArg }
    -- assertion_item_declaration :: { AssertionItemDeclaration }
    -- property_declaration ::{ PropertyDeclaration }
    -- property_port_item :: { PropertyPortItem }
    -- property_lvar_port_direction :: { PropertyLvarPortDirection }
    -- property_formal_type :: { PropertyFormalType }
    -- property_spec :: { PropertySpec }
    -- property_expr :: { PropertyExpr }
    -- property_case_item :: { PropertyCaseItem }
    -- sequence_declaration :: { SequenceDeclaration }
    -- sequence_port_list :: { SequencePortList }
    -- sequence_port_item :: { SequencePortItem }
    -- sequence_lvar_port_direction :: { SequenceLvarPortDirection }
    -- sequence_formal_type :: { SequenceFormalType }
    -- sequence_expr :: { SequenceExpr }
    -- cycle_delay_range :: { CycleDelayRange }
    -- sequence_method_call :: { SequenceMethodCall }
    -- sequence_match_item :: { SequenceMatchItem }
    -- sequence_instance :: { SequenceInstance }
    -- sequence_list_of_arguments :: { SequenceListOfArguments }
    -- sequence_actual_arg :: { SequenceActualArg }
    -- boolean_abbrev :: { BooleanAbbrev }
    -- sequence_abbrev :: { SequenceAbbrev }
    -- consecutive_repetition :: { ConsecutiveRepetition }
    -- non_consecutive_repetition :: { NonConsecutiveRepetition }
    -- goto_repetition :: { GotoRepetition }
    -- const_or_range_expression :: { ConstOrRangeExpression }
    -- cycle_delay_const_range_expression :: { CycleDelayConstRangeExpression }
    -- expression_or_dist :: { ExpressionOrDist }
    -- assertion_variable_declaration :: { AssertionVariableDeclaration }
    -- let_declaration :: { LetDeclaration }
    -- let_identifier :: { LetIdentifier }
    -- let_port_item :: { LetPortItem }
    -- let_formal_type :: { LetFormalType }
    -- let_list_of_arguments :: { LetListOfArguments }
    -- let_actual_arg :: { LetActualArg }

    ---- 2.11 - Covergroup Declarations ----
    -- covergroup_declaration :: { CovergroupDeclaration }
    -- coverage_spec_or_option :: { CoverageSpecOrOption }
    -- coverage_option :: { CoverageOption }
    -- coverage_spec :: { CoverageSpec }
    -- coverage_event :: { CoverageEvent }
    -- block_event_expression :: { BlockEventExpression }
    -- hierarchical_btf_identifier :: { HierarchicalBtfIdentifier }
    -- cover_point :: { CoverPoint }
    -- bins_or_empty :: { BinsOrEmpty }
    -- bins_or_options :: { BinsOrOptions }
    -- bins_keyword :: { BinsKeyword }
    -- trans_list :: { TransList }
    -- trans_set :: { TransSet }
    -- trans_range_list :: { TransRangeList }
    -- trans_item :: { TransItem }
    -- repeat_range :: { RepeatRange }
    -- cover_cross :: { CoverCross }
    -- cross_item :: { CrossItem }
    -- cross_body :: { CrossBody }
    -- cross_body_item :: { CrossBodyItem }
    -- bins_selection_or_option :: { BinsSelectionOrOption }
    -- bins_selection :: { BinsSelection }
    -- select_expression :: { SelectExpression }
    -- select_condition :: { SelectCondition }
    -- bins_expression :: { BinsExpression }
    -- covergroup_value_range :: { CovergroupValueRange }
    -- with_covergroup_expression :: { WithCovergroupExpression }
    -- set_covergroup_expression :: { SetCovergroupExpression }
    -- integer_covergroup_expression :: { IntegerCovergroupExpression }
    -- cross_set_expression :: { CrossSetExpression }
    -- covergroup_expression :: { CovergroupExpression }

    ---- Sec 3 ----
    ---- 3.1 - Primitive Instantiation And Instances ----
    -- gate_instantiation :: { GateInstantiation }
    -- cmos_switch_instance :: { CmosSwitchInstance }
    -- enable_gate_instance :: { EnableGateInstance }
    -- mos_switch_instance :: { MosSwitchInstance }
    -- n_input_gate_instance :: { NInputGateInstance }
    -- n_output_gate_instance :: { NOutputGateInstance }
    -- pass_switch_instance :: { PassSwitchInstance }
    -- pass_enable_switch_instance :: { PassEnableSwitchInstance }
    -- pull_gate_instance :: { PullGateInstance }

    ---- 3.2 - Primitive Strengths ----
    -- pulldown_strength :: { PulldownStrength }
    -- pullup_strength :: { PullupStrength }

    ---- 3.3 - Primitive Terminals ----
    -- enable_terminal :: { EnableTerminal }
    -- inout_terminal :: { InoutTerminal }
    -- input_terminal :: { InputTerminal }
    -- nocontrol_terminal :: { NocontrolTerminal }
    -- output_terminal :: { OutputTerminal }
    -- pcontrol_terminal :: { PcontrolTerminal }

    ---- 3.4 - Primitive Gate And Switch Types ----
    -- cmos_switchtype :: { CmosSwitchtype }
    -- enable_gatetype :: { EnableGatetype }
    -- mos_switchtype :: { MosSwitchtype }
    -- n_input_gatetype :: { NInputGatetype }
    -- n_output_gatetype :: { NOutputGatetype }
    -- pass_en_switchtype :: { PassEnSwitchtype }
    -- pass_switchtype :: { PassSwitchtype }

    ---- Sec 4 ----
    ---- 4.1.1 - Module Instantiation ----
    -- module_instantiation :: { ModuleInstantiation }
    module_instantiation <- rule $
        Ast.ModuleInstantiation
        <$> module_identifier
        <*> optional parameter_value_assignment
        <*> ((:) <$> hierarchical_instance <*> many (comma *> hierarchical_instance))
        <* semicolon

    parameter_value_assignment <- rule $
        Ast.ParameterValueAssignment
        <$> (hashtag *> ob *> optional parameter_assignments)
        <* cb
        <?> "parameter_value_assignment"

    parameter_assignments <- rule $
        Ast.PAOrdered <$> ((:) <$> ordered_parameter_assignment <*> many (comma *> ordered_parameter_assignment))
        <|> Ast.PANamed <$> ((:) <$> named_parameter_assignment <*> many (comma *> named_parameter_assignment))
        <?> "parameter_assignments"
    ordered_parameter_assignment <- rule $
        Ast.OrderedParameterAssignment <$> param_expression <?> "ordered_parameter_assignment"

    named_parameter_assignment <- rule $
        Ast.NamedParameterAssignment
        <$> (fullstop *> parameter_identifier)
        <* ob
        <*> optional param_expression
        <* cb
        <?> "named_parameter_assignment"
    -- hierarchical_instance :: { HierarchicalInstance }
    hierarchical_instance <- rule $
        Ast.HierarchicalInstance
        <$> name_of_instance
        <*> (ob *> fmap Just port_connections <* cb)
        <?> "hierarchical_instance"

    name_of_instance <- rule $
        Ast.NameOfInstance
        <$> instance_identifier
        <*> many unpacked_dimension
        <?> "name_of_instance"

    -- port_connections :: { PortConnections }
    port_connections <- rule $
        Ast.PCOrdered
        <$> ((:) <$> ordered_port_connection <*> many (comma *> ordered_port_connection))
        <|> Ast.PCNamed
        <$> ((:) <$> named_port_connection <*> many (comma *> named_port_connection))
        <?> "port_connections"

    ordered_port_connection <- rule $
        Ast.OrderedPortConnection
        <$> many attribute_instance
        <*> optional expression
    named_port_connection <- rule $
        Ast.NPC
        <$> many attribute_instance <* fullstop <*> port_identifier
        <*> optional (ob *> expression <* cb)
        <|> Ast.NPCAsterisk
        <$> many attribute_instance <* fullstop <* asterisk
        <?> "named_port_connection"
    ---- 4.1.2 - Interface Instantiation ----
    -- interface_instantiation :: { InterfaceInstantiation }

    ---- 4.1.3 - Program Instantiation ----
    -- program_instantiation :: { ProgramInstantiation }

    ---- 4.1.4 - Checker Instantiation ----
    -- checker_instantiation :: { CheckerInstantiation }
    -- list_of_checker_port_connections :: { CheckerPortConnections }
    -- ordered_checker_port_connection :: { OrderedCheckerPortConnection }
    -- named_checker_port_connection :: { NamedCheckerPortConnection }

    ---- 4.2 - Generated Instantiation ----
    -- generate_region :: { GenerateRegion }
    -- loop_generate_construct :: { LoopGenerateConstruct }
    -- genvar_initialisation :: { GenvarInitialisation }
    -- genvar_iteration :: { GenvarIteration }
    -- conditional_generate_construct :: { ConditionalGenerateConstruct }
    -- if_generate_construct :: { IfGenerateConstruct }
    -- case_generate_construct :: { CaseGenerateConstruct }
    -- case_generate_item :: { CaseGenerateItem }
    -- generate_block :: { GenerateBlock }
    -- generate_item :: { GenerateItem }

    ---- Sec 5 ----
    ---- 5.1 - UDP Declaration ----
    -- udp_nonansi_declaration :: { UdpNonansiDeclaration }
    -- udp_ansi_declaration :: { UdpAnsiDeclaration }
    -- udp_declaration :: { UdpDeclaration }

    ---- 5.2 - UDP Ports ----
    -- udp_port_list :: { UdpPortList }
    -- udp_declaration_port_list :: { UdpDeclarationPortList }
    -- udp_port_declaration :: { UdpPortDeclaration }
    -- udp_output_declaration :: { UdpOutputDeclaration }
    -- udp_input_declaration :: { UdpInputDeclaration }
    -- udp_reg_declaration :: { UdpRegDeclaration }

    ---- 5.3 - UDP Body ----
    -- udp_body :: { UdpBody }
    -- combinational_body :: { CombinationalBody }
    -- sequential_body :: { SequentialBody }
    -- init_val :: { InitVal }
    -- sequential_entry :: { SequentialEntry }
    -- seq_input_list :: { SeqInputList }
    -- level_input_list :: { LevelInputList }
    -- edge_input_list :: { EdgeInputList }
    -- edge_indicator:: { EdgeIndicator }
    -- current_state :: { CurrentState }
    -- next_state :: { NextState }
    -- output_symbol :: { OutputSymbol }
    -- level_symbol :: { LevelSymbol }
    -- edge_symbol :: { EdgeSymbol }

    ---- 5.4 - UDP Instantiation ----
    -- udp_instantiation :: { UdpInstantiation }
    -- udp_instance :: { UdpInstance }

    ---- Sec 6 -----
    ---- 6.1 - Continuous Assignment And Net Alias Statements ----
    net_assignments <- rule $ (:) <$> net_assignment <*> many (comma *> net_assignment) <?> "net_assignments"
    -- Incomplete and incorrect (delay3) ignored Production Rule
    continuous_assign <- rule $
        Ast.CANetAssignment
        <$> (assign *> optional drive_strength)
        <*> optional delay3
        <*> net_assignments
        <|> Ast.CAVariableAssignment
        <$> (assign *> optional delay_control)
        <*> variable_assignments
        <?> "continuous_assign"

    variable_assignments <- rule $
        (:) <$> variable_assignment <*> many (comma *> variable_assignment) <?> "variable_assignments"
    -- net_alias :: { NetAlias }
    net_assignment <- rule $
        Ast.NetAssignment <$> net_lvalue <* equal <*> expression <?> "net_assignment"

    ---- 6.2 - Procedural Blocks And Assignments ----
    -- initial_construct :: { InitialConstruct }
    always_construct <- rule $
        Ast.AlwaysConstruct <$> always_keyword <*> statement

    always_keyword <- rule $
        always *> pure Ast.AKAlways
        <|> always_comb *> pure Ast.AKAlwaysComb
        <|> always_latch *> pure Ast.AKAlwaysLatch
        <|> always_ff *> pure Ast.AKAlwaysFf
        <?> "always_keyword"
    -- final_construct :: { FinalConstruct }
    -- | Incomplete production rule
    blocking_assignment <- rule $
        Ast.BAOperator <$> operator_assignment
    operator_assignment <- rule $
        Ast.OperatorAssignment <$> variable_lvalue <*> assignment_operator <*> expression

    assignment_operator <- rule $
        equal *> pure Ast.AOEqual
        <|> plus_equal *> pure Ast.AOPlusEqual
        <|> minus_equal *> pure Ast.AOMinusEqual
        <|> asterisk_equal *> pure Ast.AOAsteriskEqual
        <|> forwardslash_equal*> pure Ast.AOForwardslashEqual
        <|> percentage_equal *> pure Ast.AOPercentageEqual
        <|> ampersand_equal *> pure Ast.AOAmpersandEqual
        <|> pipe_equal *> pure Ast.AOPipeEqual
        <|> caret_equal *> pure Ast.AOCaretEqual
        <|> lesser_Lesser_equal *> pure Ast.AOLesserLesserEqual
        <|> greater_greater_equal *> pure Ast.AOGreaterGreaterEqual
        <|> lesser_esser_lesser_equal *> pure Ast.AOLesserLesserLesserEqual
        <|> greater_greater_greater_equal *> pure Ast.AOGreaterGreaterGreaterEqual
        <?> "assignment_opeartor"

    nonblocking_assignment <- rule $
        Ast.NonblockingAssignment
        <$> variable_lvalue
        <* lesser_equal
        <*> expression
        <?> "nonblocking_assignment"
    -- procedural_continuous_assignment :: { ProceduralContinousAssignment }
    -- variable_assignment :: { VariableAssignment }
    variable_assignment <- rule $
        Ast.VariableAssignment <$> variable_lvalue <* equal <*> expression <?> "variable_assignment"

    ---- 6.3 - Parallel And Sequential Blocks ----
    -- action_block :: { ActionBlock }

    seq_block <- rule $
        Ast.SeqBlock
        <$> (begin *> optional (colon *> block_identifier))
        <*> many block_item_declaration
        <*> many statement_or_null
        <* end
        <*> optional(colon *> block_identifier)
        <?> "seq_block"

    -- par_block :: { ParBlock }
    -- join_keyword :: { JoinKeyword }

    ---- 6.4 - Statements ----
    statement_or_null <- rule $
        Left <$> statement
        <|> Right <$> many(attribute_instance) <* semicolon
        <?> "statement_or_null"

    -- | Incomplete production rule
    statement <- rule $
        Ast.Statement <$> optional (block_identifier <* colon) <*> many attribute_instance <*> statement_item
        <?> "statement"
    -- | Incomplete production rule
    statement_item <- rule $
        Ast.SIBlockingAssignment <$> blocking_assignment <* semicolon
        <|> Ast.SINonblockingAssignment <$> nonblocking_assignment <* semicolon
        <|> Ast.SISeqBlock <$> seq_block
        <|> Ast.SIProceduralTimingControlStatement <$> procedural_timing_control_statement
        <|> Ast.SIConditionalStatement <$> conditional_statement
        <?> "statement_item"
    -- function_statement :: { FunctionStatement }
    -- function_statement_or_null :: { FunctionStatementOrNull }

    ---- 6.5 - Timing Control Statements ----
    procedural_timing_control_statement <- rule $
        Ast.ProceduralTimingControlStatement <$> procedural_timing_control <*> statement_or_null
        <?> "procedural_timing_control_statement"

    -- delay_or_event_control :: { DelayOrEventControl }
    delay_control <- rule $
        Ast.DCValue <$> (hashtag *> delay_value)
        <|> Ast.DCMintypmaxExpression <$> (hashtag *> ob *> mintypmax_expression <* cb )
        <?> "delay_control"

    event_control <- rule $
        Ast.ECExpression <$> (at_sign *> ob *> event_expression) <* cb
        <|> at_sign *> ob *> asterisk *> cb *> pure Ast.ECAsterisk
        <?> "event_control"

    -- | Incomplete Expression
    event_expression <- rule $
        Ast.EE <$> optional edge_identifier <*> expression <*> optional (iff *> expression)
        <?> "event_expression"

    procedural_timing_control <- rule $
        Ast.PTCEvent <$> event_control
        <?> "procedural_timing_control"
    -- jump_statement :: { JumpStatement }
    -- wait_statement :: { WaitStatement }
    -- event_trigger :: { EventTrigger }
    -- disable_statement :: { DisableStatement }

-- [ unique_priority ] if ( cond_predicate ) statement_or_null
-- {else if ( cond_predicate ) statement_or_null } [ else statement_or_null ]
    ---- 6.6 - Conditional Statements ----
    conditional_statement <- rule $
                                Ast.ConditionalStatement
                                <$> optional unique_priority
                                <*> (if' *> ob *> cond_predicate <* cb)
                                <*> statement_or_null
                                <*> many ((,) <$> (else' *> if' *> ob *> cond_predicate <* cb) <*> statement_or_null)
                                <*> optional (else' *> statement_or_null)
    -- unique_priority :: { UniquePriority }
    unique_priority <- rule $
                            (unique *> pure Ast.UPUnique)
                            <|> (unique0 *> pure Ast.UPUnique0)
                            <|> priority *> pure Ast.UPPriority
                            <?> "unique_priority"
    -- cond_predicate :: { CondPredicate }
    cond_predicate <- rule $
                        (:)
                        <$> expression_or_cond_pattern
                        <*> many (ampersand_ampersand_ampersand *> expression_or_cond_pattern)
    -- expression_or_cond_pattern :: { ExpressionOrCondPattern }
    -- | Incomplete production rule
    expression_or_cond_pattern <- rule $
                                    Left <$> expression <?> "expression_or_cond_pattern"
                                    -- <|> Right cond_pattern <?> "expression_or_cond_pattern"
    -- cond_pattern :: { CondPattern }
    ---- 6.7 - Case Statements ----
    -- case_statement :: { CaseStatement }
    -- case_keyword :: { CaseKeyword }
    -- case_expression :: { CaseExpression }
    -- case_item :: { CaseItem }
    -- case_pattern_item :: { CasePatternItem }
    -- case_inside_item :: { CaseInsideItem }
    -- case_item_expression :: { CaseItemExpression }
    -- randcase_statement :: { RandcaseStatement }
    -- randcase_item :: { RandcaseItem }
    -- open_range_list :: { OpenRangeList }
    -- open_value_range ::= { OpenValueRange }

    ---- 6.7.1 - Patterns ----
    -- pattern :: { Pattern }
    -- assignment_pattern :: { AssignmentPattern }
    -- structure_pattern_key :: { StructurePatternKey }
    -- array_pattern_key :: { ArrayPatternKey }
    -- assignment_pattern_key :: { AssignmentPatternKey }
    -- assignment_pattern_expression :: { AssignmentPatternExpression }

    assignment_pattern_expression_type <- rule $
        Ast.APETPsTypeIdentifier <$> ps_type_identifier
        <|> Ast.APETPsParameterIdentifier <$> ps_parameter_identifier
        <|> Ast.APETIntegerAtomType <$> integer_atom_type
        <|> Ast.APETTypeReference <$> type_reference
        <?> "assignment_pattern_expression_type"

    -- constant_assignment_pattern_expression :: { ConstantAssignmentPatternExpression }
    -- assignment_pattern_net_lvalue :: { AssignmentPatternNetLvalue }
    -- assignment_pattern_variable_lvalue :: { AssignmentPatternVariableLvalue }

    ---- 6.8 - Looping Statements ----
    -- loop_statement :: { LoopStatement }
    -- for_initialisation :: { ForInitialisation }
    -- for_variable_declaration :: { ForVariableDeclaration }
    -- for_step :: { ForStep }
    -- for_step_assignment :: { ForStepAssignment }
    -- loop_variables :: { LoopVariables }

    ---- 6.9 - Subroutine Call Statements ----
    -- subroutine_call_statement :: { SubroutineCallStatement}

    ---- 6.10 - Assertion Statements ----
    -- assertion_item :: { AssertionItem }
    -- deferred_immediate_assertion_item :: { DeferredImmediateAssertionItem }
    -- procedural_assertion_statement :: { ProceduralAssertionStatement }
    -- immediate_assertion_statement :: { ImmediateAssertionStatement }
    -- simple_immediate_assertion_statement :: { SimpleImmediateAssertionStatement }
    -- simple_immediate_assert_statement :: { SimpleImmediateAssertStatement }
    -- simple_immediate_assume_statement :: { SimpleImmediateAssumeStatement }
    -- simple_immediate_cover_statement :: { SimpleImmediateCoverStatement }
    -- deferred_immediate_assertion_statement :: { DeferredImmediateAssertionStatement }
    -- deferred_immediate_assert_statement :: { DeferredImmediateAssertStatement }
    -- deferred_immediate_assume_statement :: { DeferredImmediateAssumeStatement }
    -- deferred_immediate_cover_statement :: { DeferredImmediateCoverStatement }

    ---- 6.11 - Clocking Block ----
    -- clocking_declaration :: { ClockingDeclaration }
    -- clocking_event :: { ClockingEvent }
    -- clocking_item :: { ClockingItem }
    -- default_skew :: { DefaultSkew }
    -- clocking_direction :: { ClockingDirection }
    -- clocking_decl_assign :: { ClockingDeclAssign }
    -- clocking_skew :: { ClockingSkew }
    -- clocking_drive :: { ClockingDrive }
    -- cycle_delay :: { CycleDelay }
    -- clockvar :: { Clockvar }
    -- clockvar_expression :: { ClockvarExpression }

    ---- 6.12 - Randsequence ----
    -- randsequence_statement :: { RandsequenceStatement }
    -- production :: { Production }
    -- rs_rule :: { RsRule }
    -- rs_production_list :: { RsProductionList }
    -- weight_specification :: { WeightSpecification }
    -- rs_code_block :: { RsCodeBlock }
    -- rs_prod :: { RsProd }
    -- production_item :: { ProductionItem }
    -- rs_if_else :: { RsIfElse }
    -- rs_repeat :: { RsRepeat }
    -- rs_case :: { RsCase }
    -- rs_case_item :: { RsCaseItem }

    ---- Sec 7 ----
    ---- 7.1 - Specify Block Declaration ----
    -- specify_block :: { SpecifyBlock }
    -- specify_item :: { SpecifyItem }
    -- pulsestyle_declaration :: { PulsestyleDeclaration }
    -- showcancelled_declaration :: { ShowcancelledDeclaration }

    ---- 7.2 - Specify Path Declarations ----
    -- path_declaration :: { PathDeclaration }
    -- simple_path_declaration :: { SimplePathDeclaration }
    -- parallel_path_description :: { ParallelPathDescription }
    -- full_path_description :: { FullPathDescription }

    ---- 7.3 - Specify Block Terminals ----
    -- specify_input_terminal_descriptor :: { SpecifyInputTerminalDescriptor }
    -- specify_output_terminal_descriptor :: { SpecifyOutputTerminalDescriptor }
    -- input_identifier :: { InputIdentifier }
    -- output_identifier :: { OutputIdentifier }

    ---- 7.4 - Specify Path Delays ----
    -- path_delay_value :: { PathDelayValue }
    -- list_of_path_delay_expressions :: { PathDelayExpressions }
    -- t_path_delay_expression :: { TPathDelayExpression }
    -- trise_path_delay_expression :: { TrisePathDelayExpression }
    -- tfall_path_delay_expression :: { TfallPathDelayExpression }
    -- tz_path_delay_expression :: { TzPathDelayExpression}
    -- t01_path_delay_expression :: { T01PathDelayExpression }
    -- t10_path_delay_expression :: { T10PathDelayExpression }
    -- t0z_path_delay_expression :: { Tx0PathDelayExpression }
    -- tz1_path_delay_expression :: { T1xPathDelayExpression }
    -- t1z_path_delay_expression :: { Tx1PathDelayExpression }
    -- tz0_path_delay_expression :: { T0xPathDelayExpression }
    -- t0x_path_delay_expression :: { Tz0PathDelayExpression }
    -- tx1_path_delay_expression :: { T1zPathDelayExpression }
    -- t1x_path_delay_expression :: { Tz1PathDelayExpression }
    -- tx0_path_delay_expression :: { T0zPathDelayExpression }
    -- txz_path_delay_expression :: { T10PathDelayExpression }
    -- tzx_path_delay_expression :: { T01PathDelayExpression }
    -- path_delay_expression :: { PathDelayExpression }
    -- edge_sensitive_path_declaration :: { EdgeSensitivePathDeclaration }
    -- parallel_edge_sensitive_path_description :: { ParallelEdgeSensitivePathDescription }
    -- full_edge_sensitive_path_description :: { FullEdgeSensitivePathDescription }
    -- data_source_expression :: { DataSourceExpression }

    edge_identifier <- rule $
        posedge *> pure Ast.EIPosedge
        <|> negedge *> pure Ast.EINegedge
        <|> edge *> pure Ast.EIEdge
        <?> "edge_identifier"

    -- state_dependent_path_declaration :: { StateDependentPathDeclaration }
    -- polarity_operator :: { PolarityOperator }

    ---- 7.5 - System Timing Checks ----
    -- system_timing_check :: { SystemTimingCheck }
    -- dollar_setup_timing_check :: { DollarSetupTimingCheck }
    -- dollar_hold_timing_check :: { DollarHoldTimingCheck}
    -- dollar_setuphold_timing_check :: { DollarSetupholdTimingCheck }
    -- dollar_recovery_timing_check :: { DollarRecoveryTimingCheck }
    -- dollar_removal_timing_check :: { DollarRemovalTimingCheck }
    -- dollar_recrem_timing_check :: { DollarRecremTimingCheck }
    -- dollar_skew_timing_check :: { DollarSkewTimingCheck }
    -- dollar_timeskew_timing_check :: { DollarTimeskewTimingCheck }
    -- dollar_fullskew_timing_check :: { DollarFullskewTimingCheck }
    -- dollar_period_timing_check :: { DollarPeriodTimingCheck }
    -- dollar_width_timing_check :: { DollarWidthTimingCheck }
    -- dollar_nochange_timing_check :: { DollarNochangeTimingCheck }

    ---- 7.5.2 - System Timing Check Command Arguments ----
    -- timecheck_condition :: { TimecheckCondition }
    -- controlled_reference_event :: { ControlledReferenceEvent }
    -- data_event :: { DataEvent }
    -- delayed_data :: { DelayedData }
    -- delayed_reference :: { DelayedReference }
    -- end_edge_offset :: { EndEdgeOffset }
    -- event_based_flag :: { EventBasedFlag }
    -- notifier :: { Notifier }
    -- reference_event :: { ReferenceEvent}
    -- remain_active_flag :: { RemainActiveFlag }
    -- timestamp_condition :: { TimestampCondition }
    -- start_edge_offset :: { StartEdgeOffset }
    -- threshold :: { Threshold }
    -- timing_check_limit :: { TimingCheckLimit }

    ---- 7.5.3 - System Timing Check Event Definitions ----
    -- timing_check_event :: { TimingCheckEvent }
    -- controlled_timing_check_event :: { ControlledTimingCheckEvent }
    -- timing_check_event_control :: { TimingCheckEventControl }
    -- specify_terminal_descriptor :: { SpecifyTerminalDescriptor }
    -- edge_control_specifier :: { EdgeControlSpecifier }
    -- edge_descriptor :: { EdgeDescriptor }
    -- zero_or_one :: { ZeroOrOne }
    -- z_or_x :: { ZOrX}
    -- timing_check_condition :: { TimingCheckCondition }
    -- scalar_timing_check_condition :: { ScalarTimingCheckCondition }
    -- scalar_constant :: { ScalarConstant }

    ---- Sec 8 ----
    ---- 8.1 - Concatenations ----
    -- concatenation :: { Concatenation }
    -- constant_concatenation :: { ConstantConcatenation }
    -- constant_multiple_concatenation :: { ConstantMultipleConcatenation }
    -- module_path_concatenation ::<- rule $ { ModulePathConcatenation }
    -- module_path_multiple_concatenation :: { ModulePathMultipleConcatenation }
    -- multiple_concatenation :: { MultipleConcatenation }

    streaming_concatenation <- rule $
        Ast.StreamingConcatenation <$> (ocb *> stream_operator) <*> optional slice_size <*> stream_concatenation <* ccb
        <?> "streaming_concatenation"
    stream_operator <- rule $
        greater_greater *> pure Ast.SOGreater
        <|> lesser_lesser *> pure Ast.SOLesser

    slice_size <- rule $
        Ast.SSSimpleType <$> simple_type
        <|> Ast.SSConstantExpression <$> constant_expression
        <?> "slice_size"

    stream_concatenation <- rule $
        Ast.StreamConcatenation
        <$> ((:) <$> (ocb *> stream_expression) <*> many (comma *> stream_expression) <* ccb)
        <?> "stream_concatenation"

    stream_expression <- rule $
        Ast.StreamExpression
        <$> expression
        <*> optional (with *> osb *> array_range_expression <* csb)
        <?> "stream_expression"

    array_range_expression <- rule $
        Ast.ARE <$> expression
        <|> Ast.AREColon <$> expression <* colon <*> expression
        <|> Ast.AREPlusColon <$> expression <* plus_colon <*> expression
        <|> Ast.AREMinusColon <$> expression <* minus_colon <*> expression
        <?> "array_range_expression"

    -- empty_queue :: { EmptyQueue }

    ---- 8.2 - Subroutine Calls ---
    -- constant_function_call :: { ConstantFunctionCall }
    -- tf_call :: { TfCall }
    -- system_tf_call :: { SystemTfCall }
    -- subroutine_call :: { SubroutineCall }
    -- function_subroutine_call :: { FunctionSubroutineCall }
    -- arguments :: { Arguments }
    -- method_call :: { MethodCall }
    -- method_call_body :: { MethodCallBody }
    -- built_in_method_call :: { BuiltInMethodCall }
    -- array_manipulation_call :: { ArrayManipulationCall }
    -- randomize_call :: { RandomizeCall }
    -- method_call_root :: { MethodCallRoot }
    -- array_method_name :: { ArrayMethodName }

    ---- 8.3 - Expressions ----
    -- inc_or_dec_expression :: { IncOrDecExpression }
    -- conditional_expression :: { ConditionalExpression }
    constant_expression <- rule $ Ast.CEPrimary <$> constant_primary <?> "constant_expression"

    constant_mintypmax_expression <- rule $
        Ast.CMESingle <$> constant_expression
        <|> Ast.CMETriple <$> constant_expression <* colon <*> constant_expression <* colon <*> constant_expression
        <?> "constant_mintypmax_expression"

    constant_param_expression <- rule $
        Ast.CPEMintypmax <$> constant_mintypmax_expression
        <|> Ast.CPEDataType <$> data_type
        <|> dollar *> pure Ast.CPEDollar
        <?> "constant_param_expression"

    param_expression <- rule $
        Ast.PEMintypmax <$> mintypmax_expression
        <|> Ast.PEDataType <$> data_type
        <|> dollar *> pure Ast.PEDollar
        <?> "param_expression"

    -- constant_range_expression :: { ConstantRangeExpression }
    -- constant_part_select_range :: { ConstantPartSelectRange }

    -- | Incomplete production rule
    constant_range <- rule $ Ast.CRExpression <$> constant_expression <* colon <*> constant_expression
    -- constant_indexed_range ::{ ConstantIndexedRange }
    -- | Incorrect production rule
    expression <- rule $ Ast.EPrimary <$> primary <?> "expression"
    -- tagged_union_expression :: { TaggedUnionExpression}
    -- inside_expression :: { InsideExpression }
    -- value_range :: { ValueRange }

    mintypmax_expression <- rule $
        Ast.MESingle <$> expression
        <|> Ast.METripple <$> expression <* colon <*> expression <* colon <*> expression
        <?> "mintypmax_expression"
    -- module_path_conditional_expression :: { ModulePathConditionalExpression }
    -- module_path_expression :: { ModulePathExpression }
    -- module_path_mintypmax_expression :: { ModulePathMintypmaxExpression}
    part_select_range <- rule $ Ast.PSRConstant <$> constant_range <?> "part_select_range"

    indexed_range <- rule $
        Ast.IRPlusColon <$> expression <* plus_colon <*> constant_expression
        <|> Ast.IRMinusColon <$> expression <* minus_colon <*> constant_expression
        <?> "indexed_range"
    -- genvar_expression :: { GenvarExpression}

    ---- 8.4 - Primaries ----
    constant_primary <- rule $ Ast.CPLiteral <$> primary_literal <?> "constant_primary"
    -- module_path_primary :: { ModulePathPrimary }
    primary <- rule $   Ast.PHierarchicalIdentifier <$> hierarchical_identifier <*> optional select
                        <|> Ast.PLiteral <$> primary_literal <?> "primary"

    -- class_qualifier :: { ClassQualifier}
    -- range_expression :: { RangeExpression }
    primary_literal <- rule $ Ast.PLNumber <$> number <?> "primary_literal"

    time_literal <- rule $
        Ast.TLUnsigned <$> unsigned_number <*> time_unit
        <|> Ast.TLFixedPoint <$> unsigned_number <* fullstop <*> unsigned_number <*> time_unit
        <?> "time_literal"
    time_unit <- rule $ s *> (pure Ast.TUSecond)
                <|> ms *> pure Ast.TUMillisecond
                <|> us *> pure Ast.TUMicrosecond
                <|> ns *> pure Ast.TUNanosecond
                <|> ps  *> pure Ast.TUPicosecond
                <|> fs *> pure Ast.TUFemtosecond
                <?> "timeunit"

    -- time_unit :: { TimeUnit }
    implicit_class_handle <- rule $
        this *> fullstop *> pure Ast.ICHThis
        <|> super *> pure Ast.ICHSuper
        <|> this *> fullstop *> super *> pure Ast.ICHThisSuper

    bit_select <- rule $ Ast.BitSelect
                <$> many(osb *> expression <* csb) <?> "bit_select"

    select <- rule $ Ast.Select
                <$> optional( (,) <$> many((,) <$> (fullstop *> member_identifier) <*> bit_select) <*> (fullstop *> member_identifier))
                <*> bit_select
                <*> fmap Just ( osb *> part_select_range <* csb)
                <|> Ast.Select
                <$> fmap Just ((,) <$> many((,) <$> (fullstop *> member_identifier) <*> bit_select) <*> (fullstop *> member_identifier))
                <*> bit_select
                <*> optional ( osb *> part_select_range <* csb)
                <|> Ast.Select
                <$> optional( (,) <$> many((,) <$> (fullstop *> member_identifier) <*> bit_select) <*> (fullstop *> member_identifier))
                <*> (Ast.BitSelect <$> ((:) <$> (osb *> expression <* csb) <*> many(osb *> expression <* csb)))
                <*> optional ( osb *> part_select_range <* csb)
                <?> "select"

    -- nonrange_select :: { NonrangeSelect }
    constant_bit_select <- rule $ Ast.ConstantBitSelect <$> many( osb *> constant_expression <* csb) <?> "constant_bit_select"

    -- Incorrect production rule
    constant_select <- rule $
        osb *> csb *> pure Ast.ConstantSelect <?> "constant_select"
    -- constant_cast :: { ConstantCast }
    -- constant_let_expression :: { ConstantLetExpression }
    -- cast :: { Cast }

    ---- 8.5 - Expression Left-Side Values ----
    -- Incomplete production rule
    net_lvalue <- rule $
        Ast.NetLValue <$> ps_or_hierarchical_net_identifier <*> constant_select <?> "net_lvalue"

    variable_lvalue <- rule $
        Ast.VLHierarchical
        <$> optional implicit_class_handle_or_package_scope
        <*> hierarchical_variable_identifier
        <*> optional select <?> "variable_lvalue"

    implicit_class_handle_or_package_scope <- rule $
        Left <$> implicit_class_handle
        <|> Right <$> package_scope
        <?> "implicit_class_handle_or_package_scope"
    -- nonrange_variable_lvalue :: { NonrangeVariableLvalue }

    ---- 8.6 - Operators ----
    ---- 8.7 - Numbers ----

    -- Incomplete production rule
    number <- rule $ Ast.NIntegral <$> integral_number <?> "integral_number"

    -- Incomplete production rule
    integral_number <- rule $ Ast.INDecimal <$> decimal_number <?> "integral_number"

    decimal_number <- rule $ Ast.DNUnsigned <$> unsigned_number <?> "decimal_number"

    ---- 8.8 - Strings ----

    ---- Sec 9 ----
    ---- 9.1 - Attributes ----
    attribute_instance <- rule $
        Ast.AttributeInstance
        <$> (ob *> asterisk *> ((:) <$> attr_spec <*> many (comma *> attr_spec)))
        <* asterisk
        <* cb
        <?> "attribute_instance"
    attr_spec <- rule $
        Ast.AttrSpec <$> attr_name <*> optional (equal *> constant_expression) <?> "attr_spec"
    attr_name <- rule $
        Ast.AttrName <$> identifier <?> "attr_name"
    ---- 9.2 - Comments ----
    -- comment :: { Comment }
    -- one_line_comment :: { OneLineComment }
    -- block_comment :: { BlockComment }
    -- comment_text :: { CommonText }

    ---- 9.3 - Identifiers ----
    block_identifier <- rule $
        Ast.BlockIdentifier <$> identifier <?> "block_identifier"

    class_identifier <- rule $
        Ast.ClassIdentifier <$> identifier <?> "class_identifier"

    hierarchical_identifier <- rule $
        Ast.HierarchicalIdentifier
        <$> pure False
        <*> many ((,) <$> identifier <*> constant_bit_select <* fullstop)
        <*> identifier
        <?> "hierarchical_identifier"


    identifier_constant_bit_select <- rule $
        (,) <$> identifier <*> constant_bit_select <* fullstop <?> "identifier_constant_bit_select"

    hierarchical_variable_identifier <- rule $ Ast.HierarchicalVariableIdentifier <$> hierarchical_identifier <?> "hierarchical_variable_identifier"

    interface_identifier <- rule $
        Ast.InterfaceIdentifier <$> identifier <?> "interface_identifier"

    parameter_identifier <- rule $
        Ast.ParameterIdentifier <$> identifier <?> "parameter_identifier"
    module_identifier <- rule $
        Ast.ModuleIdentifier <$> identifier <?> "module_identifier"
    -- Incomplete production rule
    package_scope <- rule $
        Ast.PSIdentifier <$> package_identifier <* colon_colon <?> "package_scope"
    modport_identifier <- rule $
        Ast.ModportIdentifier <$> identifier <?> "modport_identifier"
    package_identifier <- rule $
        Ast.PackageIdentifier <$> identifier <?> "package_identifier"
    port_identifier <- rule $
        Ast.PortIdentifier <$> identifier <?> "port_identifier"
    ps_class_identifier <- rule $
        Ast.PsClassIdentifier <$> optional package_scope <*> class_identifier <?> "ps_class_identifier"

    ps_or_hierarchical_net_identifier <- rule $
        Ast.POHNINet <$> optional package_scope <*> net_identifier <?> "ps_or_hierarchical_net_identifier"

    -- Incomplete production rule
    ps_parameter_identifier <- rule $
        Ast.PPIScoped <$> optional (eitherProd package_scope class_scope) <*> parameter_identifier <?> "ps_parameter_identifier"

    ps_type_identifier <- rule $
        Ast.PsTypeIdentifier <$> optional local_or_package_scope <*> type_identifier
        <?> "ps_type_identifier"

    local_or_package_scope <- rule $
        local_colon_colon *> pure Ast.LOPSLocal
        <|> Ast.LOPSPackageScope <$> package_scope
        <?> "local_or_package_scope"

    member_identifier <- rule $ Ast.MemberIdentifier <$> identifier <?> "identifier"
    net_identifier <- rule $ Ast.NetIdentifier <$> identifier <?> "net_identifier"
    type_identifier <- rule $ Ast.TypeIdentifier <$> identifier <?> "type_identifier"
    variable_identifier <- rule $ Ast.VariableIdentifier <$> identifier <?> "variable_identifier"
    instance_identifier <- rule $ Ast.InstanceIdentifier <$> identifier <?> "instance_identifier"
    return (source_text <* eof)