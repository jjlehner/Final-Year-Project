module V2H.Parser where

isPresent = fmap isJust optional
either a b =
    Left <$> a
    <|> Right <$> b

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


----- Sec 1 -----
---- 1.1 - Library Source Text ----
---- 1.2 - SystemVerilog Source Text ----
source_text = AST.SourceText <$> optional timeunits_declaration <*> many(description) <?> "source_text"
description = AST.DModuleDeclaration <$> module_declaration <?> "description"
-- module_nonansi_header :: { ModuleNonansiHeader }

-- Incomplete production rule
module_ansi_header = AST.ModuleAnsiHeader
                        <$> many attribute_instance
                        <*> module_keyword
                        <*> optional lifetime
                        <*> module_identifier
                        <*> many package_import_declaration
                        <*> optional parameter_ports
                        <*> optional port_declarations <* semicolon

module_declaration = AST.MDAnsiHeader
                        <$> module_ansi_header
                        <*> optional timeunits_declaration
                        <*> many non_port_module_item
                        <*> endmodule
                        <*> optional (colon *> module_identifier) <?> "module_declaration"

module_keyword =    module' *> (pure AST.MKModule)
                    <|> (macromodule *> pure AST.MKMacromodule)
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
timeunits_declaration = timeunit
                        *> time_literal
                        *> optional (forward_slash *> time_literal)
                        *> pure AST.TimeunitsDeclaration
                        *> semicolon
                        *> pure TimeunitsDeclaration

---- 1.3 - Module parameters and Ports ----
-- | Referred to as parameter_port_list in Standard
parameter_ports =   AST.ParameterPorts
                    <$> hashtag *> ob *> param_assignments
                    <*> many (comma *> parameter_port_declaration) <* cb
                    <|> AST.ParameterPorts []
                    <$> hashtag *> ob *> ((:) <$> parameter_port_declaration <*> many (comma *> parameter_port_declaration)) <* cb
                    <|> hashtag *> ob *> cb *> pure (AST.ParameterPorts [] [])
                    <?> "parameter_ports"

parameter_port_declaration =
    PPD <$> parameter_declaration
    <|> PPDLocal <$> local_parameter_declaration
    <|> PPDDataTypeParamAssignments <$> data_type <*> param_assignments
    <|> PPDTypeAssignments <$> type *> type_assignments
    <?> "parameter_port_declaration"

port_declarations =
    ob *> cb *> pure []
    <|> (:) <$> ob port_declarations_item <*> many(comma *> port_declarations_item)
    <?> "port_declarations"

port_declarations_item =
    PortDeclarationsItem <$> many attribute_instance <*> ansi_port_declaration <?> "port_declarations_item"
-- port_declaration :: { PortDeclaration }
-- port :: { Port }
-- port_expression :: { PortExpression }
-- port_reference :: { PortReference }

port_direction =
    input *> pure PDInput
    <|> output *> pure PDOutput
    <|> inout *> pure PDInout
    <|> ref *> pure PDRef
    <?> "port_direction"

net_port_header = NetPortHeader <$> optional port_direction <*> net_port_type <?> "net_port_header"
variable_port_header = VariablePortHeader <$> optional port_direction <*> variable_port_type <?> "variabler_port_header"
interface_port_header = IPDNamed <$> interface_identifier <*> optional (fullstop *> modport_identifier)
                        <|> IPDAnonymous <$> interface <*> optional (fullstop *> modport_identifier) <?> "interface_port_header"

-- Incomplete Production Rule
ansi_port_declaration =
    APDNetOrInterfaceHeader <$> optional net_or_interface_port_header
    <*> port_identifier
    <*> unpacked_dimension
    <*> optional $ equal *> constant_expression
    <|> APDVariableHeader <$> optional variable_port_header
    <*> port_identifier
    <*> many variable_dimension
    <*> optional $ equal *> constant_expression
    <?> "ansi_port_declaration"

net_or_interface_port_header =
    Left <$> net_port
    <|> Right <$> interface_port_header
    <?> "net_or_interface_port_header"

---- 1.4 - Module Items ----
-- elaboration_system_task :: { ElaborationSystemTask }
-- finish_number :: { FinishNumber }

-- Incomplete Production Rule
module_common_item =
    MCIModuleOrGenerateItemDeclaration <$> module_or_generate_item_declaration
    <|> MCIContinuousAssign <$> continuous_assign
    <|> MCIAlwaysConstruct <$> always_construct
    <?> "module_common_item"
-- module_item :: { ModuleItem }
-- Incomplete Production Rule
module_or_generate_item =
    MOGIModuleCommonItem <$> many attribute_instance <*> module_common_item <?> "module_or_generate_item"

-- Incomplete Production Rule
module_or_generate_item_declaration =
    MOGIDPackageOrGenerateItemDeclaration <$> package_or_generate_item_declaration <?> "module_or_generate_item_declaration"

-- Incomplete Production Rule
non_port_module_item =
    NPMIModuleOrGenerateItem <$> module_or_generate_item <?> "non_port_module_item"
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
package_or_generate_item_declaration =
    POGIDData <$> data_declaration <?> "package_or_generate_item_declaration"
-- anonymous_program :: { AnonymousProgram }
-- anonymous_program_item :: { AnonymousProgramItem }

----- Sec 2 -----
---- 2.1.1 - Module Parameter Declarations ----
local_parameter_declaration =
    LPDParamAssignments <$> localparam *> data_type_or_implicit param_assignments
    <|> LPDTypeAssignments <$> localparam *> type *> type_assignments
    <?> "local_parameter_declaration"

parameter_declaration =
    PDDataTypeOrImplicit <$> parameter *> data_type_or_implicit *> param_assignments
    <|> PDTypeAssignments <$> parameter *> type *> type_assignments
    <?> "parameter_declaration"
-- specparam_declaration :: { SpecparamDeclaration }

---- 2.1.2 - Port Declarations ----
-- inout_declaration :: { InoutDeclaration }
-- input_declaration :: { InputDeclaration }
-- output_declaration :: { OutputDeclaration }
-- interface_port_declaration :: { InterfacePortDeclaration }
-- ref_declaration :: { RefDeclaration }

---- 2.1.3 - Type Declarations ----
data_declaration =
    DD <$> isPresent const
    <*> isPresent var
    <*> optional lifetime
    <*> data_type_or_implicit
    <*> variable_decl_assignments
    <* semicolon
    <?> "data_declaration"

package_import_declaration =
    PackageImportDeclaration <$> import' *> package_import <*> many (comma *> package_import_item) <?> "package_import_declaration"

package_import_item =
    PIIIdentifier <$> package_identifier <* colon_colon <*> identifier
    <|> PIIWildcard <$> package_identifier <* colon_colon <*> asterisk
    <?> "package_import_item"

-- package_export_declaration :: { PackageExportDeclaration }
-- genvar_declaration :: { GenvarDeclaration }
-- net_declaration :: { NetDeclaration }
-- type_declaration :: { TypeDeclaration }
-- net_type_declaration :: { NetTypeDeclaration }
lifetime =
    automatic *> pure automatic
    <|> static *> pure static
    <?> "lifetime"

---- 2.2.1 - Net and Variable Types ----
-- casting_type :: { CastingType }
-- data_type :: { DataType }

-- | Incomplete Production Rule
data_type =
    DTIntegerVector <$> integer_vector_type <*> optional signing <*> many packed_dimension <?> "data_type"

data_type_or_implicit =
    Left <$> data_type
    <|> Right <$> implicit_data_type
    <?> "data_type_or_implicit"
implicit_data_type =
    ImplicitDataType <$> optional signing <*> many packed_dimension <?> "implicit_data_type"
-- enum_base_type :: { EnumBaseType }
-- enum_name_declaration :: { EnumNameDeclaration }
class_scope =
    ClassScope <$> class_type <* colon_colon
class_type =
    ClassType <$> ps_class_identifier <*> optional parameter_value_assignment <*> many class_identifier_parameter_value_assignment <?> "class_type"

class_identifier_parameter_value_assignment =
   ClassIdentifierParameterValueAssignment
   <$>  colon_colon
   *> class_identifier
   <*> optional parameter_value_assignment
   <?> "class_identifier_parameter_value_assignment"

integer_type =
    ITVector <$> integer_vector_type
    <|> ITAtom <$> integer_atom_type
    <?> "integer_type"

integer_atom_type =
    byte *> pure IATByte
    <*> shortint *> pure IATShortint
    <*> int *> pure IATInt
    <*> longint *> pure IATLongint
    <*> integer *> pure IATInteger
    <*> time *> pure IATTime
    <?> "integer_atom_type"

integer_vector_type =
    bit *> pure IVTBit
    <|> logic *> IVTLogic
    <|> reg *> IVTReg
    <?> "integer_vector_type"

non_integer_type =
    shortreal *> pure NITShortreal
    <|> real *> pure NITReal
    <|> realtime *> pure NITRealtime
    <?> "non_integer_type"

net_type =
    <|> supply0 *> pure Supply0
    <|> supply1 *> pure Supply1
    <|> tri     *> pure Tri
    <|> triand  *> pure Triand
    <|> trior   *> pure Trior
    <|> trireg  *> pure Trireg
    <|> tri0    *> pure Tri0
    <|> tri1    *> pure Tri1
    <|> uwire   *> pure Uwire
    <|> wire    *> pure Wire
    <|> wand    *> pure Wand
    <|> wor     *> pure Wor
    <?> "net_type"

net_port_type =
    NPTDataOrImplicit
    <$> optional net_type
    <*> data_type_or_implicit
    <?> "net_port_type"

variable_port_type =
    VariablePortType <$> var_data_type <?> "variable_port_type"

var_data_type =
    VDT <$> data_type
    <|> VDTOrImplicit <$> var *> data_type_or_implicit
    <?> "var_data_type"

signing =
    signed *> pure SSigned
    <|> unsigned *> pure SUnsigned
    <?> "signing"

simple_type =
    STInteger <$> integer_type
    <|> STNonInteger <$> non_integer_type
    <|> STPsIdentifier <$> ps_type_identifier
    <|> STPsParameterIdentifier <$> ps_parameter_identifier

-- struct_union_member :: { StructUnionMember }
-- data_type_or_void :: { DataTypeOrVoid }
-- struct_union :: { StructUnion }
type_reference =
    TRExpression <$> type' *> ob *> expression <* cb
    <|> TRDataType <$> type' *>  ob *> data_type <* cb
    <?> "type_reference"

---- 2.2.2 - Strengths ----
-- | Incorrect Production Rule
drive_strength =
    osb *> csb *> pure DriveStrength <?> "drive_strength"
-- strength0 :: { Strength0 }
-- strength1 :: { Strength1 }
-- charge_strength :: { ChargeStrength }

---- 2.2.3 - Delays ----
-- delay3 :: { Delay3 }
-- delay2 :: { Delay2 }
-- delay_value :: { DelayValue }

---- 2.3 - Declarations Lists ----
param_assignments :: { [ParamAssignment] }
    : param_assignment many(snd(',',param_assignment)) { $1:$2 }
type_assignments :: { [TypeAssignment] }
    : type_assignment many(snd(',', type_assignment)) { $1:$2 }
variable_decl_assignments :: { [VariableDeclAssignment] }
    : variable_decl_assignment  many(snd(',', variable_decl_assignment)) { $1:$2 }
---- 2.4 - Declarations Assignments ----
-- defparam_assignment :: { DefparamAssignment }
-- net_decl_assignment :: { NetDeclAssignment }
param_assignment :: { ParamAssignment }
    : parameter_identifier many(unpacked_dimension) optional(snd('=',constant_param_expression)) {
        ParamAssignment {
            parameterIdentifier = $1,
            unpackedDimensions = $2,
            constantParamExpression = $3
        }
    }
-- specparam_assignment :: { SpecparamAssignment }
type_assignment :: { TypeAssignment }
    : type_identifier optional(snd('=', data_type))   { TypeAssignment $1 $2 }

-- pulse_control_specparam :: { PulseControlSpecparam }
-- error_limit_value :: { ErrorLimitValue }
-- reject_limit_value :: { RejectLimitValue }
-- limit_value :: { LimitValue }
variable_decl_assignment :: { VariableDeclAssignment }
    : variable_identifier many(variable_dimension) optional(expression)     { VDA $1 $2 $3 }

-- class_new :: { ClassNew }
-- dynamic_array_new :: { DynamicArrayNew }

---- 2.5 - Declaration Ranges ----
unpacked_dimension :: { UnpackedDimension }
    : '[' constant_range ']'            { UDConstantRange $2 }
    | '[' constant_expression ']'       { UDConstantExpression $2 }

packed_dimension :: { PackedDimension }
    : '[' constant_range ']'            { PDConstantRange $2 }
    | unsized_dimension                 { PDUnsized $1 }

-- associative_dimension :: { AssociativeDimension }
variable_dimension :: { VariableDimension }
    : unsized_dimension                 { VDUnsized $1 }
    | unpacked_dimension                { VDUnpacked $1 }

-- queue_dimension :: { QueueDimension }
unsized_dimension :: { UnsizedDimension }
    : '[' ']'       { UnsizedDimension }
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
block_item_declaration :: { BlockItemDeclaration }
    : many(attribute_instance) data_declaration {BIDData $1 $2 }
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
parameter_value_assignment :: { ParameterValueAssignment }
    : '#' '(' optional(parameter_assignments) ')' { ParameterValueAssignment $3 }

parameter_assignments :: { ParameterAssignments }
    : ordered_parameter_assignment many(snd(',', ordered_parameter_assignment)) { PAOrdered ($1:$2) }
    | named_parameter_assignment many(snd(',', named_parameter_assignment)) { PANamed ($1:$2) }

ordered_parameter_assignment :: { OrderedParameterAssignment }
    : param_expression  { OrderedParameterAssignment $1  }

named_parameter_assignment :: { NamedParameterAssignment }
    : '.' parameter_identifier '(' optional(param_expression) ')' { NamedParameterAssignment $2 $4 }

-- hierarchical_instance :: { HierarchicalInstance }
-- name_of_instance :: { NameOfInstance }
-- port_connections :: { PortConnections }
-- ordered_port_connection :: { OrderedPortConnection }
-- named_port_connection :: { NamedPortConnection }

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
-- Incomplete and incorrect (delay3) ignored Production Rule
continuous_assign :: { ContinuousAssign }
    : assign optional(drive_strength) sep1(net_assignment, ',') { ContinuousAssign };

-- net_alias :: { NetAlias }
net_assignment :: { NetAssignment }
    : net_lvalue '=' expression                                 { NetAssignment }

---- 6.2 - Procedural Blocks And Assignments ----
-- initial_construct :: { InitialConstruct }
always_construct :: { AlwaysConstruct }
    : always_keyword statement                                  { AlwaysConstruct $1 $2 }
always_keyword :: { AlwaysKeyword }
    : always                                                    { Always }
    | always_comb                                               { AlwaysComb }
    | always_latch                                              { AlwaysLatch }
    | always_ff                                                 { AlwaysFf }
-- final_construct :: { FinalConstruct }
-- | Incomplete production rule
blocking_assignment :: { BlockingAssignment }
    : operator_assignment       { BAOperator $1 }

operator_assignment :: { OperatorAssignment }
    : variable_lvalue assignment_operator expression { OperatorAssignment $1 $2 $3 }

assignment_operator :: { AssignmentOperator }
    : '='       { AOEqual }
    | '+='      { AOPlusEqual }
    | '-='      { AOMinusEqual}
    | '*='      { AOAsteriskEqual }
    | '/='      { AOForwardslashEqual}
    | '%='      { AOPercentageEqual }
    | '&='      { AOAmpersandEqual }
    | '|='      { AOPipeEqual }
    | '^='      { AOCaretEqual }
    | '<<='     { AOLesserLesserEqual }
    | '>>='     { AOGreaterGreaterEqual }
    | '<<<='    { AOLesserLesserLesserEqual }
    | '>>>='    { AOGreaterGreaterGreaterEqual }

nonblocking_assignment :: { NonblockingAssignment }
    : variable_lvalue '<=' expression { NonblockingAssignment $1 $3 }
-- procedural_continuous_assignment :: { ProceduralContinousAssignment }
-- variable_assignment :: { VariableAssignment }

---- 6.3 - Parallel And Sequential Blocks ----
-- action_block :: { ActionBlock }
seq_block :: { SeqBlock }
    :begin optional(snd(':', block_identifier)) many(statement_or_null) end optional(snd(':', block_identifier))    {
        SeqBlock {
            blockIdentifier = $2 `orElse` $5,
            blockItemDeclarations = [],
            statementsOrNull = $3,
            startBlockIdentifier = $2,
            endBlockIdentifier = $5
        }
    }
    -- : begin optional(snd(':', block_identifier)) many(block_item_declaration) many(statement_or_null) end optional(snd(':', block_identifier))    {
    --     SeqBlock {
    --         blockIdentifier = $2 `orElse` $6,
    --         blockItemDeclarations = $3,
    --         statementsOrNull = $4,
    --         startBlockIdentifier = $2,
    --         endBlockIdentifier = $6
    --     }
    -- }
-- par_block :: { ParBlock }
-- join_keyword :: { JoinKeyword }

---- 6.4 - Statements ----
statement_or_null :: { StatementOrNull }
    : statement                             { Left $1 }
    | many(attribute_instance) ';'              { Right $1 }

statement :: { Statement }
    : statement_item    { Statement Nothing [] $1}
    -- | optional(fst(block_identifier, ':')) many(attribute_instance) statement_item { Statement $1 $2 $3 }

-- | Incomplete production rule
statement_item :: { StatementItem }
    : blocking_assignment ';'       { SIBlockingAssignment $1 }
    | nonblocking_assignment ';'    { SINonblockingAssignment $1 }
    | seq_block                     { SISeqBlock $1 }
    | procedural_timing_control_statement { SIProceduralTimingControlStatement $1 }
-- function_statement :: { FunctionStatement }
-- function_statement_or_null :: { FunctionStatementOrNull }

---- 6.5 - Timing Control Statements ----
procedural_timing_control_statement :: { ProceduralTimingControlStatement }
    : procedural_timing_control statement_or_null { ProceduralTimingControlStatement $1 $2 }
-- delay_or_event_control :: { DelayOrEventControl }
-- delay_control :: { DelayControl }
event_control :: { EventControl }
    : '@' '(' event_expression ')'      { ECExpression $3 }
    | '@' '(' '*' ')'                   { ECAsterisk }

-- | Incomplete Expression
event_expression :: { EventExpression }
    : expression { EE Nothing $1 Nothing}
    | edge_identifier expression    { EE (Just $1) $2 Nothing}

procedural_timing_control :: { ProceduralTimingControl }
    : event_control { PTCEvent $1 }
-- jump_statement :: { JumpStatement }
-- wait_statement :: { WaitStatement }
-- event_trigger :: { EventTrigger }
-- disable_statement :: { DisableStatement }

---- 6.6 - Conditional Statements ----
-- conditional_statement :: { ConditionalStatement }
-- unique_priority :: { UniquePriority }
-- cond_predicate :: { CondPredicate }
-- expression_or_cond_pattern :: { ExpressionOrCondPattern }
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
assignment_pattern_expression_type :: { AssignmentPatternExpressionType }
    : ps_type_identifier        { APETPsTypeIdentifier $1 }
    | ps_parameter_identifier   { APETPsParameterIdentifier $1}
    | integer_atom_type         { APETIntegerAtomType $1 }
    | type_reference            { APETTypeReference $1 }

-- constant_assignment_pattern_expression :: { ConstantAssignmentPatternExpression }
-- assignment_pattern_net_lvalue :: { AssignmentPatternNetLvalue }
assignment_pattern_variable_lvalue :: { AssignmentPatternVariableLvalue }
    : '\'' '{' variable_lvalue many(snd(',', variable_lvalue )) '}' { AssignmentPatternVariableLvalue ($3:$4) }

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
edge_identifier :: { EdgeIdentifier }
    : posedge       { EIPosedge }
    | negedge       { EINegedge }
    | edge          { EIEdge }
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
-- module_path_concatenation ::= { ModulePathConcatenation }
-- module_path_multiple_concatenation :: { ModulePathMultipleConcatenation }
-- multiple_concatenation :: { MultipleConcatenation }

streaming_concatenation :: { StreamingConcatenation }
    : '{' stream_operator optional(slice_size) stream_concatenation '}' { StreamingConcatenation $2 $3 $4 }

stream_operator :: { StreamOperator }
    : '>>'          { SOGreater }
    | '<<'          { SOLesser }

slice_size :: { SliceSize }
    : simple_type           { SSSimpleType $1 }
    | constant_expression   { SSConstantExpression $1 }

stream_concatenation :: { StreamConcatenation }
    : '{' stream_expression many(snd(',', stream_expression)) '}' { StreamConcatenation ($2:$3) }

stream_expression :: { StreamExpression }
    : expression optional(getThirdFromFour(with,'[', array_range_expression, ']'))  { StreamExpression $1 $2 }

array_range_expression :: { ArrayRangeExpression }
    : expression                    { ARE $1 }
    | expression ':' expression     { AREColon $1 $3 }
    | expression '+:' expression    { AREPlusColon $1 $3 }
    | expression '-:' expression    { AREMinusColon $1 $3 }

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
constant_expression = AST.CEPrimary <$> constant_primary <?> "constant_expression"


constant_mintypmax_expression :: { ConstantMintypmaxExpression }
    : constant_expression                                                   { CMESingle  $1 }
    | constant_expression ':' constant_expression ':' constant_expression   { CMETriple $1 $3 $5 }

constant_param_expression :: { ConstantParamExpression }
    : constant_mintypmax_expression         { CPEMintypmax $1 }
    | data_type                             { CPEDataType $1 }
    | '$'                                   { CPEDollar }

param_expression :: { ParamExpression }
    : mintypmax_expression          { PEMintypmax $1 }
    | data_type                     { PEDataType $1 }
    | '$'                           { PEDollar }
-- constant_range_expression :: { ConstantRangeExpression }
-- constant_part_select_range :: { ConstantPartSelectRange }

-- | Incomplete production rule
constant_range = AST.CRExpression <$> constant_expression <* colon <*> constant_expression
-- constant_indexed_range ::{ ConstantIndexedRange }
-- | Incorrect production rule
expression = AST.EPrimary <$> primary <?> "expression"
-- tagged_union_expression :: { TaggedUnionExpression}
-- inside_expression :: { InsideExpression }
-- value_range :: { ValueRange }
mintypmax_expression :: { MintypmaxExpression }
    : expression                                  { MESingle $1 }
    | expression ':' expression ':' expression  { METripple $1 $3 $5 }
-- module_path_conditional_expression :: { ModulePathConditionalExpression }
-- module_path_expression :: { ModulePathExpression }
-- module_path_mintypmax_expression :: { ModulePathMintypmaxExpression}
part_select_range = AST.PSRConstant <$> constant_range <?> "part_select_range"

indexed_range :: { IndexedRange }
    : expression '+:' constant_expression       { IRPlusColon $1 $3 }
    | expression '-:' constant_expression       { IRMinusColon $1 $3 }
-- genvar_expression :: { GenvarExpression}

---- 8.4 - Primaries ----
constant_primary = AST.CPLiteral <$> primary_literal <?> "constant_primary"
-- module_path_primary :: { ModulePathPrimary }
primary =   AST.PLiteral <$> primary_literal
            <|> AST.PHierarchicalIdentifier hierarchical_identifier <*> select <?> "primary"

-- class_qualifier :: { ClassQualifier}
-- range_expression :: { RangeExpression }
primary_literal = AST.PLNumber <$> number <?> "primary_literal"

time_literal :: { TimeLiteral }
    : unsigned_number time_unit                                 { TLUnsigned (L.unTokDecimal $1) (L.unTokTimeUnit $2) }
    | unsigned_number '.' unsigned_number time_unit             { TLFixedPoint (L.unTokDecimal $1) (L.unTokDecimal $3) (L.unTokTimeUnit $4) }

-- time_unit :: { TimeUnit }
implicit_class_handle :: { ImplicitClassHandle }
    : this '.'              { ICHThis }
    | super                 { ICHSuper }
    | this '.' super        { ICHThisSuper }

bit_select = AST.BitSelect <$> many(osb *> expression <* csb) <?> "bit_select"

select = AST.Select
            <$> optional( (,) <$> many((,) <$> member_identifier <*> bit_select) <*> member_identifier)
            <*> bit_select
            <*> optional ( osb *> part_select_range <* csb)
            <?> "select"


member_identifier_bit_selects :: { ([(MemberIdentifier, BitSelect)], MemberIdentifier) }
    : many(snd('.', member_identifier_bit_select)) '.' member_identifier { ($1, $3)}

member_identifier_bit_select :: { (MemberIdentifier, BitSelect) }
    : member_identifier bit_select { ($1,$2) }

-- nonrange_select :: { NonrangeSelect }
constant_bit_select = AST.ConstantBitSelect <$> many( osb *> constant_expression <* csb) <?> "constant_bit_select"

-- Incorrect production rule
constant_select :: { ConstantSelect }
    : '[' ']'                                               { ConstantSelect }

-- constant_cast :: { ConstantCast }
-- constant_let_expression :: { ConstantLetExpression }
-- cast :: { Cast }

---- 8.5 - Expression Left-Side Values ----
-- Incomplete production rule
net_lvalue :: { NetLValue }
    : ps_or_hierarchical_net_identifier constant_select     { NetLValue $1 $2 }

variable_lvalue = AST.VLHierarchical <$> optional implicit_class_handle_or_package_scope <*> hierarchical_variable_identifier <*> select <?> "variable_lvalue"

implicit_class_handle_or_package_scope :: { ImplicitClassHandleOrPackageScope }
    : implicit_class_handle             { Left $1 }
    | package_scope                     { Right $1 }

-- nonrange_variable_lvalue :: { NonrangeVariableLvalue }

---- 8.6 - Operators ----
---- 8.7 - Numbers ----

-- Incomplete production rule
number = AST.NIntegral <$> integral_number <?> "integral_number"

-- Incomplete production rule
integral_number = AST.INDecimal <$> decimal_number <?> "integral_number"

decimal_number = AST.DNUnsigned <$> unsigned_number <?> "decimal_number"

---- 8.8 - Strings ----

---- Sec 9 ----
---- 9.1 - Attributes ----
attribute_instance :: { AttributeInstance }
    : '(' '*' attr_spec many(snd(',', attr_spec)) '*' ')' { AttributeInstance ($3:$4) }
-- | Incorrect production rule (Constant Expression should not be Identifier)
attr_spec :: { AttrSpec }
    : attr_name optional(snd('=', identifier))          { AttrSpec{ attrName=$1, constantExpression=$2} }
attr_name :: { AttrName }
    : identifier { AttrName $1 }

---- 9.2 - Comments ----
-- comment :: { Comment }
-- one_line_comment :: { OneLineComment }
-- block_comment :: { BlockComment }
-- comment_text :: { CommonText }

---- 9.3 - Identifiers ----
block_identifier =
    BlockIdentifier <$> identifier <?> "block_identifier"

class_identifier =
    ClassIdentifier <$> identifier <?> "class_identifier"

hierarchical_identifier =
    AST.HierarchicalIdentifier
    <$> pure False
    <*> many ((,) <$> identifier <*> constant_bit_select <* fullstop)
    <*> identifier
    <?> "hierarchical_identifier"


identifier_constant_bit_select =
    (,) <$> identifier <*> constant_bit_select <* fullstop <?> "identifier_constant_bit_select"

hierarchical_variable_identifier = AST.HierarchicalVariableIdentifier <$> hierarchical_identifier <?> "hierarchical_variable_identifier"

interface_identifier =
    InterfaceIdentifier <$> identifier <?> "interface_identifier"

parameter_identifier =
    ParameterIdentifier <$> identifier <?> "parameter_identifier"
module_identifier =
    ModuleIdentifier <$> identifier <?> "module_identifier"
-- Incomplete production rule
package_scope =
    PSidentifier <$> package_identifier *> colon_colon <?> "package_scope"

package_identifier =
    PackageIdentifier <$> identifier <?> "package_identifier"
port_identifier =
    PortIdentifier <$> identifier <?> "port_identifier"
ps_class_identifier =
    PsClassIdentifier <$> optional package_scope <*> class_identifier <?> "ps_class_identifier"

ps_or_hierarchical_net_identifier =
    POHNINet <$> optional package_scope <*> net_identifier <?> "ps_or_hierarchical_net_identifier"

-- Incomplete production rule
ps_parameter_identifier =
    PPIScoped <$> optional either(package_scope class_scope) <*> parameter_identifier <?> "ps_parameter_identifier"

ps_parameter_identifier =
    PPIScoped <$> optional
ps_type_identifier =
    PsTypeIdentifier <$> optional local_or_package_scope <*> type_identifier
    <?> "ps_type_identifier"

local_or_package_scope =
    local_colon_colon *> pure LOPSLocal
    <|> LOPSPackageScope <$> package_scope
    <?> "local_or_package_scope"

member_identifier = AST.MemberIdentifier <$> identifier <?> "identifier"
net_identifier = NetIdentifier <$> identifier <?> "net_identifier"
type_identifier = TypeIdentifier <$> identifier <?> "type_identifier"
variable_identifier = VariableIdentifier <$> identifier <?> "variable_identifier"