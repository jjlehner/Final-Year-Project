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
    always_ff                   { L.RangedToken L.AlwaysFf _ }
    always_latch                { L.RangedToken L.AlwaysLatch _ }
    and                         { L.RangedToken L.And _ }
    assert                      { L.RangedToken L.Assert _ }
    assign                      { L.RangedToken L.Assign _ }
    assume                      { L.RangedToken L.Assume _ }
    automatic                   { L.RangedToken L.Automatic _ }
    before                      { L.RangedToken L.Before _ }
    begin                       { L.RangedToken L.Begin _ }
    bind                        { L.RangedToken L.Bind _ }
    bins                        { L.RangedToken L.Bins _ }
    binsof                      { L.RangedToken L.Binsof _ }
    bit                         { L.RangedToken L.Bit _ }
    break                       { L.RangedToken L.Break _ }
    buf                         { L.RangedToken L.Buf _ }
    bufif0                      { L.RangedToken L.Bufif0 _ }
    bufif1                      { L.RangedToken L.Bufif1 _ }
    byte                        { L.RangedToken L.Byte _ }
    case                        { L.RangedToken L.Case _ }
    casex                       { L.RangedToken L.Casex _ }
    casez                       { L.RangedToken L.Casez _ }
    cell                        { L.RangedToken L.Cell _ }
    chandle                     { L.RangedToken L.Chandle _ }
    checker                     { L.RangedToken L.Checker _ }
    class                       { L.RangedToken L.Class _ }
    clocking                    { L.RangedToken L.Clocking _ }
    cmos                        { L.RangedToken L.Cmos _ }
    config                      { L.RangedToken L.Config _ }
    constraint                  { L.RangedToken L.Constraint _ }
    context                     { L.RangedToken L.Context _ }
    continue                    { L.RangedToken L.Continue _ }
    cover                       { L.RangedToken L.Cover _ }
    covergroup                  { L.RangedToken L.Covergroup _ }
    coverpoint                  { L.RangedToken L.Coverpoint _ }
    cross                       { L.RangedToken L.Cross _ }
    deassign                    { L.RangedToken L.Deassign _ }
    default                     { L.RangedToken L.Default _ }
    defparam                    { L.RangedToken L.Defparam _ }
    design                      { L.RangedToken L.Design _ }
    disable                     { L.RangedToken L.Disable _ }
    dist                        { L.RangedToken L.Dist _ }
    do                          { L.RangedToken L.Do _ }
    edge                        { L.RangedToken L.Edge _ }
    else                        { L.RangedToken L.Else _ }
    end                         { L.RangedToken L.End _ }
    endcase                     { L.RangedToken L.Endcase _ }
    endchecker                  { L.RangedToken L.Endchecker _ }
    endclass                    { L.RangedToken L.Endclass _ }
    endclocking                 { L.RangedToken L.Endclocking _ }
    endconfig                   { L.RangedToken L.Endconfig _ }
    endfunction                 { L.RangedToken L.Endfunction _ }
    endgenerate                 { L.RangedToken L.Endgenerate _ }
    endgroup                    { L.RangedToken L.Endgroup _ }
    endinterface                { L.RangedToken L.Endinterface _ }
    endmodule                   { L.RangedToken L.Endmodule _ }
    endpackage                  { L.RangedToken L.Endpackage _ }
    endprimitive                { L.RangedToken L.Endprimitive _ }
    endprogram                  { L.RangedToken L.Endprogram _ }
    endproperty                 { L.RangedToken L.Endproperty _ }
    endspecify                  { L.RangedToken L.Endspecify _ }
    endsequence                 { L.RangedToken L.Endsequence _ }
    endtable                    { L.RangedToken L.Endtable _ }
    endtask                     { L.RangedToken L.Endtask _ }
    enum                        { L.RangedToken L.Enum _ }
    event                       { L.RangedToken L.Event _ }
    eventually                  { L.RangedToken L.Eventually _ }
    expect                      { L.RangedToken L.Expect _ }
    export                      { L.RangedToken L.Export _ }
    extends                     { L.RangedToken L.Extends _ }
    extern                      { L.RangedToken L.Extern _ }
    final                       { L.RangedToken L.Final _ }
    first_match                 { L.RangedToken L.FirstMatch _ }
    for                         { L.RangedToken L.For _ }
    force                       { L.RangedToken L.Force _ }
    foreach                     { L.RangedToken L.Foreach _ }
    forever                     { L.RangedToken L.Forever _ }
    fork                        { L.RangedToken L.Fork _ }
    forkjoin                    { L.RangedToken L.Forkjoin _ }
    function                    { L.RangedToken L.Function _ }
    generate                    { L.RangedToken L.Generate _ }
    genvar                      { L.RangedToken L.Genvar _ }
    global                      { L.RangedToken L.Global _ }
    highz0                      { L.RangedToken L.Highz0 _ }
    highz1                      { L.RangedToken L.Highz1 _ }
    if                          { L.RangedToken L.If _ }
    iff                         { L.RangedToken L.Iff _ }
    ifnone                      { L.RangedToken L.Ifnone _ }
    ignore_bins                 { L.RangedToken L.IgnoreBins _ }
    illegal_bins                { L.RangedToken L.IllegalBins _ }
    implements                  { L.RangedToken L.Implements _ }
    implies                     { L.RangedToken L.Implies _ }
    import                      { L.RangedToken L.Import _ }
    incdir                      { L.RangedToken L.Incdir _ }
    include                     { L.RangedToken L.Include _ }
    initial                     { L.RangedToken L.Initial _ }
    inout                       { L.RangedToken L.Inout _ }
    input                       { L.RangedToken L.Input _ }
    inside                      { L.RangedToken L.Inside _ }
    instance                    { L.RangedToken L.Instance _ }
    int                         { L.RangedToken L.Int _ }
    integer                     { L.RangedToken L.Integer _ }
    interconnect                { L.RangedToken L.Interconnect _ }
    interface                   { L.RangedToken L.Interface _ }
    intersect                   { L.RangedToken L.Intersect _ }
    join                        { L.RangedToken L.Join _ }
    join_any                    { L.RangedToken L.JoinAny _ }
    large                       { L.RangedToken L.Large _ }
    let                         { L.RangedToken L.Let _ }
    liblist                     { L.RangedToken L.Liblist _ }
    library                     { L.RangedToken L.Library _ }
    local                       { L.RangedToken L.Local _ }
    localparam                  { L.RangedToken L.Localparam _ }
    logic                       { L.RangedToken L.Logic _ }
    longint                     { L.RangedToken L.Longint _ }
    macromodule                 { L.RangedToken L.Macromodule _ }
    matches                     { L.RangedToken L.Matches _ }
    medium                      { L.RangedToken L.Medium _ }
    modport                     { L.RangedToken L.Modport _ }
    module                      { L.RangedToken L.Module _ }
    nand                        { L.RangedToken L.Nand _ }
    negedge                     { L.RangedToken L.Negedge _ }
    nettype                     { L.RangedToken L.Nettype _ }
    new                         { L.RangedToken L.New _ }
    nexttime                    { L.RangedToken L.Nexttime _ }
    nmos                        { L.RangedToken L.Nmos _ }
    nor                         { L.RangedToken L.Nor _ }
    noshowcancelled             { L.RangedToken L.Noshowcancelled _ }
    not                         { L.RangedToken L.Not _ }
    notif0                      { L.RangedToken L.Notif0 _ }
    notif1                      { L.RangedToken L.Notif1 _ }
    null                        { L.RangedToken L.Null _ }
    or                          { L.RangedToken L.Or _ }
    output                      { L.RangedToken L.Output _ }
    package                     { L.RangedToken L.Package _ }
    packed                      { L.RangedToken L.Packed _ }
    parameter                   { L.RangedToken L.Parameter _ }
    pmos                        { L.RangedToken L.Pmos _ }
    posedge                     { L.RangedToken L.Posedge _ }
    primitive                   { L.RangedToken L.Primitive _ }
    priority                    { L.RangedToken L.Priority _ }
    program                     { L.RangedToken L.Program _ }
    property                    { L.RangedToken L.Property _ }
    protected                   { L.RangedToken L.Protected _ }
    pull0                       { L.RangedToken L.Pull0 _ }
    pull1                       { L.RangedToken L.Pull1 _ }
    pulldown                    { L.RangedToken L.Pulldown _ }
    pullup                      { L.RangedToken L.Pullup _ }
    pulsestyle_ondetect         { L.RangedToken L.PulsestyleOndetect _ }
    pulsestyle_onevent          { L.RangedToken L.PulsestyleOnevent _ }
    pure                        { L.RangedToken L.Pure _ }
    rand                        { L.RangedToken L.Rand _ }
    randc                       { L.RangedToken L.Randc _ }
    randcase                    { L.RangedToken L.Randcase _ }
    randsequence                { L.RangedToken L.Randsequence _ }
    rcmos                       { L.RangedToken L.Rcmos _ }
    real                        { L.RangedToken L.Real _ }
    realtime                    { L.RangedToken L.Realtime _ }
    ref                         { L.RangedToken L.Ref _ }
    reg                         { L.RangedToken L.Reg _ }
    reject_on                   { L.RangedToken L.RejectOn _ }
    release                     { L.RangedToken L.Release _ }
    repeat                      { L.RangedToken L.Repeat _ }
    restrict                    { L.RangedToken L.Restrict _ }
    return                      { L.RangedToken L.Return _ }
    rnmos                       { L.RangedToken L.Rnmos _ }
    rpmos                       { L.RangedToken L.Rpmos _ }
    rtran                       { L.RangedToken L.Rtran _ }
    rtranif0                    { L.RangedToken L.Rtranif0 _ }
    rtranif1                    { L.RangedToken L.Rtranif1 _ }
    s_always                    { L.RangedToken L.SAlways _ }
    s_eventually                { L.RangedToken L.SEventually _ }
    s_nexttime                  { L.RangedToken L.SNexttime _ }
    s_until                     { L.RangedToken L.SUntil _ }
    s_until_with                { L.RangedToken L.SUntilWith _ }
    scalared                    { L.RangedToken L.Scalared _ }
    sequence                    { L.RangedToken L.Sequence _ }
    shortint                    { L.RangedToken L.Shortint _ }
    shortreal                   { L.RangedToken L.Shortreal _ }
    showcancelled               { L.RangedToken L.Showcancelled _ }
    signed                      { L.RangedToken L.Signed _ }
    small                       { L.RangedToken L.Small _ }
    soft                        { L.RangedToken L.Soft _ }
    solve                       { L.RangedToken L.Solve _ }
    specify                     { L.RangedToken L.Specify _ }
    specparam                   { L.RangedToken L.Specparam _ }
    static                      { L.RangedToken L.Static _ }
    string                      { L.RangedToken L.String _ }
    strong                      { L.RangedToken L.Strong _ }
    strong0                     { L.RangedToken L.Strong0 _ }
    strong1                     { L.RangedToken L.Strong1 _ }
    struct                      { L.RangedToken L.Struct _ }
    super                       { L.RangedToken L.Super _ }
    supply0                     { L.RangedToken L.Supply0 _ }
    supply1                     { L.RangedToken L.Supply1 _ }
    sync_accept_on              { L.RangedToken L.SyncAcceptOn _ }
    sync_reject_on              { L.RangedToken L.SyncRejectOn _ }
    table                       { L.RangedToken L.Table _ }
    tagged                      { L.RangedToken L.Tagged _ }
    task                        { L.RangedToken L.Task _ }
    this                        { L.RangedToken L.This _ }
    throughout                  { L.RangedToken L.Throughout _ }
    time                        { L.RangedToken L.Time _ }
    timeprecision               { L.RangedToken L.Timeprecision _ }
    timeunit                    { L.RangedToken L.Timeunit _ }
    tran                        { L.RangedToken L.Tran _ }
    tranif0                     { L.RangedToken L.Tranif0 _ }
    tranif1                     { L.RangedToken L.Tranif1 _ }
    tri                         { L.RangedToken L.Tri _ }
    tri0                        { L.RangedToken L.Tri0 _ }
    tri1                        { L.RangedToken L.Tri1 _ }
    triand                      { L.RangedToken L.Triand _ }
    trior                       { L.RangedToken L.Trior _ }
    trireg                      { L.RangedToken L.Trireg _ }
    type                        { L.RangedToken L.Type _ }
    typedef                     { L.RangedToken L.Typedef _ }
    union                       { L.RangedToken L.Union _ }
    unique                      { L.RangedToken L.Unique _ }
    unique0                     { L.RangedToken L.Unique0 _ }
    unsigned                    { L.RangedToken L.Unsigned _ }
    until                       { L.RangedToken L.Until _ }
    until_with                  { L.RangedToken L.UntilWith _ }
    untyped                     { L.RangedToken L.Untyped _ }
    use                         { L.RangedToken L.Use _ }
    uwire                       { L.RangedToken L.Uwire _ }
    var                         { L.RangedToken L.Var _ }
    vectored                    { L.RangedToken L.Vectored _ }
    virtual                     { L.RangedToken L.Virtual _ }
    void                        { L.RangedToken L.Void _ }
    wait                        { L.RangedToken L.Wait _ }
    wait_order                  { L.RangedToken L.WaitOrder _ }
    wand                        { L.RangedToken L.Wand _ }
    weak                        { L.RangedToken L.Weak _ }
    weak0                       { L.RangedToken L.Weak0 _ }
    weak1                       { L.RangedToken L.Weak1 _ }
    while                       { L.RangedToken L.While _ }
    wildcard                    { L.RangedToken L.Wildcard _ }
    wire                        { L.RangedToken L.Wire _ }
    with                        { L.RangedToken L.With _ }
    within                      { L.RangedToken L.Within _ }
    wor                         { L.RangedToken L.Wor _ }
    xnor                        { L.RangedToken L.Xnor _ }
    xor                         { L.RangedToken L.Xor _ }

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
    '='                         { L.RangedToken L.Equals _ }
    ','                         { L.RangedToken L.Comma _ }
    '/'                         { L.RangedToken L.Backslash _ }
    '#'                         { L.RangedToken L.Hashtag _ }
    '$'                         { L.RangedToken L.Dollar _ }
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

----- Sec 1 -----
---- 1.1 - Library Source Text ----
---- 1.2 - SystemVerilog Source Text ----
source_text :: { SourceText }
    : optional(timeunits_declaration) many(description) { SourceText $1 $2 }

description :: { Description }
    : module_declaration { DModuleDeclaration $1 }

-- module_nonansi_header :: { ModuleNonansiHeader }

-- Incomplete production rule
module_ansi_header :: { ModuleAnsiHeader }
    : many(attribute_instance) module_keyword optional(lifetime) module_identifier many(package_import_declaration) optional(parameter_ports) optional(port_declarations) ';' {
        ModuleAnsiHeader {
            attributeInstances = $1,
            moduleKeyword = $2,
            lifetime = $3,
            moduleIdentifier = $4,
            packageImportDeclarations = $5,
            parameterPorts = $6,
            portDeclarations = $7
        }
    }

module_declaration :: { ModuleDeclaration }
    : module_ansi_header optional(timeunits_declaration) many(non_port_module_item) endmodule optional(snd(':', module_identifier)) { MDAnsiHeader {
        moduleAnsiHeader=$1,
        timeunitsDeclarations=$2,
        nonPortModuleItems=$3
    }}

module_keyword :: { ModuleKeyword }
    : module                                            { Module }
    | macromodule                                       { Macromodule }

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
timeunits_declaration :: { TimeunitsDeclaration }
    : timeunit time_literal optional(snd('/', time_literal)) ';' { TimeunitsDeclaration }

---- 1.3 - Module parameters and Ports ----
-- | Referred to as parameter_port_list in Standard
parameter_ports :: { ParameterPorts }
    : '#' '(' param_assignments many(snd(',', parameter_port_declaration)) ')' {
        ParameterPorts {
            parameterAssignments = $3,
            parameterPortDeclaration = $4
        }}
    | '#' '(' parameter_port_declaration many(snd(',', parameter_port_declaration)) ')' {
        ParameterPorts {
            parameterAssignments = [],
            parameterPortDeclaration = $3:$4
        }}
    | '#' '(' ')' {
        ParameterPorts {
            parameterAssignments = [],
            parameterPortDeclaration = []
        }}

parameter_port_declaration :: { ParameterPortDeclaration }
    : parameter_declaration         { PPD $1 }
    | local_parameter_declaration   { PPDLocal $1 }
    | data_type param_assignments   { PPDDataTypeParamAssignments $1 $2 }
    | type type_assignments         { PPDTypeAssignments $2 }

port_declarations :: { [PortDeclarationsItem] }
    : '(' ')'                                                  { [] }
    | '(' port_declarations_item many(snd(',', port_declarations_item)) ')'                 { $2:$3 }

port_declarations_item :: { PortDeclarationsItem }
    : many(attribute_instance) ansi_port_declaration            { PortDeclarationsItem $1 $2 }

-- port_declaration :: { PortDeclaration }
-- port :: { Port }
-- port_expression :: { PortExpression }
-- port_reference :: { PortReference }
port_direction :: { PortDirection }
    : input                                                     { PDInput }
    | output                                                    { PDOutput }
    | inout                                                     { PDInout }
    | ref                                                       { PDRef }

net_port_header :: { NetPortHeader }
    : optional(port_direction) net_port_type                    { NetPortHeader $1 $2 }

variable_port_header :: { VariablePortHeader }
    : optional(port_direction) variable_port_type                       { VariablePortHeader $1 $2 }

interface_port_header :: { InterfacePortHeader }
    : interface_identifier optional(snd('.', modport_identifier))       { IPDNamed $1 $2 }
    | interface optional(snd('.', modport_identifier))                  { IPDAnonymous $2 }

-- Incomplete Production Rule
ansi_port_declaration :: { AnsiPortDeclaration }
    : optional(net_or_interface_port_header) port_identifier many(unpacked_dimension) optional(snd('=',constant_expression))
        { APDNetOrInterfaceHeader{
            netOrInterfacePortHeader = $1,
            portIdentifier = $2,
            unpackedDimensions = $3,
            constantExpression = $4
        }}
    | optional(variable_port_header) port_identifier many(variable_dimension) optional(snd('=',constant_expression))
        { APDVariableHeader {
            variablePortHeader = $1,
            portIdentifier = $2,
            variableDimensions = $3,
            constantExpression = $4
        }}

net_or_interface_port_header :: { Either NetPortHeader InterfacePortHeader }
    : net_port_header { Left $1 }
    | interface_port_header { Right $1 }
---- 1.4 - Module Items ----
-- elaboration_system_task :: { ElaborationSystemTask }
-- finish_number :: { FinishNumber }

-- Incomplete Production Rule
module_common_item :: { ModuleCommonItem }
    : continuous_assign                                 { MCIContinuousAssign $1 }

-- module_item :: { ModuleItem }
-- Incomplete Production Rule
module_or_generate_item :: { ModuleOrGenerateItem }
    : many(attribute_instance) module_common_item   { MOGIModuleCommonItem $1 $2 }

-- module_or_generate_item_declaration :: { ModuleOrGenerateItemDeclaration }

-- Incomplete Production Rule
non_port_module_item :: { NonPortModuleItem }
    : module_or_generate_item                           { NPMIModuleOrGenerateItem $1 }

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
-- package_or_generate_item_declaration :: { PackageOrGenerateItemDeclaration }
-- anonymous_program :: { AnonymousProgram }
-- anonymous_program_item :: { AnonymousProgramItem }

----- Sec 2 -----
---- 2.1.1 - Module Parameter Declarations ----
local_parameter_declaration :: { LocalParameterDeclaration }
    : localparam data_type_or_implicit param_assignments {
        LPDParamAssignments {
            dataTypeOrImplicit = $2,
            paramAssignments = $3
        }
    } | localparam type type_assignments {
        LPDTypeAssignments {
            typeAssignments = $3
        }
    }

parameter_declaration :: { ParameterDeclaration }
    : parameter data_type_or_implicit param_assignments  {
        PDDataTypeOrImplicit {
            dataTypeOrImplicit = $2,
            paramAssignments = $3
        }
    }
    | parameter type type_assignments                   {
        PDTypeAssignments {
            typeAssignments = $3
        }
    }
-- specparam_declaration :: { SpecparamDeclaration }

---- 2.1.2 - Port Declarations ----
-- inout_declaration :: { InoutDeclaration }
-- input_declaration :: { InputDeclaration }
-- output_declaration :: { OutputDeclaration }
-- interface_port_declaration :: { InterfacePortDeclaration }
-- ref_declaration :: { RefDeclaration }

---- 2.1.3 - Type Declarations ----
-- data_declaration :: { DataDeclaration }
package_import_declaration :: { PackageImportDeclaration }
    : import package_import_item many(snd(',', package_import_item)) ';' { PackageImportDeclaration ($2:$3) }

package_import_item :: { PackageImportItem }
    : package_identifier "::" identifier          { PIIIdentifier $1 $3 }
    | package_identifier "::" '*'                   { PIIWildcard $1 }
-- package_export_declaration :: { PackageExportDeclaration }
-- genvar_declaration :: { GenvarDeclaration }
-- net_declaration :: { NetDeclaration }
-- type_declaration :: { TypeDeclaration }
-- net_type_declaration :: { NetTypeDeclaration }
lifetime :: { Lifetime }
    : automatic                                         { Automatic }
    | static                                            { Static }

---- 2.2.1 - Net and Variable Types ----
-- casting_type :: { CastingType }
-- data_type :: { DataType }

-- | Incomplete Production Rule
data_type :: { DataType }
    : integer_vector_type optional(signing) many(packed_dimension) {
        DTIntegerVector {
            integerVectorType = $1,
            signing = $2,
            packedDimension = $3
        }
    }

data_type_or_implicit :: { DataTypeOrImplicit }
    : data_type             { Left $1 }
    | implicit_data_type    { Right $1 }

implicit_data_type :: { ImplicitDataType }
    :  optional(signing) many(packed_dimension)                 { ImplicitDataType { signing=$1, packedDimensions=$2} }

-- enum_base_type :: { EnumBaseType }
-- enum_name_declaration :: { EnumNameDeclaration }
-- class_scope :: { ClassScope }
-- class_type :: { ClassType }
-- integer_type :: { IntegerType }
-- integer_atom_type :: { IntegerAtomType }
integer_vector_type :: { IntegerVectorType }
    : bit { Bit }
    | logic { Logic }
    | reg { Reg }
-- non_integer_type :: { NonIntegerType }
net_type :: { NetType }
    : supply0   { Supply0 }
    | supply1   { Supply1 }
    | tri       { Tri }
    | triand    { Triand }
    | trior     { Trior }
    | trireg    { Trireg }
    | tri0      { Tri0 }
    | tri1      { Tri1 }
    | uwire     { Uwire }
    | wire      { Wire }
    | wand      { Wand }
    | wor       { Wor }

net_port_type :: { NetPortType }
    : optional(net_type) data_type_or_implicit { NPTDataOrImplicit $1 $2 }

variable_port_type :: { VariablePortType }
    : var_data_type { VariablePortType $1 }

var_data_type :: { VarDataType }
    : data_type                     { VDT $1 }
    | var data_type_or_implicit     { VDTOrImplicit $2 }

signing :: { Signing }
    : signed                                    { SSigned }
    | unsigned                                  { SUnsigned }
-- simple_type :: { SimpleType }
-- struct_union_member :: { StructUnionMember }
-- data_type_or_void :: { DataTypeOrVoid }
-- struct_union :: { StructUnion }
-- type_reference :: { TypeReference }

---- 2.2.2 - Strengths ----
-- | Incorrect Production Rule
drive_strength :: { DriveStrength }
    : '[' ']'                       { DriveStrength }

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
-- variable_decl_assignment :: { VariableDeclAssignment }
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
-- block_item_declaration :: { BlockItemDeclaration }
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
-- parameter_value_assignment :: { ParameterValueAssignment }
-- parameter_assignments :: { ParameterAssignments }
-- ordered_parameter_assignment :: { OrderedParameterAssignment}
-- named_parameter_assignment :: { NamedParameterAssignment }
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
-- always_construct :: { AlwaysConstruct }
-- always_keyword :: { AlwaysKeyword }
-- final_construct :: { FinalConstruct }
-- blocking_assignment :: { BlockingAssignment }
-- operator_assignment :: { OperatorAssignment }
-- assignment_operator ::= { AssignmentOperator }
-- nonblocking_assignment :: { NonblockingAssignment }
-- procedural_continuous_assignment :: { ProceduralContinousAssignment }
-- variable_assignment :: { VariableAssignment }

---- 6.3 - Parallel And Sequential Blocks ----
-- action_block :: { ActionBlock }
-- seq_block :: { SeqBlock }
-- par_block :: { ParBlock }
-- join_keyword :: { JoinKeyword }

---- 6.4 - Statements ----
-- statement_or_null :: { StatementOrNull }
-- statement :: { Statement }
-- statement_item :: { StatementItem }
-- function_statement :: { FunctionStatement }
-- function_statement_or_null :: { FunctionStatementOrNull }

---- 6.5 - Timing Control Statements ----
-- procedural_timing_control_statement :: { ProceduralTimingControlStatement }
-- delay_or_event_control :: { DelayOrEventControl }
-- delay_control :: { DelayControl }
-- event_control :: { EventControl }
-- event_expression :: { EventExpression }
-- procedural_timing_control :: { ProceduralTimingControl }
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
-- assignment_pattern_expression_type :: { AssignmentPatternExpressionType }
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
-- edge_identifier :: { EdgeIdentifier }
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
-- streaming_concatenation :: { StreamingConcatenation }
-- stream_operator :: { StreamOperator }
-- slice_size :: { SliceSize }
-- stream_concatenation :: { StreamConcatenation }
-- stream_expression :: { StreamExpression }
-- array_range_expression :: { ArrayRangeExpression }
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
constant_expression :: { ConstantExpression }
    : constant_primary              { CEPrimary $1 }

constant_mintypmax_expression :: { ConstantMintypmaxExpression }
    : constant_expression                                                   { CMESingle  $1 }
    | constant_expression ':' constant_expression ':' constant_expression   { CMETriple $1 $3 $5 }

constant_param_expression :: { ConstantParamExpression }
    : constant_mintypmax_expression         { CPEMintypmax $1 }
    | data_type                             { CPEDataType $1 }
    | '$'                                   { CPEDollar }
-- param_expression :: { ParamExpression }
-- constant_range_expression :: { ConstantRangeExpression }
-- constant_part_select_range :: { ConstantPartSelectRange }

-- | Incomplete production rule
constant_range :: { ConstantRange }
    : constant_expression                   { CRExpression $1 }
-- constant_indexed_range ::{ ConstantIndexedRange }
-- | Incorrect production rule
expression :: { Expression }
    : '[' ']'                                               { Expression }
-- tagged_union_expression :: { TaggedUnionExpression}
-- inside_expression :: { InsideExpression }
-- value_range :: { ValueRange }
-- mintypmax_expression :: { MintypmaxExpression }
-- module_path_conditional_expression :: { ModulePathConditionalExpression }
-- module_path_expression :: { ModulePathExpression }
-- module_path_mintypmax_expression :: { ModulePathMintypmaxExpression}
-- part_select_range :: {PartSelectRange }
-- indexed_range :: { IndexedRange }
-- genvar_expression :: { GenvarExpression}

---- 8.4 - Primaries ----
constant_primary :: { ConstantPrimary }
    : primary_literal { CPLiteral $1 }
-- module_path_primary :: { ModulePathPrimary }
-- primary :: { Primary }
-- class_qualifier :: { ClassQualifier}
-- range_expression :: { RangeExpression }
primary_literal :: { PrimaryLiteral }
    : number                                                    { PLNumber $1 }
time_literal :: { TimeLiteral }
    : unsigned_number time_unit                                 { TLUnsigned (L.unTokDecimal $1) (L.unTokTimeUnit $2) }
    | unsigned_number '.' unsigned_number time_unit             { TLFixedPoint (L.unTokDecimal $1) (L.unTokDecimal $3) (L.unTokTimeUnit $4) }

-- time_unit :: { TimeUnit }
-- implicit_class_handle :: { ImplicitClassHandle }
-- bit_select :: { BitSelect }
-- select :: { Select }
-- nonrange_select :: { NonrangeSelect }
-- constant_bit_select :: { ConstantBitSelect }
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

-- variable_lvalue :: { VariableLvalue }
-- nonrange_variable_lvalue :: { NonrangeVariableLvalue }

---- 8.6 - Operators ----
---- 8.7 - Numbers ----

-- Incomplete production rule
number :: { Number }
    : integral_number   { NIntegral $1 }

-- Incomplete production rule
integral_number :: { IntegralNumber }
    : decimal_number    { INDecimal $1 }

decimal_number :: { DecimalNumber }
     : unsigned_number   { DNUnsigned (L.unTokDecimal $1) }

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
interface_identifier :: { InterfaceIdentifier }
    : identifier                                            { InterfaceIdentifier $1 }
parameter_identifier :: { ParameterIdentifier }
    : identifier                                            { ParameterIdentifier $1 }

module_identifier :: { ModuleIdentifier }
    : identifier { ModuleIdentifier $1 }

-- Incomplete production rule
ps_or_hierarchical_net_identifier :: { PsOrHierachicalNetIdentifier }
    : optional(package_scope) net_identifier                { POHNINet $1 $2 }

-- Incomplete production rule
package_scope :: { PackageScope }
    : package_identifier "::"                               { PSIdentifier $1 }

package_identifier :: { PackageIdentifier }
    : identifier                                            { PackageIdentifier $1 }

port_identifier :: { PortIdentifier }
    : identifier                                            { PortIdentifier $1 }

modport_identifier :: { ModportIdentifier }
    : identifier                                            { ModportIdentifier $1 }

net_identifier :: { NetIdentifier }
    : identifier                                            { NetIdentifier $1 }

type_identifier :: { TypeIdentifier }
    : identifier                                            { TypeIdentifier $1 }
----------------------------

{
parseError :: (L.RangedToken, [String]) -> L.Alex a
parseError (_,b) = do
  (L.AlexPn _ line column, _, _, _) <- L.alexGetInput
  L.alexError $ "Parse error at line " <> show line <> ", column " <> show column <> " -- Possible tokens " <> (intercalate ", " b)

lexer :: (L.RangedToken -> L.Alex a) -> L.Alex a
lexer = (=<< L.alexMonadScan)
}