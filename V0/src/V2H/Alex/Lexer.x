{
module V2H.AlexLexer.Lexer (
    Alex,
    AlexPosn (..),
    Range (..),
    RangedToken (..),
    Token (..),
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan
) where
import qualified Data.ByteString.Lazy.Char8 as BS
}
%wrapper "monadUserState-bytestring"
$alpha = [a-zA-Z]
$decimalDigit = [0-9]

@identifier = ($alpha | \_) ($alpha | $decimalDigit | \_ | \$)*

tokens :-

<0> $white+ ;

-- Keywords
<0> "accept_on"                 { tok AcceptOn }
<0> "alias"                     { tok Alias }
<0> "always"                    { tok Always }
<0> "always_comb"               { tok AlwaysComb }
<0> "always_ff"                 { tok AlwaysFf }
<0> "always_latch"              { tok AlwaysLatch}

<0> "automatic"                 { tok Automatic }

<0> "endmodule"                 { tok Endmodule }

<0> "module"                    { tok Module }

<0> "static"                    { tok Static }

<0> $decimalDigit+            { tokDecimal }
{
data UnaryOp =  PlusUOp
                | MinusUOp
                | ExclamationUOp
                | TildeUOp
                | AmpersandUOp
                | TidleAmpersandUOp
                | PipeUOp
                | TildePipeUOp
                | CaretUOp
                | TildeCaretUOp
                | CaretTildeUOp

data BinaryOp = PlusBOp
                | MinusBOp
                | AsteriskBOp
                | BackslashBOp
                | PercentBop
                | EqualEqualBOp
                | ExlamationEqualBOp
                | EqualEqualEqualBOp
                | ExclamationEqualEqualBOp
                | EqualEqualQuestionBOp
                | ExlamationEqualQuestionBOp
                | AmpersandAmpersandBOp
                | PipePipeBOp
                | AsteriskAsteriskBOp
                | LessBOp
                | LessEqualBOp
                | GreaterBop
                | GreaterEqualBOp
                | AmpersandBOp
                | PipeBOp
                | CaretBOp
                | CaretTildeBOp
                | TildeCaretBOp
                | GreaterGreaterBOp
                | LessLessBOp
                | GreaterGreaterGreaterBOp
                | LessLessLessBOp
                | DashGreaterBOp
                | LessDashGreaterBOp

data IncOrDecOp =   IncOp
                    | DecOp

data UnaryModulePathOp =    ExclamationUMPOp
                            | TildeUMPOp
                            | AmpersandUMPOp
                            | TildeAmpersandUMPOp
                            | PipeUMPOp
                            | TildePipeUMPOp
                            | CaretUMPOp
                            | TildeCaretUMPOp
                            | CaretTildeUMPOp

data BinaryModulePathOp =   EqualEqualBMPOp
                            | ExclamationEqualBMPOp
                            | AmpersandAmpersandBMPOp
                            | PipePipeBMPOp
                            | AmpersandBMPOp
                            | PipeBMPOp
                            | CaretBMPOp
                            | CaretTildeBMPOp
                            | TildeCaretBMPOp

data Token =
            -- Alex Required
            EOF
            -- Keywords
            | AcceptOn
            | Alias
            | Always
            | AlwaysComb
            | AlwaysFf
            | AlwaysLatch
            | And
            | Assert
            | Assign
            | Assume
            | Automatic
            | Before
            | Begin
            | Bind
            | Bins
            | Binsof
            | Bit
            | Break
            | Buf
            | Bufif0
            | Bufif1
            | Byte
            | Case
            | Casex
            | Casez
            | Cell
            | Chandle
            | Checker
            | Class
            | Clocking
            | Cmos
            | Config
            | Const
            | Constraint
            | Context
            | Continue
            | Cover
            | Covergroup
            | Coverpoint
            | Cross
            | Deassign
            | Default
            | Defparam
            | Design
            | Disable
            | Dist
            | Do
            | Edge
            | Else
            | End
            | Endcase
            | Endchecker
            | Endclass
            | Endclocking
            | Endconfig
            | Endfunction
            | Endgenerate
            | Endgroup
            | Endinterface
            | Endmodule
            | Endpackage
            | Endprimitive
            | Endprogram
            | Endproperty
            | Endspecify
            | Endsequence
            | Endtable
            | Endtask
            | Enum
            | Event
            | Eventually
            | Expect
            | Export
            | Extends
            | Extern
            | Final
            | First_match
            | For
            | Force
            | Foreach
            | Forever
            | Fork
            | Forkjoin
            | Function
            | Generate
            | Genvar
            | Global
            | Highz0
            | Highz1
            | If
            | Iff
            | Ifnone
            | Ignore_bins
            | Illegal_bins
            | Implements
            | Implies
            | Import
            | Incdir
            | Include
            | Initial
            | Inout
            | Input
            | Inside
            | Instance
            | Int
            | Integer
            | Interconnect
            | Interface
            | Intersect
            | Join
            | Join_any
            | Join_none
            | Large
            | Let
            | Liblist
            | Library
            | Local
            | Localparam
            | Logic
            | Longint
            | Macromodule
            | Matches
            | Medium
            | Modport
            | Module
            | Nand
            | Negedge
            | Nettype
            | New
            | Nexttime
            | Nmos
            | Nor
            | Noshowcancelled
            | Not
            | Notif0
            | Notif1
            | Null
            | Or
            | Output
            | Package
            | Packed
            | Parameter
            | Pmos
            | Posedge
            | Primitive
            | Priority
            | Program
            | Property
            | Protected
            | Pull0
            | Pull1
            | Pulldown
            | Pullup
            | Pulsestyle_ondetect
            | Pulsestyle_onevent
            | Pure
            | Rand
            | Randc
            | Randcase
            | Randsequence
            | Rcmos
            | Real
            | Realtime
            | Ref
            | Reg
            | Reject_on
            | Release
            | Repeat
            | Restrict
            | Return
            | Rnmos
            | Rpmos
            | Rtran
            | Rtranif0
            | Rtranif1
            | S_always
            | S_eventually
            | S_nexttime
            | S_until
            | S_until_with
            | Scalared
            | Sequence
            | Shortint
            | Shortreal
            | Showcancelled
            | Signed
            | Small
            | Soft
            | Solve
            | Specify
            | Specparam
            | Static
            | String
            | Strong
            | Strong0
            | Strong1
            | Struct
            | Super
            | Supply0
            | Supply1
            | Sync_accept_on
            | Sync_reject_on
            | Table
            | Tagged
            | Task
            | This
            | Throughout
            | Time
            | Timeprecision
            | Timeunit
            | Tran
            | Tranif0
            | Tranif1
            | Tri
            | Tri0
            | Tri1
            | Triand
            | Trior
            | Trireg
            | Type
            | Typedef
            | Union
            | Unique
            | Unique0
            | Unsigned
            | Until
            | Until_with
            | Untyped
            | Use
            | Uwire
            | Var
            | Vectored
            | Virtual
            | Void
            | Wait
            | Wait_order
            | Wand
            | Weak
            | Weak0
            | Weak1
            | While
            | Wildcard
            | Wire
            | With
            | Within
            | Wor
            | Xnor
            | Xor
            | Identifier BS.ByteString
            -- Unary Operators
            | UnaryOperator UnaryOp
            -- Binary Operators
            | BinaryOperator BinaryOp
            -- INC/DEC operators
            | IncOrDecOperator IncOrDecOp
            -- UnaryModulePathOperator
            | UnaryModulePathOperator UnaryModulePathOp
            -- BinaryModulePathOperator
            | BinaryModulePathOperator BinaryModulePathOp
            -- Punctuation
            | OpenBracket
            | CloseBracket
            | OpenCurlyBracket
            | CloseCurlyBracket
            | OpenSquareBracket
            | CloseSquareBracket
            | Semicolon
            | Colon
            | FullStop
            | Asterisk
            | ColonColon
            -- Time_unit
            | TimeUnitOperator TimeUnit
            -- Numbers
            | UnsignedNumberToken UnsignedNumber
            deriving (Eq, Show)

alexEOF :: Alex RangedToken
alexEOF = do
    (pos, _, _, _) <- alexGetInput
    pure $ RangedToken EOF (Range pos pos)

data Range = Range {
    start :: AlexPosn,
    stop :: AlexPosn
} deriving (Eq, Show)

data RangedToken = RangedToken {
    rtToken :: Token,
    rtRange :: Range
} deriving (Eq, Show)

data AlexUserState = AlexUserState

alexInitUserState = AlexUserState

tokId :: AlexAction RangedToken
tokId inp@(_, _, str, _) len =
    pure RangedToken {
        rtToken = Identifier $ BS.take len str,
        rtRange = mkRange inp len
    }

mkRange :: AlexInput -> Int64 -> Range
mkRange (start, _, str, _) len = Range{start = start, stop = stop}
    where
        stop = BS.foldl' alexMove start $ BS.take len str

tok :: Token -> AlexAction RangedToken
tok ctor inp len =
    pure RangedToken {
        rtToken = ctor,
        rtRange = mkRange inp len
    }

tokDecimal inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = UnsignedNumberToken $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

}

