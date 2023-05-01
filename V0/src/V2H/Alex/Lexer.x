{
module V2H.Alex.Lexer (
    Alex,
    AlexPosn (..),
    Range (..),
    RangedToken (..),
    Token (..),
    alexGetInput,
    alexError,
    runAlex,
    alexMonadScan,
    unTok,
    unTokDecimal,
    unTokTimeUnit,
    unTokIdentifier,
    tokDecimal,
    scanMany
) where
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified V2H.Ast as Ast
}
%wrapper "monadUserState-bytestring"
$alpha = [a-zA-Z]
$decimalDigit = [0-9]

@identifier = ($alpha | \_)($alpha | $decimalDigit | \_ | \$)*

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
<0> "begin"                     { tok Begin }
<0> "bit"                       { tok Bit }

<0> "edge"                      { tok Edge }
<0> "end"                       { tok End }
<0> "endmodule"                 { tok Endmodule }
<0> "input"                     { tok Input}
<0> "localparam"                { tok Localparam }
<0> "logic"                     { tok Logic }

<0> "module"                    { tok Module }
<0> "negedge"                   { tok Negedge }
<0> "output"                    { tok Output }
<0> "posedge"                   { tok Posedge }
<0> "reg"                       { tok Reg }

<0> "static"                    { tok Static }
<0> "super"                     { tok Super }
<0> "this"                      { tok This }
<0> "wire"                      { tok Wire }

<0> "@"                         { tok AtSign }
<0> "*"                         { tok Asterisk }
<0> ","                         { tok Comma }
<0> ";"                         { tok Semicolon }
<0> "("                         { tok OpenBracket }
<0> ")"                         { tok CloseBracket }
<0> "["                         { tok OpenSquareBracket }
<0> "]"                         { tok CloseSquareBracket }
<0> ":"                         { tok Colon }
<0> "="                         { tok Equal }
<0> "$root"                     { tok DollarRoot }
<0> "+:"                        { tok PlusColon }
<0> "-:"                        { tok MinusColon }
<0> "local::"                   { tok LocalColonColon }
<0> "."                         { tok FullStop }
<0> "<="                        { tok LesserEqual }
<0> @identifier                 { tokId }
<0> $decimalDigit+              { tokDecimal }
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
                | CaretTildeUOp deriving (Eq, Show)

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
                | LesserBOp
                | LesserEqualBOp
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
                | LessDashGreaterBOp deriving (Eq, Show)

data IncOrDecOp =   IncOp
                    | DecOp deriving (Eq, Show)

data UnaryModulePathOp =    ExclamationUMPOp
                            | TildeUMPOp
                            | AmpersandUMPOp
                            | TildeAmpersandUMPOp
                            | PipeUMPOp
                            | TildePipeUMPOp
                            | CaretUMPOp
                            | TildeCaretUMPOp
                            | CaretTildeUMPOp deriving (Eq, Show)

data BinaryModulePathOp =   EqualEqualBMPOp
                            | ExclamationEqualBMPOp
                            | AmpersandAmpersandBMPOp
                            | PipePipeBMPOp
                            | AmpersandBMPOp
                            | PipeBMPOp
                            | CaretBMPOp
                            | CaretTildeBMPOp
                            | TildeCaretBMPOp deriving (Eq, Show)

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
            | FirstMatch
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
            | IgnoreBins
            | IllegalBins
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
            | JoinAny
            | JoinNone
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
            | PulsestyleOndetect
            | PulsestyleOnevent
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
            | RejectOn
            | Release
            | Repeat
            | Restrict
            | Return
            | Rnmos
            | Rpmos
            | Rtran
            | Rtranif0
            | Rtranif1
            | SAlways
            | SEventually
            | SNexttime
            | SUntil
            | SUntilWith
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
            | SyncAcceptOn
            | SyncRejectOn
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
            | UntilWith
            | Untyped
            | Use
            | Uwire
            | Var
            | Vectored
            | Virtual
            | Void
            | Wait
            | WaitOrder
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
            | AtSign
            | Dollar
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
            | Apostrophe
            | ColonColon
            | Equal
            | Comma
            | Forwardslash
            | Backslash
            | Hashtag
            | PlusEqual
            | MinusEqual
            | AsteriskEqual
            | ForwardslashEqual
            | PercentageEqual
            | AmpersandEqual
            | PipeEqual
            | CaretEqual
            | CaretSymbol
            | LesserEqual
            | LesserLesserEqual
            | GreaterGreaterEqual
            | LesserLesserLesserEqual
            | GreaterGreaterGreaterEqual
            | PlusColon
            | MinusColon
            | LocalColonColon
            | DollarRoot
            | GreaterGreater
            | LesserLesser
            -- Time_unit
            | TimeUnitOperator Ast.TimeUnit
            -- Numbers
            | UnsignedNumberT Ast.UnsignedNumber
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

unTok :: RangedToken -> (Range -> Token -> a) -> a
unTok (RangedToken tok range) ctor = ctor range tok

unTokDecimal rangedToken = unTok rangedToken (\range (UnsignedNumberT num)-> num)
unTokTimeUnit rangedToken = unTok rangedToken (\range (TimeUnitOperator num)-> num)
unTokIdentifier rangedToken = unTok rangedToken (\range (Identifier str) -> BS.unpack str)

tokDecimal inp@(_, _, str, _) len =
  pure RangedToken
    { rtToken = UnsignedNumberT $ read $ BS.unpack $ BS.take len str
    , rtRange = mkRange inp len
    }

scanMany :: BS.ByteString -> Either String [RangedToken]
scanMany input = runAlex input go
  where
    go = do
      output <- alexMonadScan
      if rtToken output == EOF
        then pure [output]
        else (output :) <$> go

}

