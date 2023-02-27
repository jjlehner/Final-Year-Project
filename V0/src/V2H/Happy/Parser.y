{
module Parser
  ( parseSV
  ) where

import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Maybe (fromJust)
import Data.Monoid (First (..))

import qualified V2H.Alex.Lexer as L
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

    endmodule                   { L.RangedToken L.Endmodule _ }
    module                      { L.RangedToken L.Module _ }

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

%%
many_rev(p)
    :               { [] }
    | many_rev(p) p { $2 : $1 }

many(p)
    : many_rev(p) { reverse $1 }

optional(p)
    :   { Nothing }
    | p { Just $1 }

source_text :: { SourceText }
    : optional(timeunits_declaration) many(description) {SourceText $1 $2}


description :: { Description }
    : module_declaration { DModuleDeclaration $1 }

