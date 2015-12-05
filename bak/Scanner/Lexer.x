{
module Scanner.Lexer where

--import Scanner.Tokens

-- https://github.com/ghulette/haskell-parser-examples
-- https://www.haskell.org/alex/doc/html/syntax.html#lexical
}

%wrapper "basic"

$digit = 0-9
$white = [\ \t \r \n]
$alpha = [a-zA-Z]
$graphic = $printable # $white

@string = \" ($printable # \")* \"



tokens :-

  "#".*   ;
  comm      { \s -> TkComm  }
  etran     { \s -> TkEtran }
  ntran     { \s -> TkNtran }
  period    { \s -> TkPeriod }
  return    { \s -> TkReturn }
  yahoo     { \s -> TkYahoo }
  @string   { \s -> TkGraphic s }
  $graphic+ { \s -> TkGraphic s }
  $white  ;

{

data Token = TkComm
     | TkEol
     | TkEtran
     | TkGraphic String
     | TkNtran
     | TkPeriod
     | TkReturn
     | TkYahoo
     deriving (Eq, Show)
              

}
