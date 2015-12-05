{
module Lexer where

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
  @string   { \s -> tail (init s) }
  $graphic+ { id }
  $white  ;

{



}
