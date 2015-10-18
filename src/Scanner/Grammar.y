{
module Scanner.Grammar where

import Scanner.Lexer
--import Scanner.Tokens

--data Recs = TkComm String
-- data PrComm = TkComm TkGraphic
data PrComm = PrComm [String]
}

%name parz
%error { parseError }
%tokentype { Token }


%token comm { TkComm }
       gra  { TkGraphic $$ }
       
%%

PrComm : comm gra { [$2] }

{
parseError :: [Token] -> a
parseError tokenList =
  error "Parse error"
{-
parseError tokenList = let pos = tokenPosn(head(tokenList)) 
  in 
  error ("parse error at " ++ show (head(tokenList))++ show(getLineNum(pos)) ++ ":" ++ show(getColumnNum(pos)))
-}

parseIt :: String -> PrComm
parseIt = parz . alexScanTokens
}
