Alex and Happy seem too complicated. I think you'd
be better off using Parsec:


import Text.ParserCombinators.Parsec

myparser::Parser String
myparser = (++ ) <$> string "HT" <*> (string "TP" <|> string "ML")

p1 :: Parser String
p1 = do
  cmd <- many1 lower
  case cmd of
   "foo" -> do { string " " ; many1 lower }
   "bar" -> return "it's a bar"
   _ -> unexpected ("command:" ++ cmd)


months = ["Jan", "Mar", "May"]


p2 ::Parser String
p2 = do  
  mth <- count 3 letter
  if elem mth months then return mth else unexpected ("Wrong: " ++ mth)


