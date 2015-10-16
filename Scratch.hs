--{-# LANGUAGE TemplateHaskell #-}

--import Debug.Trace
--{-# OPTIONS_GHC -prof #-}


data Caller a = Result a | Message String | Fail deriving (Show)


instance Functor Caller where
  fmap f (Result x) = Result (f x)
  fmap _ (Message x) = Message x
  fmap _ Fail = Fail

instance Applicative Caller where
  pure = Result
  (Result f) <*> (Result x) = Result (f x)
  

{-
instance Functor (Caller a) where
  fmap _ (Msg x) 
-}

{-
instance Monad Caller where
  return x = Caller "" x
  Msg x >>= _ = Msg x
  Call x >>= _ = Call x
-}
