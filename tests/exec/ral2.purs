module Main where

import Prelude
import Effect
import Effect.Console

data Pair a b = P a b
data List a = Nil | Zero (List (Pair a a)) | One a (List (Pair a a))
data Rien = Obj

cons:: forall a. a -> List a -> List a
cons x Nil       = One x Nil
cons x (Zero l)  = One x l
cons x (One y l) = Zero (cons (P x y) l)

create:: Int -> List Rien
create 0 = Nil
create n = cons Obj (create (n-1))

make:: forall a. Int -> a -> List a
make 0 _ = Nil
make n v = let l = make (n/2) (P v v) in
           if mod n 2 == 0 then Zero l else One v l

class ShowX a where
  showX :: a -> String

instance ShowX Rien where
  showX Obj = "O"


instance (ShowX a, ShowX b) => ShowX (Pair a b) where
  showX (P x y) = showX x <> "," <> showX y

instance ShowX a => ShowX (List a) where
  showX Nil       = ""
  showX (Zero l)  = "Z" <> showX l
  showX (One x l) = "O(" <> showX x <> ")" <> showX l


main :: Effect Unit
main = do log (showX (create 4))
          log (showX (create 10))
          log (showX (create 42))
          log (showX (make 4 Obj))
          log (showX (make 10 Obj))
          log (showX (make 42 Obj))

