-------------------------------------------------------------------------
--  
--     Store.hs
--  
--         An abstract data type of stores of integers, implemented as
--         a list of pairs of variables and values.         
--                                  
--         (c) Addison-Wesley, 1996-2011.                   
--  
-------------------------------------------------------------------------

module StoreGen 
   ( Store, 
     initial,     -- Store
     value,       -- Store -> Var -> Integer
     update       -- Store -> Var -> Integer -> Store
    ) where

-- Var is the type of variables.                    

type Var = Char

-- The implementation is given by a newtype declaration, with one
-- constructor, taking an argument of type [ (Integer,Var) ].

data Store index value = Store [ (index,value) ] 

instance (Eq index, Eq value) => Eq (Store index value) where 
  (Store sto1) == (Store sto2) = (sto1 == sto2)                 

instance (Show index, Show value) => Show (Store index value) where
  show (Store sto) = show sto                 
--  
initial :: Store  index value

initial = Store []

value  :: (Eq index, Num value) => Store index value -> index -> value

value (Store []) v         = 0
value (Store ((w,n):sto)) v 
  | v==w            = n
  | otherwise       = value (Store sto) v

update  :: Store index value -> index -> value -> Store index value

update (Store sto) v n = Store ((v,n):sto)

storetest = initial
s2 = update storetest "casa" 10