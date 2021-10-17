#!/usr/bin/env stack
-- stack --package mtl monadic-dfas


import Control.Monad.Identity


data FA m q s = FA (q -> s -> m q) q
-- interp. type. A finite automata in the monad M with 
-- state type q 
-- symbol type s
--
-- interp. ctor. A 
-- state transition function delta 
-- current (or initial) state q


-- Total DFA
type DFA q s = FA Identity q s

-- Partial DFA
type PFA q s = FA Maybe q s

-- Nondeterministic FA
type NFA q s = FA [] q s







-- One function to run them all
run_FA :: (Monad m) => FA m q s -> [s] -> m q
run_FA (FA delta q0) ss = foldM delta q0 ss


-- Example DFA: Increment N if true
incrementor :: DFA Int Bool 
incrementor = FA inc_f 0 
    where inc_f x False = return x
          inc_f x True  = return (x+1)


-- Because the monad associated by a DFA is the identity monad 
-- it's easy to translate any monadic FA into a DFA

-- The NFA to DFA transformation
fa_to_dfa :: (Monad m) => FA m q s -> DFA (m q) s
fa_to_dfa (FA delta q0) = FA delta' (return q0)
    where
        delta' mq s = return $ mq >>= ((flip delta) s)


-- The transformation follows the law
-- run_FA (fa_to_dfa f) (pure s)  = pure $ run_FA f s
-- It is a natural mmonad transformation



-- NFA's also emulate PFA's, so we can write a transformation for that too.

-- Example NFA 
-- Partial: 7's map to nothing
--          5's split into {6, 
incrementor_no_7s :: NFA Int Bool
incrementor_no_7s = FA inc_f 1
    where inc_f x False     = [x]
          inc_f x True      = [] ++ h5 x ++ ge x
        
          h5 x | x `mod` 7 == 0     = [] 
               | x `mod` 5 == 0     = [x+6]
               | otherwise          = []
        
          ge x | x `mod` 7 == 0     = []
               | otherwise          = [x+1]







-- Natural transformations?
-- Monad morphisms?
eta_id_maybe :: Identity a -> Maybe a
eta_id_maybe = return . runIdentity

eta_id_list :: Identity a -> [a]
eta_id_list = return . runIdentity

eta_maybe_list :: Maybe a -> [a]
eta_maybe_list Nothing  = []
eta_maybe_list (Just x) = [x]
