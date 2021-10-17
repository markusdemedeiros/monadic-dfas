# Monadic DFAs

In my "Theory of Computation" class we introduced the NFA to DFA construction, where an NFA is representable as a DFA on the power set of states. This transformation intrigued me, since we Haskell programmers know that the  ``List`` monad models nondetermism. 

Let us define a *monadic finite automata* (abbreviated FA) as 
```
data FA m q s = FA (q -> s -> m q) q
```
and we can define the evaluation of a FA as a monadic fold
```
run_FA :: (Monad m) => FA m q s -> [s] -> m q
run_FA (FA delta q0) ss = foldM delta q0 ss
```
The choice of model appears to make this model very general. Using the ``Identity`` monad gives us a regular, total DFA. The ``[]`` monad gives us a NFA. A partial DFA is given by the ``Maybe`` monad. 

Interestingly, we can generalize the NFA to DFA transformation to work with any FA under this framework. Under this general construction the state type becomes the type of lifted states, and the transition functio amounts to a bind. 
```
fa_to_dfa :: (Monad m) => FA m q s -> DFA (m q) s 
fa_to_dfa (FA delta q0) = FA delta' (return q0)     
    where delta' mq s = return $ mq >>= ((flip delta) s)
```
In fact, performing this operation on a FA in the ``Maybe`` monad yields a structure that's isomorphic to the standard completion procedure for partial DFA's: adding a finalizing state to which all 

Perhaps we can generalize this further. If my understanding is correct, every monad has a natrual transformation into the identity monad over it's Kleisli category. Can we write a general emulation function for FA's between which there is a natural transformation? 

What other FA's can be represented under this model? How powerful can we make this monad? How do accepting functions on FA's transform when you change the monad? There are many questions still to answer. 
