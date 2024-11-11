---------------------------------------------------------
--       WARNING: THIS PROGRAM CONTAINS A BUG!!!       --
--                                                     --
--  This program belongs to the faulty nofib library   --
--      and contains a bug to benchmark debuggers      --
--                                                     --
---------------------------------------------------------
--                                                     --
--  "The faulty nofib library" is a  collection of     --
--  Haskell programs from the 'nofib' benchmark suite  --
--                                                     --
--  Faults are always marked with a comment: "BUG"     --
--  The commented correct line appears after the       --
--  faulty line marked with "CORRECT"                  --
--                                                     --
--  We welcome any comment or improvement about        --
--  bugs. You can send them to:                        --
--        Josep Silva (jsilva@dsic.upv.es)             --
--                                                     --
---------------------------------------------------------
--                                                     --
--  There are three kinds of bugs depending on their   --
--  consequences:                                      --
--  1) Bugs that produce an incorrect result           --
--  2) Bugs that produce non-termination               --
--  3) Bugs that produce an exception (e.g. div by 0)  --
--                                                     --
--  This program contains a bug of tipe 3.	       --
--   Stack space overflow: current size 8388608 bytes. --
---------------------------------------------------------

module InferMonad     (Infer, returnI, eachI, thenI, guardI, useI, getSubI,
                       substituteI, unifyI, freshI, freshesI)
                      where

import MaybeM         (Maybe, returnM, eachM, thenM, failM, guardM, theM, existsM, useM)
import StateX         (StateX, returnSX, eachSX, thenSX, toSX, putSX, getSX, useSX)
import Type           (TVarId, TConId, MonoType (TVar, TCon), freeTVarMono)
import Substitution   (Sub, applySub, lookupSub, emptySub, extendSub, domSub, unifySub)
type  Counter         =  Int
data  Infer x         =  MkI (StateX Sub (StateX Counter (Maybe ((x, Sub), Counter))))
rep (MkI xJ)          =  xJ
returnI               :: x -> Infer x
returnI x             =  MkI (returnSX (returnSX returnM) x)
eachI                 :: Infer x -> (x -> y) -> Infer y
xI `eachI` f          =  MkI (eachSX (eachSX eachM) (rep xI) f)
thenI                 :: Infer x -> (x -> Infer y) -> Infer y
xI `thenI` kI         =  MkI (thenSX (thenSX thenM) (rep xI) (rep . kI))
failI                 :: Infer x
failI                 =  MkI (toSX (eachSX eachM) (toSX eachM failM))
useI                  :: x -> Infer x -> x
useI xfail            =  useM xfail
                      .  useSX eachM 0
                      .  useSX (eachSX eachM) emptySub
                      .  rep
guardI                :: Bool -> Infer x -> Infer x
guardI b xI           =  if  b  then  xI  else  failI
putSubI               :: Sub -> Infer ()
putSubI s             =  MkI (putSX (returnSX returnM) s)
getSubI               :: Infer Sub
getSubI               =  MkI (getSX (returnSX returnM))
putCounterI           :: Counter -> Infer ()
putCounterI c         =  MkI (toSX (eachSX eachM) (putSX returnM c))
getCounterI           :: Infer Counter
getCounterI           =  MkI (toSX (eachSX eachM) (getSX returnM))
substituteI           :: MonoType -> Infer MonoType
substituteI t         =  getSubI              `thenI`  (\ s ->
                                              returnI  (applySub s t))
unifyI                :: MonoType -> MonoType -> Infer ()
unifyI t u            =  getSubI              `thenI`  (\ s  ->
			 let sM = unifySub t u s
			 in
                         existsM sM           `guardI` (
                         putSubI (theM sM)    `thenI`  (\ () ->
                                              returnI  ())))
freshI                :: Infer MonoType
freshI                =  getCounterI          `thenI` (\c  ->
                         putCounterI (c+1)    `thenI` (\() ->
                                              returnI (TVar ("a" ++ show c))))
freshesI              :: Int -> Infer [MonoType]
freshesI 0            =                       returnI []
freshesI n            =  freshI               `thenI` (\x  ->
	-- BUG: The following line contains a bug
                         freshesI (n+1)       `thenI` (\xs ->
	-- CORRECT-- freshesI (n-1)       `thenI` (\xs ->
                                              returnI (x:xs)))
