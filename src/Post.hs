module Post where

import Data.List
import Data.Maybe
import Data.Ord
import Text.Printf
import Data.Tuple.Select

import Aggregate
import Etran
import Ledger
--import Ntran
import Types
import Utils

-- FIXME EASY to more conventional form
data Post = Post Dstamp Acc Acc Pennies Desc deriving (Show)

postTuple (Post dstamp dr cr pennies desc) =
  (dstamp, dr, cr, pennies, desc)


postDstamp p = sel1 $ postTuple p

postDr :: Post -> Acc
postDr p = sel2 $ postTuple p

postCr :: Post -> Acc
postCr p = sel3 $ postTuple p
postPennies p = sel4 $ postTuple p
postDesc p = sel5 $ postTuple p

postingsFromNtran ntran =
  [n1, n2]
  where
    Ntran dstamp dr cr pennies _ desc =  ntran
    n1 = Post dstamp dr cr pennies desc
    n2 = Post dstamp cr dr (negp pennies) desc
    
postingsFromNtrans  ntrans = concatMap postingsFromNtran ntrans

--postingsFromEtran e = []

postingsFromEtran :: Etran -> [Post]
postingsFromEtran e =
  posts
  where
    --cost = etAmount e
    folio = etFolio e
    --de = etranDerived e
    --(odstamp, startValue, flow, profit, endValue, comm) = de
    ds = etDstamp e
    s = etSym e
    pCost = Post ds folio "pCost" (negp $ etAmount e) ("pCost:" ++ s) -- actual cost
    pPdp  = Post ds (folio ++ "/g") "pPdp" (negp $ etPdp e) ("pPdp:" ++ s) -- profit during period
    pVcd  = Post ds (folio ++ "/c") "pVcd" ( etVcd e) ("pVcd:" ++ s) -- value c/d
    posts1 = [pCost, pPdp, pVcd]

    --pPbd1 = Post ds (folio ++ "/b") "pPdb1" (negp $ etVbd e) ("pPdb1:" ++ s)
    pPbd2 = Post ds "opn" "pdb2" (negp $ etPbd e) ("pdb2:" ++ s) -- profit b/d

    posts = if etBetween e then posts1 else pPbd2:posts1
    --posts = posts1
    


postingsFromEtrans etrans = concatMap postingsFromEtran etrans


testPostings = do
  ledger <- ratl False
  let es = etrans ledger      
  let ps = postingsFromEtrans es
  printAll ps


createPostings :: [Ntran] -> [Etran] -> [Post]
createPostings ntrans derivedEtrans =
  postings
  where
    ntranPostings = postingsFromNtrans ntrans
    --derivedEtrans = deriveEtrans start comms etrans
    etranPostings = postingsFromEtrans derivedEtrans
    unsortedPostings = ntranPostings ++ etranPostings
    postings = sortBy (comparing $ postDstamp) unsortedPostings
    

--showPost :: Post -> String
showPost p =
  let (dstamp, dr, cr, pennies, desc) = postTuple p in
  printf "%s %4.4s %4.4s %20.20s %s" dstamp dr cr desc (show pennies) 

-- | Aggregate posts into account and date order
aggPosts :: [Post] -> [[Post]]
aggPosts posts =
  res
  where
    accOrder = sortBy (comparing $ postDr) posts
    grp = groupOn postDr accOrder
    sortSubGroup = sortOnMc postDstamp -- (comparing $ postDstamp)
    res = map sortSubGroup grp
