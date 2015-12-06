module Post where

import Data.List
import Data.Maybe
import Data.Ord
import GHC.Exts
import Text.Printf

--import Aggregate
import Etran
--import Ledger
import Types
import Utils

data Post = Post {
  postDstamp::Dstamp
  , postDr::Acc
  , postCr::Acc
  , postPennies::Pennies
  , postDec::Desc}
          deriving (Show)


postingsFromNtran ntran =
  [n1, n2]
  where
    Ntran dstamp dr cr pennies _ desc =  ntran
    n1 = Post dstamp dr cr pennies desc
    n2 = Post dstamp cr dr (negp pennies) desc
    
postingsFromNtrans  ntrans = concatMap postingsFromNtran ntrans


postingsFromEtran :: Etran -> [Post]
postingsFromEtran e =
  posts
  where
    folio = etFolio e
    ds = etDstamp e
    s = etSym e
    pCost = Post ds folio "pCost" (negp $ etAmount e) ("pCost:" ++ s) -- actual cost
    pPdp  = Post ds (folio ++ "/g") "pPdp" (negp $ etPdp e) ("pPdp:" ++ s) -- profit during period
    pVcd  = Post ds (folio ++ "/c") "pVcd" ( etVcd e) ("pVcd:" ++ s) -- value c/d
    posts1 = [pCost, pPdp, pVcd]

    pPbd2 = Post ds "opn" "pdb2" (negp $ etPbd e) ("pdb2:" ++ s) -- profit b/d

    posts = if etDuring e then posts1 else pPbd2:posts1
    


postingsFromEtrans etrans = concatMap postingsFromEtran etrans

{-
testPostings = do
  ledger <- ratl False
  let es = etrans ledger      
  let ps = postingsFromEtrans es
  printAll ps
-}

createPostings :: [Ntran] -> [Etran] -> [Post]
createPostings ntrans derivedEtrans =
  postings
  where
    ntranPostings = postingsFromNtrans ntrans
    etranPostings = postingsFromEtrans derivedEtrans
    unsortedPostings = ntranPostings ++ etranPostings
    postings = sortBy (comparing $ postDstamp) unsortedPostings
    

showPost :: Post -> String
showPost p =
  let Post dstamp dr cr pennies desc = p in
  printf "%s %4.4s %4.4s %20.20s %s" dstamp dr cr desc (show pennies) 

-- | Aggregate posts into account and date order
aggPosts :: [Post] -> [[Post]]
aggPosts posts =
  res
  where
    accOrder = sortBy (comparing $ postDr) posts
    --grp = groupOn postDr accOrder
    grp = groupWith postDr accOrder
    sortSubGroup = sortWith postDstamp -- (comparing $ postDstamp)
    res = map sortSubGroup grp
