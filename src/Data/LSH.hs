
module Data.LSH where

import           Data.Foldable    (foldr)
import           Data.Hashable
import           Data.List.Split
import qualified Data.LSH.MinHash as MH
import qualified Data.Map         as M
import qualified Data.Set         as S
import           Prelude          hiding (foldr,lookup)


data LSH k = LSH
    { lshBand :: Int
    , lshRow  :: Int
    , lshMH   :: MH.MinHash
    , lshDB   :: M.Map Int (S.Set k)
    }


-- | Create LSH
new :: Int  -- ^ Number of band
    -> Int  -- ^ Number of row
    -> LSH k
new band row = LSH
    { lshBand = band
    , lshRow = row
    , lshMH = MH.new (band * row)
    , lshDB = M.empty
    }


-- | Insert a row to an LSH
insert :: (Hashable a,Ord k)
       => k -> [a] -> LSH k -> LSH k
insert key value lsh = lsh { lshDB = updateLSHDB }
 where 
    updateLSHDB = foldr update (lshDB lsh) 
                $ chunksOf (lshRow lsh) (MH.mhhash value (lshMH lsh))
    update chk = M.insertWith S.union (hash chk) (S.singleton key) 

-- | Search nearest rows for given
nearest :: (Hashable a, Ord k)
           => [a]
           -> LSH k
           -> [k]
nearest value lsh = S.toList 
                  $ foldr lookup S.empty 
                  $ chunksOf (lshRow lsh) (MH.mhhash value (lshMH lsh))
  where 
    lookup chk res = case M.lookup (hash chk) (lshDB lsh) of
      Just s  -> S.union s res
      Nothing -> res
