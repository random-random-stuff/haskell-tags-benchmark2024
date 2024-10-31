module Main.Gens where

import Main.Transaction (Transaction)
import qualified Main.Transaction as Transaction
import StmHamt.Hamt (Hamt)
import qualified StmHamt.Hamt as StmHamt
import Test.QuickCheck.Gen
import Prelude hiding (choose)

key :: Gen Text
key = do
  length <- frequency [(1, pure 0), (20, pure 1), (2, pure 2), (1, pure 3)]
  chars <- vectorOf length (choose ('a', 'b'))
  return (fromString chars)

value :: Gen Int
value = choose (0, 9)

lookupTransaction :: Gen Transaction
lookupTransaction = Transaction.lookup <$> key

insertTransaction :: Gen Transaction
insertTransaction = Transaction.insert <$> key <*> value

insertWithHashTransaction :: (Text -> Int) -> Gen Transaction
insertWithHashTransaction hash = do
  keyValue <- key
  valueValue <- value
  let !hashValue = hash keyValue
   in return (Transaction.insertWithHash hashValue keyValue valueValue)

insertUsingFocusTransaction :: Gen Transaction
insertUsingFocusTransaction = Transaction.insertUsingFocus <$> key <*> value

deleteUsingFocusTransaction :: Gen Transaction
deleteUsingFocusTransaction = Transaction.deleteUsingFocus <$> key

incrementUsingAdjustFocusTransaction :: Gen Transaction
incrementUsingAdjustFocusTransaction = Transaction.incrementUsingAdjustFocus <$> key

seriesOfModificationsTransaction :: Gen Transaction
seriesOfModificationsTransaction = undefined

transaction :: Gen Transaction
transaction =
  frequency
    [ (9, lookupTransaction),
      (2, insertTransaction),
      (2, insertUsingFocusTransaction),
      (9, deleteUsingFocusTransaction),
      (9, incrementUsingAdjustFocusTransaction)
    ]

hamt :: Gen (STM (Hamt (Text, Int)))
hamt = do
  list <- keyValueList
  return $ do
    hamt <- StmHamt.new
    forM_ list $ \pair -> StmHamt.insert fst pair hamt
    return hamt

keyValueList :: Gen [(Text, Int)]
keyValueList = do
  size <- choose (0, 9)
  replicateM size ((,) <$> key <*> value)
