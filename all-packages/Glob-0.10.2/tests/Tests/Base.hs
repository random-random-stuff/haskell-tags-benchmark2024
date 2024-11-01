-- File created: 2008-10-10 22:03:00

module Tests.Base ( PString(unPS), Path(unP), COpts(unCOpts)
                  , (-->), fromRight, isRight
                  ) where

import System.FilePath (extSeparator, pathSeparators)
import Test.QuickCheck

import System.FilePath.Glob.Base (CompOptions(..))

newtype PString = PatString { unPS    :: String } deriving Show
newtype Path    = Path      { unP     :: String } deriving Show
newtype COpts   = COpts     { unCOpts :: CompOptions } deriving Show

alpha0, alpha :: String
alpha0 = extSeparator : "-^!" ++ ['a'..'z'] ++ ['0'..'9']
alpha  = pathSeparators ++ alpha0

instance Arbitrary PString where
   arbitrary = sized $ \size -> do
      let xs =
             (1, return "**/") :
             map (\(a,b) -> (a*100,b))
             [ (40, plain alpha)
             , (20, return "?")
             , (20, charRange)
             , (10, return "*")
             , (10, openRange)
             ]

      s <- mapM (const $ frequency xs) [1..size]
      return.PatString $ concat s

   shrink (PatString s) = map PatString (shrink s)

instance Arbitrary Path where
   arbitrary = sized $ \size -> do
      s <- mapM (const $ plain alpha) [1..size `mod` 16]
      return.Path $ concat s

   shrink (Path s) = map Path (shrink s)

instance Arbitrary COpts where
   arbitrary = do
      (a,b,c,d,e,f) <- arbitrary
      return.COpts $ CompOptions a b c d e f False


plain :: String -> Gen String
plain from = sized $ \size -> mapM (const $ elements from) [0..size `mod` 3]

charRange :: Gen String
charRange = do
   s <- plain alpha0
   if s `elem` ["^","!"]
      then do
         s' <- plain alpha0
         return$ "[" ++ s ++ s' ++ "]"
      else
         return$ "[" ++ s ++       "]"

openRange :: Gen String
openRange = do
   probA <- choose (0,1) :: Gen Float
   probB <- choose (0,1) :: Gen Float
   a <- if probA > 0.4
           then fmap (Just .abs) arbitrary
           else return Nothing
   b <- if probB > 0.4
           then fmap (Just .abs) arbitrary
           else return Nothing
   return.concat $
      [ "<"
      , maybe "" show (a :: Maybe Int)
      , "-"
      , maybe "" show (b :: Maybe Int)
      , ">"
      ]

-- Not in Data.Either until base-4.7 (GHC 7.8)
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _         = error "fromRight :: Left"

(-->) :: Bool -> Bool -> Bool
a --> b = not a || b
