import Test.Tasty       (defaultMain,testGroup)
import qualified Tests.Chebyshev
import qualified Tests.Comparison
import qualified Tests.RootFinding
import qualified Tests.SpecFunctions
import qualified Tests.Sum

main :: IO ()
main = defaultMain $ testGroup "math-functions"
  [ Tests.SpecFunctions.tests
  -- FIXME: tests for chebyshev polynomials fail intermittently
  -- , Tests.Chebyshev.tests
  , Tests.Sum.tests
  , Tests.Comparison.tests
  , Tests.RootFinding.tests
  ]
