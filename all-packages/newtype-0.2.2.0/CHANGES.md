See also https://pvp.haskell.org/faq

## 0.2.2.0 *(minor)*

 - Provide safe `Coercible`-based default-methods for `pack`/`unpack`
   ([#1](https://github.com/haskell-hvr/newtype/pull/1))

## 0.2.1.0 *(minor)*

 - Added `Newtype` instances for
     - `Data.Fixed.Fixed`
     - `Data.Functor.Compose.Compose`
     - `Data.Functor.Identity.Identity`
     - `Data.Monoid.Alt`
     - `Data.Monoid.Ap`
     - `Data.Monoid.Dual`
     - `Data.Ord.Down`
  - Declare `Control.Newtype` explicitly as `Trustworthy` under SafeHaskell
