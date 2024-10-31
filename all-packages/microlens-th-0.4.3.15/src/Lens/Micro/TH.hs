{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706)
#endif

#ifndef MIN_VERSION_containers
#define MIN_VERSION_containers(x,y,z) 1
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE TemplateHaskellQuotes #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif


{- |
Module      :  Lens.Micro.TH
Copyright   :  (C) 2013-2016 Eric Mertens, Edward Kmett, Artyom Kazak; 2018 Monadfix
License     :  BSD-style (see the file LICENSE)
-}
module Lens.Micro.TH
(
  -- * Dealing with “not in scope” errors
  -- $errors-note

  -- * Using this module in GHCi
  -- $ghci-note

  -- * 'SimpleGetter' and 'SimpleFold'
  -- $getter-fold-note

  -- * Generating lenses
  makeLenses,
  makeLensesFor,
  makeLensesWith,
  makeFields,
  makeClassy,

  -- * Default lens rules
  LensRules,
  DefName(..),
  lensRules,
  lensRulesFor,
  classyRules,
  camelCaseFields,
  abbreviatedFields,

  -- * Configuring lens rules
  lensField,
  lensClass,
  createClass,
  simpleLenses,
  generateSignatures,
  generateUpdateableOptics,
  generateLazyPatterns,
)
where


import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Data
import           Data.Either
import qualified Data.Map as Map
import           Data.Map (Map)
import qualified Data.Set as Set
import           Data.Set (Set)
import           Data.List (nub, findIndices, stripPrefix, isPrefixOf)
import           Data.Maybe
import           Lens.Micro
import           Lens.Micro.Internal (phantom)
import           Lens.Micro.TH.Internal
import           Language.Haskell.TH
import qualified Language.Haskell.TH.Datatype as D
import qualified Language.Haskell.TH.Datatype.TyVarBndr as D

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative
import           Data.Traversable (traverse, sequenceA)
#endif


{- $errors-note

When you use Template Haskell, the order of declarations suddenly starts to matter. For instance, if you try to use 'makeLenses', 'makeFields', etc before the type is defined, you'll get a “not in scope” error:

@
'makeLenses' ''Foo

data Foo = Foo {_foo :: Int}
@

@
Not in scope: type constructor or class ‘Foo’ …
    In the Template Haskell quotation ''Foo
@

You can't refer to generated lenses before you call 'makeLenses', either:

@
data Foo = Foo {_foo :: Int}

bar :: Lens' Foo Int
bar = foo

'makeLenses' ''Foo
@

@
Not in scope: ‘foo’ …
    Perhaps you meant one of these:
      data constructor ‘Foo’ (line 1), ‘_foo’ (line 1)
@
-}

{- $ghci-note

You can use 'makeLenses' and friends to define lenses right from GHCi, but it's slightly tricky.

First, enable Template Haskell:

>>> :set -XTemplateHaskell

Then define a bogus type (you can use any name in place of @M@, and you can use the same name many times), and follow the definition by the actual Template Haskell command you want to use:

>>> data M; makeLenses ''Foo

This will generate lenses for @Foo@ and you'll be able to use them from GHCi.

If you want, you can define the type and lenses for it simultaneously with @:{@ and @:}@:

@
>>> :{
data Foobar = Foobar {
  _foo :: Int,
  _bar :: Bool }
  deriving (Eq, Show)

makeLenses ''Foobar
:}
@
-}

{- $getter-fold-note

When updates are forbidden (by using 'generateUpdateableOptics'), or when a field simply can't be updated (for instance, in the presence of @forall@), instead of 'Lens' and 'Traversal' we generate 'SimpleGetter' and 'SimpleFold'. These aren't true @Getter@ and @Fold@ from lens, so beware. (Still, they're compatible, it's just that you can't do some things with them that you can do with original ones – for instance, @backwards@ and @takingWhile@ don't work on 'SimpleFold'.)

If you want to export true folds, it's recommended that you depend on <http://hackage.haskell.org/package/microlens-contra microlens-contra>, use 'makeLensesFor' to generate 'SimpleFold's with prefixes, and then export versions of those folds with @<http://hackage.haskell.org/package/microlens-contra/docs/Lens-Micro-Contra.html#v:fromSimpleFold fromSimpleFold>@ applied.
-}

-- Utilities

-- like 'rewrite' from uniplate
rewrite :: (Data a, Data b) => (a -> Maybe a) -> b -> b
rewrite f mbA = case cast mbA of
  Nothing -> gmapT (rewrite f) mbA
  Just a  -> let a' = gmapT (rewrite f) a
             in  fromJust . cast $ fromMaybe a' (f a')

-- like 'children' from uniplate
children :: Data a => a -> [a]
children = catMaybes . gmapQ cast

-- Control.Lens.TH

{- |
Generate lenses for a data type or a newtype.

To use it, you have to enable Template Haskell first:

@
\{\-\# LANGUAGE TemplateHaskell \#\-\}
@

Then, after declaring the datatype (let's say @Foo@), add @makeLenses ''Foo@ on a separate line (if you do it before the type is declared, you'll get a “not in scope” error – see the section at the top of this page):

@
data Foo = Foo {
  _x :: Int,
  _y :: Bool }

'makeLenses' ''Foo
@

This would generate the following lenses, which can be used to access the fields of @Foo@:

@
x :: 'Lens'' Foo Int
x f foo = (\\x' -> foo {_x = x'}) '<$>' f (_x foo)

y :: 'Lens'' Foo Bool
y f foo = (\\y' -> foo {_y = y'}) '<$>' f (_y foo)
@

(If you don't want a lens to be generated for some field, don't prefix it with “_”.)

If you want to create lenses for many types, you can do it all in one place like this (of course, instead you just can use 'makeLenses' several times if you feel it would be more readable):

@
data Foo = ...
data Bar = ...
data Quux = ...

'concat' '<$>' 'mapM' 'makeLenses' [''Foo, ''Bar, ''Quux]
@

When the data type has type parameters, it's possible for a lens to do a polymorphic update – i.e. change the type of the thing along with changing the type of the field. For instance, with this type

@
data Foo a = Foo {
  _x :: a,
  _y :: Bool }
@

the following lenses would be generated:

@
x :: 'Lens' (Foo a) (Foo b) a b
y :: 'Lens'' (Foo a) Bool
@

However, when there are several fields using the same type parameter, type-changing updates are no longer possible:

@
data Foo a = Foo {
  _x :: a,
  _y :: a }
@

generates

@
x :: 'Lens'' (Foo a) a
y :: 'Lens'' (Foo a) a
@

Finally, when the type has several constructors, some of fields may not be /always/ present – for those, a 'Traversal' is generated instead. For instance, in this example @y@ can be present or absent:

@
data FooBar
  = Foo { _x :: Int, _y :: Bool }
  | Bar { _x :: Int }
@

and the following accessors would be generated:

@
x :: 'Lens'' FooBar Int
y :: 'Traversal'' FooBar Bool
@

So, to get @_y@, you'd have to either use ('^?') if you're not sure it's there, or ('^?!') if you're absolutely sure (and if you're wrong, you'll get an exception). Setting and updating @_y@ can be done as usual.
-}
makeLenses :: Name -> DecsQ
makeLenses = makeFieldOptics lensRules

{- |
Like 'makeLenses', but lets you choose your own names for lenses:

@
data Foo = Foo {foo :: Int, bar :: Bool}

'makeLensesFor' [(\"foo\", \"fooLens\"), (\"bar\", \"_bar\")] ''Foo
@

would create lenses called @fooLens@ and @_bar@. This is useful, for instance, when you don't want to prefix your fields with underscores and want to prefix /lenses/ with underscores instead.

If you give the same name to different fields, it will generate a 'Traversal' instead:

@
data Foo = Foo {slot1, slot2, slot3 :: Int}

'makeLensesFor' [(\"slot1\", \"slots\"),
               (\"slot2\", \"slots\"),
               (\"slot3\", \"slots\")] ''Foo
@

would generate

@
slots :: 'Traversal'' Foo Int
slots f foo = Foo '<$>' f (slot1 foo)
                  '<*>' f (slot2 foo)
                  '<*>' f (slot3 foo)
@
-}
makeLensesFor :: [(String, String)] -> Name -> DecsQ
makeLensesFor fields = makeFieldOptics (lensRulesFor fields)

{- |
Generate lenses with custom parameters.

To see what exactly you can customise, look at the “Configuring lens rules” section. Usually you would build upon the 'lensRules' configuration, which is used by 'makeLenses':

@
'makeLenses' = 'makeLensesWith' 'lensRules'
@

Here's an example of generating lenses that would use lazy patterns:

@
data Foo = Foo {_x, _y :: Int}

'makeLensesWith' ('lensRules' '&' 'generateLazyPatterns' '.~' True) ''Foo
@

When there are several modifications to the rules, the code looks nicer when you use 'flip':

@
'flip' 'makeLensesWith' ''Foo $
  'lensRules'
    '&' 'generateLazyPatterns' '.~' True
    '&' 'generateSignatures'   '.~' False
@
-}
makeLensesWith :: LensRules -> Name -> DecsQ
makeLensesWith = makeFieldOptics

{- |
Generate overloaded lenses.

This lets you deal with several data types having same fields. For instance, let's say you have @Foo@ and @Bar@, and both have a field named @x@. To avoid those fields clashing, you would have to use prefixes:

@
data Foo a = Foo {
  fooX :: Int,
  fooY :: a }

data Bar = Bar {
  barX :: Char }
@

However, if you use 'makeFields' on both @Foo@ and @Bar@ now, it would generate lenses called @x@ and @y@ – and @x@ would be able to access both @fooX@ and @barX@! This is done by generating a separate class for each field, and making relevant types instances of that class:

@
class HasX s a | s -> a where
  x :: 'Lens'' s a

instance HasX (Foo a) Int where
  x :: 'Lens'' (Foo a) Int
  x = ...

instance HasX Bar Char where
  x :: 'Lens'' Bar Char
  x = ...


class HasY s a | s -> a where
  y :: 'Lens'' s a

instance HasY (Foo a) a where
  y :: 'Lens'' (Foo a) a
  y = ...
@

(There's a minor drawback, though: you can't perform type-changing updates with these lenses.)

If you only want to make lenses for some fields, you can prefix them with underscores – the rest would be untouched. If no fields are prefixed with underscores, lenses would be created for all fields.

The prefix must be the same as the name of the name of the data type (/not/ the constructor). If you don't like this behavior, use @'makeLensesWith' 'abbreviatedFields'@ – it allows any prefix (and even different prefixes).

If you want to use 'makeFields' on types declared in different modules, you can do it, but then you would have to export the @Has*@ classes from one of the modules – 'makeFields' creates a class if it's not in scope yet, so the class must be in scope or else there would be duplicate classes and you would get an “Ambiguous occurrence” error.

Finally, 'makeFields' is implemented as @'makeLensesWith' 'camelCaseFields'@, so you can build on 'camelCaseFields' if you want to customise behavior of 'makeFields'.
-}
makeFields :: Name -> DecsQ
makeFields = makeFieldOptics camelCaseFields

{- |
Generate overloaded lenses without ad-hoc classes; useful when there's a collection of fields that you want to make common for several types.

Like 'makeFields', each lens is a member of a class. However, the classes are per-type and not per-field. Let's take the following type:

@
data Person = Person {
  _name :: String,
  _age :: Double }
@

'makeClassy' would generate a single class with 3 methods:

@
class HasPerson c where
  person :: Lens' c Person

  age :: Lens' c Double
  age = person.age

  name :: Lens' c String
  name = person.name
@

And an instance:

@
instance HasPerson Person where
  person = id

  name = ...
  age = ...
@

So, you can use @name@ and @age@ to refer to the @_name@ and @_age@ fields, as usual. However, the extra lens – @person@ – allows you to do a kind of subtyping. Let's say that there's a type called @Worker@ and every worker has the same fields that a person has, but also a @job@. If you were using 'makeFields', you'd do the following:

@
data Worker = Worker {
  _workerName :: String,
  _workerAge :: Double,
  _workerJob :: String }
@

However, with 'makeClassy' you can say “every worker is a person” in a more principled way:

@
data Worker = Worker {
  _workerPerson :: Person,
  _job :: String }

makeClassy ''Worker

instance HasPerson Worker where person = workerPerson
@

Now you can use @age@ and @name@ to access name\/age of a @Worker@, but you also can use @person@ to “downgrade” a @Worker@ to a @Person@ (and e.g. apply some @Person@-specific function to it).

Unlike 'makeFields', 'makeClassy' doesn't make use of prefixes. @_workerPerson@ could've just as well been named @_foobar@.

'makeClassy' is implemented as @'makeLensesWith' 'classyRules'@, so you can build on 'classyRules' if you want to customise behavior of 'makeClassy'.
-}
makeClassy :: Name -> DecsQ
makeClassy = makeFieldOptics classyRules

{- |
Generate simple (monomorphic) lenses even when type-changing lenses are possible – i.e. 'Lens'' instead of 'Lens' and 'Traversal'' instead of 'Traversal'. Just in case, here's an example of a situation when type-changing lenses would be normally generated:

@
data Foo a = Foo { _foo :: a }
@

Generated lens:

@
foo :: 'Lens' (Foo a) (Foo b) a b
@

Generated lens with 'simpleLenses' turned on:

@
foo :: 'Lens'' (Foo a) a
@

This option is disabled by default.
-}
simpleLenses :: Lens' LensRules Bool
simpleLenses f r = fmap (\x -> r { _simpleLenses = x}) (f (_simpleLenses r))

{- |
Supply type signatures for the generated lenses.

This option is enabled by default. Disable it if you want to write the signature by yourself – for instance, if the signature should be more restricted, or if you want to write haddocks for the lens (as haddocks are attached to the signature and not to the definition).
-}
generateSignatures :: Lens' LensRules Bool
generateSignatures f r =
  fmap (\x -> r { _generateSigs = x}) (f (_generateSigs r))

{- |
Generate “updateable” optics. When turned off, 'SimpleFold's will be generated instead of 'Traversal's and 'SimpleGetter's will be generated instead of 'Lens'es.

This option is enabled by default. Disabling it can be useful for types with invariants (also known as “types with smart constructors”) – if you generate updateable optics, anyone would be able to use them to break your invariants.
-}
generateUpdateableOptics :: Lens' LensRules Bool
generateUpdateableOptics f r =
  fmap (\x -> r { _allowUpdates = x}) (f (_allowUpdates r))

{- |
Generate lenses using lazy pattern matches. This can allow fields of an undefined value to be initialized with lenses:

@
data Foo = Foo {_x :: Int, _y :: Bool}
  deriving Show

'makeLensesWith' ('lensRules' '&' 'generateLazyPatterns' '.~' True) ''Foo
@

@
>>> 'undefined' '&' x '.~' 8 '&' y '.~' True
Foo {_x = 8, _y = True}
@

(Without 'generateLazyPatterns', the result would be just 'undefined'.)

This option is disabled by default. The downside of enabling it is that it can lead to space-leaks and code-size\/compile-time increases when lenses are generated for large records.

When you have a lazy lens, you can get a strict lens from it by composing with ('$!'):

@
strictLens = ('$!') . lazyLens
@
-}
generateLazyPatterns :: Lens' LensRules Bool
generateLazyPatterns f r =
  fmap (\x -> r { _lazyPatterns = x}) (f (_lazyPatterns r))

{- |
This lets you choose which fields would have lenses generated for them and how would those lenses be called. To do that, you provide a function that would take a field name and output a list (possibly empty) of lenses that should be generated for that field.

Here's the full type of the function you have to provide:

@
'Name' ->     -- The datatype lenses are being generated for
['Name'] ->   -- A list of all fields of the datatype
'Name' ->     -- The current field
['DefName']   -- A list of lens names
@

Most of the time you won't need the first 2 parameters, but sometimes they are useful – for instance, the list of all fields would be useful if you wanted to implement a slightly more complicated rule like “if some fields are prefixed with underscores, generate lenses for them, but if no fields are prefixed with underscores, generate lenses for /all/ fields”.

As an example, here's a function used by default. It strips “_” off the field name, lowercases the next character after “_”, and skips the field entirely if it doesn't start with “_”:

@
\\_ _ n ->
  case 'nameBase' n of
    \'_\':x:xs -> ['TopName' ('mkName' ('toLower' x : xs))]
    _        -> []
@

You can also generate classes (i.e. what 'makeFields' does) by using @'MethodName' className lensName@ instead of @'TopName' lensName@.
-}
lensField :: Lens' LensRules (Name -> [Name] -> Name -> [DefName])
lensField f r = fmap (\x -> r { _fieldToDef = x}) (f (_fieldToDef r))

{- |
This lets you choose whether a class would be generated for the type itself (like 'makeClassy' does). If so, you can choose the name of the class and the name of the type-specific lens.

For 'makeLenses' and 'makeFields' this is just @const Nothing@. For 'makeClassy' this function is defined like this:

@
\\n ->
  case 'nameBase' n of
    x:xs -> Just ('mkName' ("Has" ++ x:xs), 'mkName' ('toLower' x : xs))
    []   -> Nothing
@
-}
lensClass :: Lens' LensRules (Name -> Maybe (Name, Name))
lensClass f r = fmap (\x -> r { _classyLenses = x }) (f (_classyLenses r))

{- |
Decide whether generation of classes is allowed at all.

If this is disabled, neither 'makeFields' nor 'makeClassy' would work, regardless of values of 'lensField' or 'lensClass'. On the other hand, if 'lensField' and 'lensClass' don't generate any classes, enabling this won't have any effect.
-}
createClass :: Lens' LensRules Bool
createClass f r =
  fmap (\x -> r { _generateClasses = x}) (f (_generateClasses r))

{- |
Lens rules used by default (i.e. in 'makeLenses'):

* 'generateSignatures' is turned on
* 'generateUpdateableOptics' is turned on
* 'generateLazyPatterns' is turned off
* 'simpleLenses' is turned off
* 'lensField' strips “_” off the field name, lowercases the next character after “_”, and skips the field entirely if it doesn't start with “_” (you can see how it's implemented in the docs for 'lensField')
* 'lensClass' isn't used (i.e. defined as @const Nothing@)
-}
lensRules :: LensRules
lensRules = LensRules
  { _simpleLenses    = False
  , _generateSigs    = True
  , _generateClasses = False
  -- , _allowIsos       = True
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = \_ _ n ->
       case nameBase n of
         '_':x:xs -> [TopName (mkName (toLower x:xs))]
         _        -> []
  }

{- |
A modification of 'lensRules' used by 'makeLensesFor' (the only difference is that a simple lookup function is used for 'lensField').
-}
lensRulesFor
  :: [(String, String)] -- ^ @[(fieldName, lensName)]@
  -> LensRules
lensRulesFor fields = lensRules & lensField .~ mkNameLookup fields

mkNameLookup :: [(String,String)] -> Name -> [Name] -> Name -> [DefName]
mkNameLookup kvs _ _ field =
  [ TopName (mkName v) | (k,v) <- kvs, k == nameBase field]

{- |
Lens rules used by 'makeFields':

* 'generateSignatures' is turned on
* 'generateUpdateableOptics' is turned on
* 'generateLazyPatterns' is turned off
* 'simpleLenses' is turned on (unlike in 'lensRules')
* 'lensField' is more complicated – it takes fields which are prefixed with the name of the type they belong to (e.g. “fooFieldName” for “Foo”), strips that prefix, and generates a class called “HasFieldName” with a single method called “fieldName”. If some fields are prefixed with underscores, underscores would be stripped too, but then fields without underscores won't have any lenses generated for them. Also note that e.g. “foolish” won't have a lens called “lish” generated for it – the prefix must be followed by a capital letter (or else it wouldn't be camel case).
* 'lensClass' isn't used (i.e. defined as @const Nothing@)
-}
camelCaseFields :: LensRules
camelCaseFields = defaultFieldRules

camelCaseNamer :: Name -> [Name] -> Name -> [DefName]
camelCaseNamer tyName fields field = maybeToList $ do

  fieldPart <- stripPrefix expectedPrefix (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  expectedPrefix = optUnderscore ++ over _head toLower (nameBase tyName)

  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing

{- |
Like standard rules used by 'makeFields', but doesn't put any restrictions on the prefix. I.e. if you have fields called

* @_fooBarBaz@
* @_someX@
* @someY@

then the generated lenses would be called @barBaz@ and @x@.
-}
abbreviatedFields :: LensRules
abbreviatedFields = defaultFieldRules { _fieldToDef = abbreviatedNamer }

abbreviatedNamer :: Name -> [Name] -> Name -> [DefName]
abbreviatedNamer _ fields field = maybeToList $ do

  fieldPart <- stripMaxLc (nameBase field)
  method    <- computeMethod fieldPart
  let cls = "Has" ++ fieldPart
  return (MethodName (mkName cls) (mkName method))

  where
  stripMaxLc f = do x <- stripPrefix optUnderscore f
                    case break isUpper x of
                      (p,s) | null p || null s -> Nothing
                            | otherwise        -> Just s
  optUnderscore  = ['_' | any (isPrefixOf "_" . nameBase) fields ]

  computeMethod (x:xs) | isUpper x = Just (toLower x : xs)
  computeMethod _                  = Nothing

defaultFieldRules :: LensRules
defaultFieldRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True  -- classes will still be skipped if they already exist
  -- , _allowIsos       = False -- generating Isos would hinder field class reuse
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = const Nothing
  , _fieldToDef      = camelCaseNamer
  }

underscoreNoPrefixNamer :: Name -> [Name] -> Name -> [DefName]
underscoreNoPrefixNamer _ _ n =
  case nameBase n of
    '_':x:xs -> [TopName (mkName (toLower x:xs))]
    _        -> []

{- |
Lens rules used by 'makeClassy':

* 'generateSignatures' is turned on
* 'generateUpdateableOptics' is turned on
* 'generateLazyPatterns' is turned off
* 'simpleLenses' is turned on (unlike in 'lensRules')
* 'lensField' is the same as in 'lensRules'
* 'lensClass' just adds “Has” to the name of the type (so for “Person” the generated class would be called “HasPerson” and the type-specific lens in that class would be called “person”)
-}
classyRules :: LensRules
classyRules = LensRules
  { _simpleLenses    = True
  , _generateSigs    = True
  , _generateClasses = True
  -- , _allowIsos       = False -- generating Isos would hinder "subtyping"
  , _allowUpdates    = True
  , _lazyPatterns    = False
  , _classyLenses    = \n ->
        case nameBase n of
          x:xs -> Just (mkName ("Has" ++ x:xs), mkName (toLower x:xs))
          []   -> Nothing
  , _fieldToDef      = underscoreNoPrefixNamer
  }

-- FieldTH.hs

------------------------------------------------------------------------
-- Field generation entry point
------------------------------------------------------------------------


-- Compute the field optics for the type identified by the given type name.
-- Lenses will be computed when possible, Traversals otherwise.
makeFieldOptics :: LensRules -> Name -> DecsQ
makeFieldOptics rules = (`evalStateT` Set.empty) . makeFieldOpticsForDatatype rules <=< D.reifyDatatype

type HasFieldClasses = StateT (Set Name) Q

addFieldClassName :: Name -> HasFieldClasses ()
addFieldClassName n = modify $ Set.insert n

-- | Compute the field optics for a deconstructed datatype Dec
-- When possible build an Iso otherwise build one optic per field.
makeFieldOpticsForDatatype :: LensRules -> D.DatatypeInfo -> HasFieldClasses [Dec]
makeFieldOpticsForDatatype rules info =
  do perDef <- liftState $ do
       fieldCons <- traverse normalizeConstructor cons
       let allFields  = toListOf (folded . _2 . folded . _1 . folded) fieldCons
       let defCons    = over normFieldLabels (expandName allFields) fieldCons
           allDefs    = setOf (normFieldLabels . folded) defCons
       sequenceA (Map.fromSet (buildScaffold rules s defCons) allDefs)

     let defs = Map.toList perDef
     case _classyLenses rules tyName of
       Just (className, methodName) ->
         makeClassyDriver rules className methodName s defs
       Nothing -> do decss <- traverse (makeFieldOptic rules) defs
                     return (concat decss)

  where
  tyName = D.datatypeName     info
  s      = datatypeTypeKinded info
  cons   = D.datatypeCons     info

  -- Traverse the field labels of a normalized constructor
  normFieldLabels :: Traversal [(Name,[(a,Type)])] [(Name,[(b,Type)])] a b
  normFieldLabels = traverse . _2 . traverse . _1

  -- Map a (possibly missing) field's name to zero-to-many optic definitions
  expandName :: [Name] -> Maybe Name -> [DefName]
  expandName allFields = concatMap (_fieldToDef rules tyName allFields) . maybeToList

normalizeConstructor ::
  D.ConstructorInfo ->
  Q (Name, [(Maybe Name, Type)]) -- ^ constructor name, field name, field type

normalizeConstructor con =
  return (D.constructorName con,
          zipWith checkForExistentials fieldNames (D.constructorFields con))
  where
    fieldNames =
      case D.constructorVariant con of
        D.RecordConstructor xs -> fmap Just xs
        D.NormalConstructor    -> repeat Nothing
        D.InfixConstructor     -> repeat Nothing

    -- Fields mentioning existentially quantified types are not
    -- elligible for TH generated optics.
    checkForExistentials _ fieldtype
      | any (\tv -> D.tvName tv `Set.member` used) unallowable
      = (Nothing, fieldtype)
      where
        used        = setOf typeVars fieldtype
        unallowable = D.constructorVars con
    checkForExistentials fieldname fieldtype = (fieldname, fieldtype)

makeClassyDriver ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  HasFieldClasses [Dec]
makeClassyDriver rules className methodName s defs = sequenceA (cls ++ inst)

  where
  cls | _generateClasses rules = [liftState $ makeClassyClass className methodName s defs]
      | otherwise = []

  inst = [makeClassyInstance rules className methodName s defs]

makeClassyClass ::
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  DecQ
makeClassyClass className methodName s defs = do
  let ss   = map (stabToS . (^. _2._2)) defs
  (sub,s') <- unifyTypes (s : ss)
  c <- newName "c"
  let vars     = D.freeVariablesWellScoped [s']
      varNames = map D.tvName vars
      fd   | null vars = []
           | otherwise = [FunDep [c] varNames]

#if MIN_VERSION_template_haskell(2,21,0)
  classD (cxt[]) className (D.plainTV c : D.changeTVFlags D.defaultBndrFlag vars) fd
#else
  classD (cxt[]) className (D.plainTV c:vars) fd
#endif
    $ sigD methodName (return (''Lens' `conAppsT` [VarT c, s']))
    : concat
      [ [sigD defName (return ty)
        ,valD (varP defName) (normalB body) []
        ] ++
        inlinePragma defName
      | (TopName defName, (_, stab, _)) <- defs
      , let body = appsE [varE '(.), varE methodName, varE defName]
      , let ty   = quantifyType' (Set.fromList (c:varNames))
                                 (stabToContext stab)
                 $ stabToOptic stab `conAppsT`
                       [VarT c, applyTypeSubst sub (stabToA stab)]
      ]

makeClassyInstance ::
  LensRules ->
  Name ->
  Name ->
  Type {- ^ Outer 's' type -} ->
  [(DefName, (OpticType, OpticStab, [(Name, Int, [Int])]))] ->
  HasFieldClasses Dec
makeClassyInstance rules className methodName s defs = do
  methodss <- traverse (makeFieldOptic rules') defs

  liftState $ instanceD (cxt[]) (return instanceHead)
    $ valD (varP methodName) (normalB (varE 'id)) []
    : map return (concat methodss)

  where
  instanceHead = className `conAppsT` (s : map tvbToType vars)
  vars         = D.freeVariablesWellScoped [s]
  rules'       = rules { _generateSigs    = False
                       , _generateClasses = False
                       }

data OpticType = GetterType | LensType -- or IsoType


-- Compute the positional location of the fields involved in
-- each constructor for a given optic definition as well as the
-- type of clauses to generate and the type to annotate the declaration
-- with.
buildScaffold ::
  LensRules                                                                ->
  Type                              {- outer type                       -} ->
  [(Name, [([DefName], Type)])]     {- normalized constructors          -} ->
  DefName                           {- target definition                -} ->
  Q (OpticType, OpticStab, [(Name, Int, [Int])])
              {- ^ optic type, definition type, field count, target fields -}
buildScaffold rules s cons defName =

  do (s',t,a,b) <- buildStab s (concatMap snd consForDef)

     let defType
           | Just (_,cx,a') <- a ^? _ForallT =
               let optic | lensCase  = ''SimpleGetter
                         | otherwise = ''SimpleFold
               in OpticSa cx optic s' a'

           -- Getter and Fold are always simple
           | not (_allowUpdates rules) =
               let optic | lensCase  = ''SimpleGetter
                         | otherwise = ''SimpleFold
               in OpticSa [] optic s' a

           -- Generate simple Lens and Traversal where possible
           | _simpleLenses rules || s' == t && a == b =
               let optic -- isoCase && _allowIsos rules = ''Iso'
                         | lensCase                    = ''Lens'
                         | otherwise                   = ''Traversal'
               in OpticSa [] optic s' a

           -- Generate type-changing Lens and Traversal otherwise
           | otherwise =
               let optic -- isoCase && _allowIsos rules = ''Iso
                         | lensCase                    = ''Lens
                         | otherwise                   = ''Traversal
               in OpticStab optic s' t a b

         opticType | has _ForallT a            = GetterType
                   | not (_allowUpdates rules) = GetterType
                   -- isoCase                   = IsoType
                   | otherwise                 = LensType

     return (opticType, defType, scaffolds)
  where
  consForDef :: [(Name, [Either Type Type])]
  consForDef = over (mapped . _2 . mapped) categorize cons

  scaffolds :: [(Name, Int, [Int])]
  scaffolds = [ (n, length ts, rightIndices ts) | (n,ts) <- consForDef ]

  rightIndices :: [Either Type Type] -> [Int]
  rightIndices = findIndices (has _Right)

  -- Right: types for this definition
  -- Left : other types
  categorize :: ([DefName], Type) -> Either Type Type
  categorize (defNames, t)
    | defName `elem` defNames = Right t
    | otherwise               = Left  t

  lensCase :: Bool
  lensCase = all (\x -> lengthOf (_2 . folded . _Right) x == 1) consForDef

  -- isoCase :: Bool
  -- isoCase = case scaffolds of
  --             [(_,1,[0])] -> True
  --             _           -> False

data OpticStab = OpticStab     Name Type Type Type Type
               | OpticSa   Cxt Name Type Type


stabToType :: OpticStab -> Type
stabToType (OpticStab  c s t a b) = quantifyType [] (c `conAppsT` [s,t,a,b])
stabToType (OpticSa cx c s   a  ) = quantifyType cx (c `conAppsT` [s,a])

stabToContext :: OpticStab -> Cxt
stabToContext OpticStab{}        = []
stabToContext (OpticSa cx _ _ _) = cx

stabToOptic :: OpticStab -> Name
stabToOptic (OpticStab c _ _ _ _) = c
stabToOptic (OpticSa _ c _ _) = c

stabToS :: OpticStab -> Type
stabToS (OpticStab _ s _ _ _) = s
stabToS (OpticSa _ _ s _) = s

stabToA :: OpticStab -> Type
stabToA (OpticStab _ _ _ a _) = a
stabToA (OpticSa _ _ _ a) = a

-- Compute the s t a b types given the outer type 's' and the
-- categorized field types. Left for fixed and Right for visited.
-- These types are "raw" and will be packaged into an 'OpticStab'
-- shortly after creation.
buildStab :: Type -> [Either Type Type] -> Q (Type,Type,Type,Type)
buildStab s categorizedFields =
  do (subA,a) <- unifyTypes targetFields
     let s' = applyTypeSubst subA s

     -- compute possible type changes
     sub <- sequenceA (Map.fromSet (newName . nameBase) unfixedTypeVars)
     let (t,b) = over both (substTypeVars sub) (s',a)

     return (s',t,a,b)

  where
  (fixedFields, targetFields) = partitionEithers categorizedFields

  fixedTypeVars, unfixedTypeVars :: Set Name
  fixedTypeVars   = closeOverKinds $ setOf typeVars fixedFields
  unfixedTypeVars = setOf typeVars s Set.\\ fixedTypeVars

  -- Compute the kind variables that appear in the kind of a type variable
  -- binder. For example, @kindVarsOfTvb (x :: (a, b)) = (x, {a, b})@. If a
  -- type variable binder lacks an explicit kind annotation, this
  -- conservatively assumes that there are no kind variables. For example,
  -- @kindVarsOfTvb (y) = (y, {})@.
  kindVarsOfTvb :: D.TyVarBndr_ flag -> (Name, Set Name)
  kindVarsOfTvb = D.elimTV (\n   -> (n, Set.empty))
                           (\n k -> (n, setOf typeVars k))

  -- For each type variable name that appears in @s@, map to the kind variables
  -- that appear in that type variable's kind.
  sKindVarMap :: Map Name (Set Name)
  sKindVarMap = Map.fromList $ map kindVarsOfTvb $ D.freeVariablesWellScoped [s]

  lookupSKindVars :: Name -> Set Name
  lookupSKindVars n = fromMaybe Set.empty $ Map.lookup n sKindVarMap

  -- Consider this example (adapted from #972):
  --
  --   data Dart (s :: k) = Dart { _arc :: Proxy s, _direction :: Int }
  --   $(makeLenses ''Dart)
  --
  -- When generating a Lens for `direction`, the type variable `s` should be
  -- fixed. But note that (s :: k), and as a result, the kind variable `k`
  -- needs to be fixed as well. This is because a type like this would be
  -- ill kinded:
  --
  --   direction :: Lens (Dart (s :: k1)) (Dart (s :: k2)) Direction Direction
  --
  -- However, only `s` is mentioned syntactically in the type of `_arc`, so we
  -- have to infer that `k` is mentioned in the kind of `s`. We accomplish this
  -- with `closeOverKinds`, which does the following:
  --
  -- 1. Use freeVariablesWellScoped to compute the free type variables of
  --    `Dart (s :: k)`, which gives us `(s :: k)`.
  -- 2. For each type variable name in `Proxy s`, the type of `_arc`, look up
  --    the kind variables in the type variable's kind. In the case of `s`,
  --    the only kind variable is `k`.
  -- 3. Add these kind variables to the set of fixed type variables.
  closeOverKinds :: Set Name -> Set Name
  closeOverKinds st = Set.foldl' Set.union Set.empty (Set.map lookupSKindVars st) `Set.union` st

-- Build the signature and definition for a single field optic.
-- In the case of a singleton constructor irrefutable matches are
-- used to enable the resulting lenses to be used on a bottom value.
makeFieldOptic ::
  LensRules ->
  (DefName, (OpticType, OpticStab, [(Name, Int, [Int])])) ->
  HasFieldClasses [Dec]
makeFieldOptic rules (defName, (opticType, defType, cons)) = do
  locals <- get
  addName
  liftState $ do
    cls <- mkCls locals
    sequenceA (cls ++ sig ++ def)
  where
  mkCls locals = case defName of
                 MethodName c n | _generateClasses rules ->
                  do classExists <- isJust <$> lookupTypeName (show c)
                     return (if classExists || Set.member c locals then [] else [makeFieldClass defType c n])
                 _ -> return []

  addName = case defName of
            MethodName c _ -> addFieldClassName c
            _              -> return ()

  sig = case defName of
          _ | not (_generateSigs rules) -> []
          TopName n -> [sigD n (return (stabToType defType))]
          MethodName{} -> []

  fun n = funD n clauses : inlinePragma n

  def = case defName of
          TopName n      -> fun n
          MethodName c n -> [makeFieldInstance defType c (fun n)]

  clauses = makeFieldClauses rules opticType cons

------------------------------------------------------------------------
-- Field class generation
------------------------------------------------------------------------

makeFieldClass :: OpticStab -> Name -> Name -> DecQ
makeFieldClass defType className methodName =
  classD (cxt []) className [D.plainTV s, D.plainTV a] [FunDep [s] [a]]
         [sigD methodName (return methodType)]
  where
  methodType = quantifyType' (Set.fromList [s,a])
                             (stabToContext defType)
             $ stabToOptic defType `conAppsT` [VarT s,VarT a]
  s = mkName "s"
  a = mkName "a"

-- | Build an instance for a field. If the field’s type contains any type
-- families, will produce an equality constraint to avoid a type family
-- application in the instance head.
makeFieldInstance :: OpticStab -> Name -> [DecQ] -> DecQ
makeFieldInstance defType className decs =
  containsTypeFamilies a >>= pickInstanceDec
  where
  s = stabToS defType
  a = stabToA defType

  containsTypeFamilies = go <=< D.resolveTypeSynonyms
    where
    go (ConT nm) = (\i -> case i of FamilyI d _ -> isTypeFamily d; _ -> False)
                   <$> reify nm
    go ty = or <$> traverse go (children ty)

#if MIN_VERSION_template_haskell(2,11,0)
  isTypeFamily OpenTypeFamilyD{}       = True
  isTypeFamily ClosedTypeFamilyD{}     = True
#elif MIN_VERSION_template_haskell(2,9,0)
  isTypeFamily (FamilyD TypeFam _ _ _) = True
  isTypeFamily ClosedTypeFamilyD{}     = True
#else
  isTypeFamily (FamilyD TypeFam _ _ _) = True
#endif
  isTypeFamily _ = False

  pickInstanceDec hasFamilies
    | hasFamilies = do
        placeholder <- VarT <$> newName "a"
        mkInstanceDec
          [return (D.equalPred placeholder a)]
          [s, placeholder]
    | otherwise = mkInstanceDec [] [s, a]

  mkInstanceDec context headTys =
    instanceD (cxt context) (return (className `conAppsT` headTys)) decs

------------------------------------------------------------------------
-- Optic clause generators
------------------------------------------------------------------------

makeFieldClauses :: LensRules -> OpticType -> [(Name, Int, [Int])] -> [ClauseQ]
makeFieldClauses rules opticType cons =
  case opticType of

    -- IsoType    -> [ makeIsoClause conName | (conName, _, _) <- cons ]

    GetterType -> [ makeGetterClause conName fieldCount fields
                    | (conName, fieldCount, fields) <- cons ]

    LensType   -> [ makeFieldOpticClause conName fieldCount fields irref
                    | (conName, fieldCount, fields) <- cons ]
      where
      irref = _lazyPatterns rules
           && length cons == 1

-- Construct an optic clause that returns an unmodified value
-- given a constructor name and the number of fields on that
-- constructor.
makePureClause :: Name -> Int -> ClauseQ
makePureClause conName fieldCount =
  do xs <- newNames "x" fieldCount
     -- clause: _ (Con x1..xn) = pure (Con x1..xn)
     clause [wildP, conP conName (map varP xs)]
            (normalB (appE (varE 'pure) (appsE (conE conName : map varE xs))))
            []

-- Construct an optic clause suitable for a Getter or Fold
-- by visited the fields identified by their 0 indexed positions
makeGetterClause :: Name -> Int -> [Int] -> ClauseQ
makeGetterClause conName fieldCount []     = makePureClause conName fieldCount
makeGetterClause conName fieldCount fields =
  do f  <- newName "f"
     xs <- newNames "x" (length fields)

     let pats (i:is) (y:ys)
           | i `elem` fields = varP y : pats is ys
           | otherwise = wildP : pats is (y:ys)
         pats is     _  = map (const wildP) is

         fxs   = [ appE (varE f) (varE x) | x <- xs ]
         body  = foldl (\a b -> appsE [varE '(<*>), a, b])
                       (appE (varE 'phantom) (head fxs))
                       (tail fxs)

     -- clause f (Con x1..xn) = coerce (f x1) <*> ... <*> f xn
     clause [varP f, conP conName (pats [0..fieldCount - 1] xs)]
            (normalB body)
            []

-- Build a clause that updates the field at the given indexes
-- When irref is 'True' the value with me matched with an irrefutable
-- pattern. This is suitable for Lens and Traversal construction
makeFieldOpticClause :: Name -> Int -> [Int] -> Bool -> ClauseQ
makeFieldOpticClause conName fieldCount [] _ =
  makePureClause conName fieldCount
makeFieldOpticClause conName fieldCount (field:fields) irref =
  do f  <- newName "f"
     xs <- newNames "x" fieldCount
     ys <- newNames "y" (1 + length fields)

     let xs' = foldr (\(i,x) -> set (ix i) x) xs (zip (field:fields) ys)

         mkFx i = appE (varE f) (varE (xs !! i))

         body0 = appsE [ varE 'fmap
                       , lamE (map varP ys) (appsE (conE conName : map varE xs'))
                       , mkFx field
                       ]

         body = foldl (\a b -> appsE [varE '(<*>), a, mkFx b]) body0 fields

     let wrap = if irref then tildeP else id

     clause [varP f, wrap (conP conName (map varP xs))]
            (normalB body)
            []

------------------------------------------------------------------------
-- Unification logic
------------------------------------------------------------------------

-- The field-oriented optic generation supports incorporating fields
-- with distinct but unifiable types into a single definition.

-- Unify the given list of types, if possible, and return the
-- substitution used to unify the types for unifying the outer
-- type when building a definition's type signature.
unifyTypes :: [Type] -> Q (Map Name Type, Type)
unifyTypes (x:xs) = foldM (uncurry unify1) (Map.empty, x) xs
unifyTypes []     = fail "unifyTypes: Bug: Unexpected empty list"


-- Attempt to unify two given types using a running substitution
unify1 :: Map Name Type -> Type -> Type -> Q (Map Name Type, Type)
unify1 sub (VarT x) y
  | Just r <- Map.lookup x sub = unify1 sub r y
unify1 sub x (VarT y)
  | Just r <- Map.lookup y sub = unify1 sub x r
unify1 sub x y
  | x == y = return (sub, x)
unify1 sub (AppT f1 x1) (AppT f2 x2) =
  do (sub1, f) <- unify1 sub  f1 f2
     (sub2, x) <- unify1 sub1 x1 x2
     return (sub2, AppT (applyTypeSubst sub2 f) x)
unify1 sub x (VarT y)
  | elemOf typeVars y (applyTypeSubst sub x) =
      fail "Failed to unify types: occurs check"
  | otherwise = return (Map.insert y x sub, x)
unify1 sub (VarT x) y = unify1 sub y (VarT x)

-- TODO: Unify contexts
unify1 sub (ForallT v1 [] t1) (ForallT v2 [] t2) =
     -- This approach works out because by the time this code runs
     -- all of the type variables have been renamed. No risk of shadowing.
  do (sub1,t) <- unify1 sub t1 t2
     v <- fmap nub (traverse (limitedSubst sub1) (v1++v2))
     return (sub1, ForallT v [] t)

unify1 _ x y = fail ("Failed to unify types: " ++ show (x,y))

-- Perform a limited substitution on type variables. This is used
-- when unifying rank-2 fields when trying to achieve a Getter or Fold.
limitedSubst :: Map Name Type -> D.TyVarBndrSpec -> Q D.TyVarBndrSpec
limitedSubst sub tv
  | Just r <- Map.lookup (D.tvName tv) sub =
       case r of
         VarT m -> limitedSubst sub (D.mapTVName (const m) tv)
         _ -> fail "Unable to unify exotic higher-rank type"
  | otherwise = return tv

-- Apply a substitution to a type. This is used after unifying
-- the types of the fields in unifyTypes.
applyTypeSubst :: Map Name Type -> Type -> Type
applyTypeSubst sub = rewrite aux
  where
  aux (VarT n) = Map.lookup n sub
  aux _        = Nothing

------------------------------------------------------------------------
-- Field generation parameters
------------------------------------------------------------------------

{- |
Rules used to generate lenses. The fields are intentionally not exported; to create your own rules, see lenses in the “Configuring lens rules” section. You'd have to customise one of the existing rulesets; for an example of doing that, see 'makeLensesWith'.
-}
data LensRules = LensRules
  { _simpleLenses    :: Bool
  , _generateSigs    :: Bool
  , _generateClasses :: Bool
  -- , _allowIsos       :: Bool
  , _allowUpdates    :: Bool -- Allow Lens/Traversal (otherwise Getter/Fold)
  , _lazyPatterns    :: Bool
  -- Type Name -> Field Names -> Target Field Name -> Definition Names
  , _fieldToDef      :: Name -> [Name] -> Name -> [DefName]
  -- Type Name -> (Class Name, Top Method)
  , _classyLenses    :: Name -> Maybe (Name, Name)
  }

{- |
Name to give to a generated lens (used in 'lensField').
-}
data DefName
  = TopName Name          -- ^ Simple top-level definiton name
  | MethodName Name Name  -- ^ 'makeFields'-style class name and method name
  deriving (Show, Eq, Ord)


------------------------------------------------------------------------
-- Miscellaneous utility functions
------------------------------------------------------------------------

liftState :: Monad m => m a -> StateT s m a
liftState act = StateT (\s -> liftM (flip (,) s) act)
