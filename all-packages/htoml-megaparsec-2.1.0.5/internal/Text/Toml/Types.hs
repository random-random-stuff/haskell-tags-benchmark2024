{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Text.Toml.Types
  ( Table
  , emptyTable
  , VTArray
  , VArray
  , Node (..)
  , Explicitness (..)
  , isExplicit
  , insert
  , throwParser
  , Toml
  , TomlM
  , Parser
  ) where

import           Control.Applicative       (Alternative)
import           Control.DeepSeq           (NFData)
import           Control.Monad             (MonadPlus, join, when)
import           Control.Monad.State       (State)
import           Control.Monad.State.Class (MonadState, get, modify)
import           Control.Monad.Trans       (lift)
import           Data.HashMap.Lazy         (HashMap)
import qualified Data.HashMap.Lazy         as M
import           Data.Int                  (Int64)
import           Data.List                 (intersect)
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Format          ()
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Data.Void                 (Void)
import           GHC.Generics              (Generic)
import           Text.Megaparsec           hiding (State)

type Parser m a = (MonadState (Set [Text]) m) => ParsecT Void Text m a

type TomlM m = (MonadState (S.Set [Text]) m)

type Toml = State (S.Set [Text])

-- | The TOML 'Table' is a mapping ('HashMap') of 'Text' keys to 'Node' values.
type Table = HashMap Text Node

-- | Contruct an empty 'Table'.
emptyTable :: Table
emptyTable = M.empty

-- | An array of 'Table's, implemented using a 'Vector'.
type VTArray = Vector Table

-- | A \"value\" array that may contain zero or more 'Node's, implemented using a 'Vector'.
type VArray = Vector Node

-- | A 'Node' may contain any type of value that may be put in a 'VArray'.
data Node = VTable    Table
          | VTArray   VTArray
          | VString   Text
          | VInteger  Int64
          | VFloat    Double
          | VBoolean  Bool
          | VDatetime UTCTime
          | VArray    VArray
  deriving (Show, Eq, Generic, NFData)

-- | To mark whether or not a 'Table' has been explicitly defined.
-- See: https://github.com/toml-lang/toml/issues/376
data Explicitness = Explicit | Implicit

-- | Convenience function to get a boolean value.
isExplicit :: Explicitness -> Bool
isExplicit Explicit = True
isExplicit Implicit = False

throwParser :: (MonadPlus m, Alternative m, Ord e, MonadParsec e s m) => String -> m a
throwParser x = fancyFailure $ S.fromList [ErrorFail x]

-- | Inserts a table, 'Table', with the namespaced name, '[Text]', (which
-- may be part of a table array) into a 'Table'.
-- It may result in an error in the 'ParsecT' monad for redefinitions.
insert :: (TomlM m) => Explicitness -> ([Text], Node) -> Table -> Parser m Table
insert _ ([], _) _ = throwParser "FATAL: Cannot call 'insert' without a name."
insert ex ([name], node) ttbl =
    -- In case 'name' is final (a top-level name)
    case M.lookup name ttbl of
      Nothing -> do when (isExplicit ex) $ updateExStateOrError [name] node
                    return $ M.insert name node ttbl
      Just (VTable t) -> case node of
          (VTable nt) -> case merge t nt of
                  Left ds -> nameInsertError ds name
                  Right r -> do when (isExplicit ex) $
                                  updateExStateOrError [name] node
                                return $ M.insert name (VTable r) ttbl
          _ -> commonInsertError node [name]
      Just (VTArray a) -> case node of
          (VTArray na) -> return $ M.insert name (VTArray $ a V.++ na) ttbl
          _            -> commonInsertError node [name]
      Just _ -> commonInsertError node [name]
insert ex (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final (not a top-level name)
    case M.lookup name ttbl of
      Nothing -> do
          r <- insert Implicit (ns, node) emptyTable
          when (isExplicit ex) $ updateExStateOrError fullName node
          return $ M.insert name (VTable r) ttbl
      Just (VTable t) -> do
          r <- insert Implicit (ns, node) t
          when (isExplicit ex) $ updateExStateOrError fullName node
          return $ M.insert name (VTable r) ttbl
      Just (VTArray a) ->
          if V.null a
          then throwParser "FATAL: Call to 'insert' found impossibly empty VArray."
          else do r <- insert Implicit (ns, node) (V.last a)
                  return $ M.insert name (VTArray $ V.init a `V.snoc` r) ttbl
      Just _ -> commonInsertError node fullName


-- FIXME use a Set here (?)
-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contain those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: Table -> Table -> Either [Text] Table
merge existing new = case M.keys existing `intersect` M.keys new of
                       [] -> Right $ M.union existing new
                       ds -> Left ds

-- TOML tables maybe redefined when first definition was implicit.
-- For instance a top-level table `a` can implicitly defined by defining a non top-level
-- table `b` under it (namely with `[a.b]`). Once the table `a` is subsequently defined
-- explicitly (namely with `[a]`), it is then not possible to (re-)define it again.
-- A parser state of all explicitly defined tables is maintained, which allows
-- raising errors for illegal redefinitions of such.
updateExStateOrError :: (TomlM m) => [Text] -> Node -> Parser m ()
updateExStateOrError name node@(VTable _) = do
    explicitlyDefinedNames <- lift get
    let ns = explicitlyDefinedNames
    when (S.member name ns) $ tableClashError name
    updateExState name node
updateExStateOrError _ _ = return ()

-- | Like 'updateExStateOrError' but does not raise errors. Only use this when sure
-- that redefinitions cannot occur.
updateExState :: (TomlM m) => [Text] -> Node -> Parser m ()
updateExState name (VTable _) = lift $ modify (S.insert name)
updateExState _ _             = return ()


-- * Parse errors resulting from invalid TOML

-- | Key(s) redefintion error.
nameInsertError :: (TomlM m) => [Text] -> Text -> Parser m a
nameInsertError ns name = throwParser . T.unpack $ T.concat
    [ "Cannot redefine key(s) (", T.intercalate ", " ns
    , "), from table named '", name, "'." ]

-- | Table redefinition error.
tableClashError :: (TomlM m) => [Text] -> Parser m a
tableClashError name = throwParser . T.unpack $ T.concat
    [ "Cannot redefine table named: '", T.intercalate "." name, "'." ]

-- | Common redefinition error.
commonInsertError :: (TomlM m) => Node -> [Text] -> Parser m a
commonInsertError what name = throwParser . join $
    [ "Cannot insert ", w, " as '", n, "' since key already exists." ]
  where
    n = T.unpack $ T.intercalate "." name
    w = case what of (VTable _) -> "tables"
                     _          -> "array of tables"
