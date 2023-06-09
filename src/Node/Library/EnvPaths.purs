-- The below code was ported from this JavaScript library
-- https://github.com/sindresorhus/env-paths
-- Copyright `sindresorhus`
-- MIT License: https://opensource.org/license/mit/
--
-- | Constructs the platform-independent file paths for commonly used storage places.
-- | These paths may not exist on the computer, so calls to `mkdir -p` may be necessary.
module Node.Library.EnvPaths
  ( EnvPaths
  , EnvPathsContext -- constructor intentionally not exported
  , envPathsContext
  , envPaths
  , envPathsNow
  ) where

import Prelude

import Control.Alternative (guard)
import Data.FoldableWithIndex (findMapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String as String
import Effect (Effect)
import Foreign.Object as Object
import Node.OS (homedir, tmpdir)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Platform (Platform(..))
import Node.Process as Process

-- | Platform-generic file paths.
type EnvPaths =
  { data :: FilePath
  , config :: FilePath
  , cache :: FilePath
  , log :: FilePath
  , temp :: FilePath
  }

-- | Context needed to accurately produce an `EnvPaths` value.
newtype EnvPathsContext = EnvPathsContext
  { home :: FilePath
  , tmp :: FilePath
  , appData :: Maybe FilePath
  , localAppData :: Maybe FilePath
  , xdgDataHome :: Maybe FilePath
  , xdgConfigHome :: Maybe FilePath
  , xdgCacheHome :: Maybe FilePath
  , xdgStateHome :: Maybe FilePath
  }

envPathsContext :: Effect EnvPathsContext
envPathsContext = do
  home <- homedir
  tmp <- tmpdir
  env <- Process.getEnv
  let
    normalLookup key = Object.lookup key env
    caseSensitiveLookupEnv key = findMapWithIndex (\k v -> v <$ guard (String.toUpper k == String.toUpper key)) env
  pure $ EnvPathsContext
    { home
    , tmp
    , appData: caseSensitiveLookupEnv "APPDATA"
    , localAppData: caseSensitiveLookupEnv "LOCALAPPDATA"
    , xdgDataHome: normalLookup "XDG_DATA_HOME"
    , xdgConfigHome: normalLookup "XDG_CONFIG_HOME"
    , xdgCacheHome: normalLookup "XDG_CACHE_HOME"
    , xdgStateHome: normalLookup "XDG_STATE_HOME"
    }

-- | Gets the paths based on information obtained previously sometime in the past. 
-- | Since these values rarely change, the below example should be safe for most usages.
-- |
-- | ```
-- | foo = do
-- |   ctx <- envPathsContext
-- |   -- if user changes an environment value after this call
-- |   -- then `paths` below will provide out-of-date information.
-- |   let paths = envPaths' ctx { name: "foo", suffix: Nothing }
-- | ```
-- |
-- | The `suffix` arg defaults to `-nodejs` to prevent name conflicts with native apps.
envPaths :: { ctx :: EnvPathsContext, name :: String, suffix :: Maybe String } -> EnvPaths
envPaths { ctx, name, suffix } = do
  let name' = maybe (name <> "-nodejs") (append name) suffix
  case Process.platform of
    Just Win32 -> windows ctx name'
    Just Darwin -> macos ctx name'
    _ -> linux ctx name'

-- | Variant of `envPaths` that immediately gets path information at the cost of running in `Effect`.
envPathsNow :: { name :: String, suffix :: Maybe String } -> Effect EnvPaths
envPathsNow { name, suffix } = map (\ctx -> envPaths { ctx, name, suffix }) envPathsContext

macos :: EnvPathsContext -> String -> EnvPaths
macos (EnvPathsContext ctx) name = do
  let library = Path.concat [ ctx.home, "Library" ]
  { data: Path.concat [ library, "Application Support", name ]
  , config: Path.concat [ library, "Preferences", name ]
  , cache: Path.concat [ library, "Caches", name ]
  , log: Path.concat [ library, "Logs", name ]
  , temp: Path.concat [ ctx.tmp, name ]
  }

windows :: EnvPathsContext -> String -> EnvPaths
windows (EnvPathsContext ctx) name = do
  let
    appData = fromMaybe (Path.concat [ ctx.home, "AppData", "Roaming" ]) ctx.appData
    localAppData = fromMaybe (Path.concat [ ctx.home, "AppData", "Local" ]) ctx.localAppData
  { data: Path.concat [ localAppData, name, "Data" ]
  , config: Path.concat [ appData, name, "Config" ]
  , cache: Path.concat [ localAppData, name, "Cache" ]
  , log: Path.concat [ localAppData, name, "Log" ]
  , temp: Path.concat [ ctx.tmp, name ]
  }

linux :: EnvPathsContext -> String -> EnvPaths
linux (EnvPathsContext ctx) name = do
  let
    username = Path.basename ctx.home
    prependNamePath ctxVal fallbackPath =
      Path.concat [ fromMaybe fallbackPath ctxVal, name ]
  { data: prependNamePath ctx.xdgDataHome $ Path.concat [ ctx.home, ".local", "share" ]
  , config: prependNamePath ctx.xdgConfigHome $ Path.concat [ ctx.home, ".config" ]
  , cache: prependNamePath ctx.xdgCacheHome $ Path.concat [ ctx.home, ".cache" ]
  , log: prependNamePath ctx.xdgStateHome $ Path.concat [ ctx.home, ".local", "state" ]
  , temp: Path.concat [ ctx.tmp, username, name ]
  }
