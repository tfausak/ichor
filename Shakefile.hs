#! /usr/bin/env cabal
{- cabal:
build-depends: base == 4.14.1.0, shake == 0.19.2
-}

-- | <https://shakebuild.com>
-- <https://stackoverflow.com/a/50724825/1274282>
-- <https://hackage.haskell.org/package/shake-0.19.2/docs/Development-Shake.html>
-- <https://downloads.haskell.org/~ghc/8.10.2/docs/html/users_guide/index.html>
module Main ( main ) where

import Development.Shake
import Development.Shake.FilePath
import Development.Shake.Util


main :: IO ()
main = shakeArgs options $ do
  let makefile = target </> "Makefile"
  let executable = target </> "ichor" <.> exe
  want [ executable ]


  "clean" ~> do
    removeFilesAfter target [ "//" ]


  makefile %> \ out -> do
    hs <- getDirectoryFiles "" [ source <//> "*.hs" ]
    need hs
    command_ [] "ghc"
      $ "-dep-makefile" : out
      : "-dep-suffix" : "."
      : "-M"
      : "-outputdir" : target
      : hs


  [ "//*.hi", "//*.o" ] &%> \ [ hi, o ] -> do
    let hs = replace target source hi -<.> "hs"
    needMakefileDependenciesFor makefile o
    command_ [] "ghc" [ "-c", "-i" <> target, "-outputdir", target, hs ]


  executable %> \ out -> do
    let root = target </> "Main.o"
    need [ root ]
    needMakefileDependenciesFor makefile root

    -- This ends up needing every object file even if it's not used in the
    -- executable. That means adding a new unused module causes the executable
    -- to be re-linked. Not the end of the world, but it would be nice to
    -- avoid. Doing so would requiring walking the tree of dependencies from
    -- the Makefile.
    os <- needDirectoryFiles "" [ target <//> "*.o" ]

    command_ [] "ghc" $ "-o" : out : os


source :: FilePath
source = "src"


target :: FilePath
target = ".shake"


options :: ShakeOptions
options = shakeOptions
  { shakeChange = ChangeDigest
  , shakeFiles = target
  }


needDirectoryFiles :: FilePath -> [FilePattern] -> Action [FilePath]
needDirectoryFiles directory patterns = do
  files <- getDirectoryFiles directory patterns
  need files
  pure files


needMakefileDependenciesFor :: FilePath -> String -> Action ()
needMakefileDependenciesFor makefile target = do

  -- We only care about the ordering here. If the contents of the Makefile
  -- change, it doesn't necessarily mean that everything mentioned in the
  -- Makefile needs to be rebuilt. A rebuild is only necessary if a module's
  -- dependencies changed, which is handled by the next part of this function.
  orderOnly [ makefile ]

  contents <- liftIO $ readFile makefile
  need . concatMap snd . filter ((== target) . fst) $ parseMakefile contents


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace old new x = case stripPrefix old x of
  Just y -> new <> y
  Nothing -> case x of
    [] -> []
    xh : xt -> xh : replace old new xt


stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix p x = case ( p, x ) of
  ( [], _ ) -> Just x
  ( ph : pt, xh : xt ) | ph == xh -> stripPrefix pt xt
  _ -> Nothing
