# See README for Cabal-based building.  Other fancy stuff (like haddock) here.

user = conal
package = DeepArrow

haddock_args=\
  --no-use-packages \
  --haddock-arg=--read-interface=http://haskell.org/ghc/docs/latest/html/libraries/base,c:/ghc/ghc-6.6/doc/html/libraries/base/base.haddock \
  --haddock-arg=--read-interface=http://haskell.org/ghc/docs/latest/html/libraries/mtl,c:/ghc/ghc-6.6/doc/html/libraries/mtl/mtl.haddock \
  # enough, already!

include ../my-cabal-make.inc
