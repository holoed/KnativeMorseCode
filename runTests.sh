rm cabal.project.local
cabal user-config update
cabal install --only-dependencies --overwrite-policy=always
cabal configure --enable-tests --ghc-options="-j"
cabal new-build -j8 
cabal new-test --test-show-details=always --test-option=--color