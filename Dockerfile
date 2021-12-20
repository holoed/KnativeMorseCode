FROM haskell:8.10.5

RUN cabal update

# Add .cabal file
ADD ./MorseCode.cabal /opt/MorseCode.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt && cabal build --only-dependencies 

# Add and Install Application Code
ADD . /opt
RUN cd /opt && cabal configure && cabal build MorseCode && cabal install

# Add installed cabal executable to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt

ENTRYPOINT ["MorseCode"]