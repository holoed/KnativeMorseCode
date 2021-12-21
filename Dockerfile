FROM haskell:8.10.5 as mybuild

RUN cabal update

# Add .cabal file
ADD ./MorseCode.cabal /opt/MorseCode.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
RUN cd /opt && cabal build --only-dependencies 

# Add and Install Application Code
ADD . /opt
RUN cd /opt && cabal configure && cabal build MorseCode && cabal install

FROM haskell:8.10.5

COPY --from=mybuild /root/.cabal/bin/MorseCode /root/.cabal/bin/MorseCode 

# Add installed cabal executable to PATH
ENV PATH /root/.cabal/bin:$PATH

# Default Command for Container
WORKDIR /opt

EXPOSE 8080

ENTRYPOINT ["MorseCode"]