FROM haskell:9.6.6

RUN apt-get update && apt-get install -y \
    z3 \
    build-essential \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*
RUN cabal update

WORKDIR /panini

COPY cabal.project cabal.project
COPY regex-algebra/regex-algebra.cabal regex-algebra/regex-algebra.cabal
COPY panini-lib/panini-lib.cabal panini-lib/panini-lib.cabal
COPY panini-python/panini-python.cabal panini-python/panini-python.cabal
COPY panini.cabal panini.cabal

COPY regex-algebra regex-algebra
RUN cabal build regex

COPY panini-lib panini-lib
COPY panini-python panini-python
COPY app app
RUN cabal build panini

ENTRYPOINT ["cabal", "run", "panini", "--"]
