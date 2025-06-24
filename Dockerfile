FROM haskell:9.6.6

RUN apt-get update && apt-get install -y \
    z3 \
    build-essential \
    libgmp-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /panini

COPY app app
COPY panini-lib panini-lib
COPY panini-python panini-python
COPY regex-algebra regex-algebra
COPY cabal.project cabal.project
COPY panini.cabal panini.cabal

RUN cabal update
RUN cabal install --only-dependencies

RUN cabal build panini
RUN cabal build regex

ENTRYPOINT ["cabal", "run", "panini", "--"]
