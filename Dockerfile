FROM haskell:9.4.8

WORKDIR /app

# Update cabal index
RUN cabal update

# Install dependencies
RUN apt-get update && apt-get install libpq-dev

# Cache dependencies
COPY S32EFV.cabal S32EFV.cabal 
RUN cabal build --only-dependencies -j4

# Build the exe
COPY . /app/
RUN cabal install -j4

ENTRYPOINT ["S32EFV", "serve"]
