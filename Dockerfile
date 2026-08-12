# Build stage
FROM haskell:9.10.3-slim-bookworm AS builder

# System libraries needed for build (xml, yaml, gmp, zlib, etc.)
RUN apt-get update && apt-get install -y \
    libxml2-dev \
    libyaml-dev \
    zlib1g-dev \
    libgmp-dev \
    libffi-dev \
    libtinfo-dev \
    ca-certificates \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Copy dependency descriptors first to benefit from Docker layer caching
COPY package.yaml stack.yaml ./
RUN stack build --only-dependencies

# Copy full source and build the executables
COPY . .
RUN stack build --copy-bins --local-bin-path /app/bin

# Runtime stage
FROM debian:bookworm-slim

# Runtime libraries needed by the compiled binaries
RUN apt-get update && apt-get install -y \
    libxml2 \
    libyaml-0-2 \
    zlib1g \
    libgmp10 \
    libffi8 \
    libtinfo6 \
    ca-certificates \
  && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/bin/* /usr/local/bin/

WORKDIR /data

CMD ["etf-comparator"]
