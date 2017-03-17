FROM haskell:8.0.1

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq-dev

RUN stack install --install-ghc --resolver lts-8.2 \
      aeson \
      bytestring \
      data-default \
      dotenv \
      hspec \
      postgresql-simple \
      QuickCheck \
      scotty \
      time \
      wai-extra \
      wai-middleware-static

COPY . /var/confsinfo

WORKDIR /var/confsinfo
RUN stack --no-terminal build --copy-bins

CMD /root/.local/bin/confsinfo-backend-exe
