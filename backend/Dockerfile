FROM haskell:8.0.1

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq-dev

COPY app /var/confsinfo/app
COPY db /var/confsinfo/db
COPY dist /var/confsinfo/dist
COPY confsinfo-backend.cabal stack.yaml /var/confsinfo/

WORKDIR /var/confsinfo
RUN stack --no-terminal --install-ghc build --copy-bins

CMD /root/.local/bin/confsinfo-backend-exe
