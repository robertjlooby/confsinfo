FROM fpco/stack-build:lts-8.2

RUN apt-get update && \
    apt-get install -y --no-install-recommends libpq-dev

COPY . /var/confsinfo

WORKDIR /var/confsinfo
RUN stack --no-terminal build --system-ghc --copy-bins

CMD /root/.local/bin/confsinfo-backend-exe
