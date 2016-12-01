FROM haskell:8.0.1

COPY app /var/confsinfo/app
COPY dist /var/confsinfo/dist
COPY confsinfo-backend.cabal stack.yaml /var/confsinfo/

WORKDIR /var/confsinfo
RUN stack build --copy-bins

CMD /root/.local/bin/confsinfo-backend-exe
