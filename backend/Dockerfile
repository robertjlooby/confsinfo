FROM haskell:8.0.1

COPY app /var/confsinfo/app
COPY dist /var/confsinfo/dist
COPY confsinfo-backend.cabal stack.yaml /var/confsinfo/
RUN mkdir /var/confsinfo/log

WORKDIR /var/confsinfo
RUN stack build

CMD stack exec confsinfo-backend-exe
