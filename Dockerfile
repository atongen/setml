# Backend
FROM ocaml/opam2:alpine-3.8 as be_deps

RUN sudo apk update && \
    opam switch 4.06 && \
    opam update && \
    opam install depext && \
    opam upgrade

WORKDIR /setml
COPY setml.opam .
RUN sudo chown -R opam:nogroup . && \
    opam pin add -yn setml . && \
    opam depext setml && \
    opam install --deps-only setml && \
    opam depext -ln setml > depexts
COPY . .
RUN sudo chown -R opam:nogroup . && \
    opam config exec make

# Front-end
FROM node:8-alpine as fe_deps

RUN apk add --update \
    build-base \
    gcc \
    wget \
    git \
    python

WORKDIR /setml
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run-script build && \
    npm run-script webpack:production

# Final
FROM alpine:3.8
WORKDIR /setml
COPY --from=be_deps /setml/_build/default/src/server/setml.exe setml
COPY --from=be_deps /setml/depexts depexts
COPY --from=fe_deps /setml/public public
RUN cat depexts | xargs apk --update add && \
    rm -rf /var/cache/apk/*
ENTRYPOINT ["/setml/setml"]
