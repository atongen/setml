FROM ocaml/opam2:alpine-3.8 as dependencies

RUN sudo apk update && \
    opam switch 4.06 && \
    opam update && \
    opam install depext && \
    opam upgrade

COPY . /setml/
RUN sudo chown -R opam:nogroup /setml
WORKDIR /setml
RUN opam pin add -yn setml . && \
    opam depext setml && \
    opam install --deps-only setml && \
    opam depext -ln setml 2>/dev/null > depexts && \
    opam config exec make

FROM alpine:3.8
WORKDIR /setml
COPY --from=dependencies /setml/_build/default/src/server/setml.exe setml
COPY --from=dependencies /setml/depexts depexts
RUN cat depexts && \
    cat depexts | xargs apk --update add && \
    rm -rf /var/cache/apk/*
ENTRYPOINT ["/setml/setml"]
