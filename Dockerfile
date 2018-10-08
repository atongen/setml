FROM ocaml/opam2:alpine as dependencies

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
    opam config exec make

RUN opam depext -ln setml | egrep -o "\-\s.*" | sed "s/- //" > depexts

FROM alpine
WORKDIR /setml
COPY --from=dependencies /setml/_build/default/src/server/setml.exe setml
COPY --from=dependencies /setml/depexts depexts
RUN cat depexts && \
    cat depexts | xargs apk --update add && \
    rm -rf /var/cache/apk/*
ENTRYPOINT ["/bin/sh"]
