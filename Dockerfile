FROM ocaml/opam2:alpine as dependencies

RUN sudo apk update
RUN opam switch 4.06 && opam update && opam upgrade

COPY . /setml/
RUN sudo chown -R opam:nogroup /setml
WORKDIR /setml
RUN opam pin add -yn setml . && \
    opam depext setml && \
    $(opam depext -ln setml | egrep -o "\-\s.*" | sed "s/- //" > depexts) && \
    opam install --deps-only setml && \
    opam config exec make

FROM alpine
WORKDIR /setml
COPY --from=dependencies /setml/_build/default/src/server/setml.exe setml
COPY --from=dependencies /setml/depexts depexts
RUN cat depexts | xargs apk --update add && rm -rf /var/cache/apk/*
ENTRYPOINT ["/setml/setml"]
