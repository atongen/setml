FROM ocaml/opam2:alpine as dependencies

RUN sudo apk update
RUN opam switch 4.06 && opam update && opam upgrade
RUN echo 2

COPY . /setml/
RUN sudo chown -R opam:nogroup /setml
WORKDIR /setml
RUN opam pin add -yn setml .
RUN opam depext setml
RUN opam install --deps-only setml
RUN /bin/sh -c 'eval $(opam env) make'

FROM scratch
COPY --from=dependencies /setml/bin/setml /usr/local/bin/setml
ENTRYPOINT ["/usr/local/bin/setml"]
