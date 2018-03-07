build_server:
	ocamlbuild \
		-pkg containers \
		-pkg lwt \
		-pkg nocrypto \
        -pkg nocrypto.lwt \
		-pkg websocket \
		-pkg websocket.lwt \
		-pkg websocket-lwt.cohttp \
		-use-ocamlfind \
			src/server/session.cmi \
			src/server/cohttp_server.cmi \
			src/server/file_server.cmi \
			src/server/setml.native
