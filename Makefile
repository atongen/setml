build_server:
	ocamlbuild \
		-pkg containers \
		-pkg lwt \
		-pkg nocrypto \
		-pkg nocrypto.lwt \
		-pkg session \
		-pkg session-cohttp \
		-pkg session-cohttp-lwt \
		-pkg websocket \
		-pkg websocket.lwt \
		-pkg websocket-lwt.cohttp \
		-use-ocamlfind \
			src/server/cohttp_server.cmi \
			src/server/file_server.cmi \
			src/server/setml.native
