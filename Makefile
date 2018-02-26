build_server:
	ocamlbuild \
		-pkg containers \
		-pkg lwt \
		-pkg websocket.lwt \
		-pkg websocket-lwt.cohttp \
		-use-ocamlfind \
			src/server/cohttp_server.cmi \
			src/server/file_server.cmi \
			src/server/setml.native
