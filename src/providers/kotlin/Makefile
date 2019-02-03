LIBRARY = kotlin
LIBRARY_EXPORT_PREFIX = provider

include ../../Makefile.build

register:
	make
	NATIVE=1 make
	ocamlfind remove neal-provider-kotlin
	ocamlfind install neal-provider-kotlin \
		opam/META \
		_build/parsing/parser.cmi \
		_build/parsing/parser.cmo \
		_build/parsing/lexer.cmo \
		_build/parsing/unicode.cmo \
		_build/parsing/combinators.cmo
