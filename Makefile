HS_FILES = $(wildcard src/day*.hs)
HS_OUTPUTS = $(patsubst src/day%.hs, out/day%.hsexe, $(HS_FILES))
ML_FILES = $(wildcard src/day*.ml)
ML_OUTPUTS = $(patsubst src/day%.ml, out/day%.mlexe, $(ML_FILES))

all: $(HS_OUTPUTS) $(ML_OUTPUTS)

out/day%.hsexe: src/day%.hs src/AOC.hs
	@mkdir -p $(@D)
	ghc $< -o $@ -no-keep-o-files -no-keep-hi-files -isrc

out/day%.mlexe: src/day%.ml src/day%.mli src/AOC.ml
	@mkdir -p $(@D)
	ocamlc -c $<i
	ocamlfind ocamlopt -o $@ -linkpkg -package core -thread -I src src/AOC.ml $<

silent:
	@:
