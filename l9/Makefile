all: main run

main: Main.hs
	ghc -XFlexibleContexts Main.hs -o main

run: main
	./main input.txt
