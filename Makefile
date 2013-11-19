
make:
	@ghc -package wx Main.hs -o Main
	@rm *.hi *.o

clean:
	@rm Main
