main:
	ghc --make main.hs -o main

.PHONY: clean
clean:
	rm -f ./main ./main.hi ./main.o
