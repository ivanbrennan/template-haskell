.PHONY: main
main:
	ghc --make main.hs -o main

.PHONY: clean
clean:
	rm -f ./main ./*.hi ./*.o
