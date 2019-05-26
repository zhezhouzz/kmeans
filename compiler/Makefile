all: mparser compiler

mparser:
	cd parser && $(MAKE)

compiler:
	mlton compiler.mlb

clean:
	rm compiler && cd parser && $(MAKE) clean
