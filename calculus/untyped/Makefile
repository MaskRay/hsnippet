.PHONY: clean distclean runghc

untyped: *.hs
	ghc -o $@ $^

runghc:
	rlwrap runghc Main.hs

clean:
	$(RM) *.o *.hi

distclean: clean
	$(RM) untyped
