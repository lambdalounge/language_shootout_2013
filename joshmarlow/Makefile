DELIM = ----------------------------
CFLAGS = "-w A"
TEST_BUILD_MODE = byte

tests: count_nt_tests mendel_tests

app: count_nt_app.native

count_nt_tests: *.mli *.ml
	@echo $(DELIM) Building count_nt_tests native library...
	@ocamlbuild -cflags $(CFLAGS) -use-ocamlfind count_nt_tests.native
	@./count_nt_tests.native

mendel_tests: *.mli *.ml
	@echo $(DELIM) Building mendel_tests native library...
	@ocamlbuild -cflags $(CFLAGS) -use-ocamlfind mendel_tests.native
	@./mendel_tests.native

%.byte: %.ml
	@echo $(DELIM) Building $@... from $<
	ocamlbuild -use-ocamlfind -cflags $(CFLAGS) $@

%.native: %.ml
	@echo $(DELIM) Building $@... from $<
	ocamlbuild -use-ocamlfind -cflags $(CFLAGS) $@

clean:
	@echo $(DELIM) Cleaning up... $(DELIM)
	@ocamlbuild -clean
