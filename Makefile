ROOT=.
include Makefile.conf

PKGNAME = $(shell oasis query name)
PKGVERSION = $(shell oasis query version)
PKG_TARBALL = $(PKGNAME)-$(PKGVERSION).tar.gz

DISTFILES = AUTHORS.txt INSTALL.txt README.txt _oasis _tags myocamlbuild.ml \
  config.ml setup.ml Makefile \
  $(wildcard $(addprefix src/, *.ml *.mli *.mllib *.c *.h *.clib)) \
  tests/ examples/ doc/

.PHONY: all byte native configure doc install uninstall reinstall upload-doc

all byte native: configure
	ocaml setup.ml -build

configure: setup.ml
	ocaml $< -configure

setup.ml: _oasis
	oasis setup

doc install uninstall reinstall:
	ocaml setup.ml -$@

upload-doc: doc
	scp -C -p -r _build/src/API.docdir/ $(WEB)

examples: native
	$(MAKE) -C examples

# Depends on the version number set in delimited_overloading.mli :
cairo.godiva: cairo.godiva.in
	@ sed -e "s/@PACKAGE@/$(PKGNAME)/" $< \
	| sed -e "s/@VERSION@/$(PKGVERSION)/" \
	| sed -e "s/@TARBALL@/$(PKG_TARBALL)/" \
	| sed -e "s/@DOWNLOAD@/$(OCAMLFORGE_FILE_NO)/" > $@
	@ echo "Updated \"$@\"."

# Assume the environment variable $GODI_LOCALBASE is set
.PHONY: godi
godi: cairo.godiva
	godiva $<

# Make a tarball
.PHONY: dist tar
dist tar: $(DISTFILES)
	mkdir $(PKGNAME)-$(PKGVERSION)
	cp --parents -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
	tar -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	rm -rf $(PKGNAME)-$(PKGVERSION)

.PHONY: tests
tests: native
	$(MAKE) -C tests

.PHONY: web web-html tutorial
web-html: doc
	$(MAKE) -C doc $@
web tutorial: doc examples
	$(MAKE) -C doc $@


.PHONY: sync-scm sync_scm
sync-scm sync_scm:
	bzr push  svn+ssh://svn.forge.ocamlcore.org/svnroot/archimedes/cairo

.PHONY: clean dist-clean
clean:
	$(RM) $(wildcard *~ *.pdf *.ps *.png *.svg) cairo.godiva setup.data
	$(MAKE) -C src $@
	$(MAKE) -C examples $@
	$(MAKE) -C doc $@
	$(MAKE) -C tests $@

dist-clean::
	$(RM) -r aclocal.m4 autom4te.cache config.log config.status
