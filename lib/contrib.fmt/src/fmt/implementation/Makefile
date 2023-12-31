PACKAGE = fmt

CHEZ = scheme
INSTALL = install -D

PREFIX = /usr/local
EXEC_PREFIX = ${PREFIX}
BINDIR = ${EXEC_PREFIX}/bin
LIBDIR = ${EXEC_PREFIX}/lib
INCLUDEDIR = ${PREFIX}/include
DATAROOTDIR = ${PREFIX}/share
DATADIR = ${DATAROOTDIR}
MANDIR = ${DATAROOTDIR}/man
INFODIR = ${DATAROOTDIR}/info
DOCDIR = ${DATAROOTDIR}/doc/${PACKAGE}-`cat VERSION`

.PHONY: all chicken gauche gauche-dir mzscheme planet clean distclean doc

all: gauche mzscheme plt doc

gauche/lib/text/fmt: 
	mkdir -p $@

gauche/lib/text/fmt.scm: fmt-gauche.scm fmt.scm fmt-pretty.scm fmt-column.scm
	(cat $+; echo '(provide "text/fmt")') > $@
gauche/lib/text/fmt/c.scm: fmt-c-gauche.scm fmt-c.scm
	(cat $+; echo '(provide "text/fmt/c")') > $@
gauche/lib/text/fmt/js.scm: fmt-js-gauche.scm fmt-js.scm
	(cat $+; echo '(provide "text/fmt/js")') > $@
gauche/lib/text/fmt/unicode.scm: fmt-unicode-gauche.scm fmt-unicode.scm
	(cat $+; echo '(provide "text/fmt/unicode")') > $@
gauche/lib/text/fmt/color.scm: fmt-color-gauche.scm fmt-color.scm
	(cat $+; echo '(provide "text/fmt/color")') > $@

gauche: gauche/lib/text/fmt/ gauche/lib/text/fmt.scm \
	gauche/lib/text/fmt/c.scm gauche/lib/text/fmt/js.scm \
	gauche/lib/text/fmt/unicode.scm gauche/lib/text/fmt/color.scm

install-gauche:
	gauche-install -d `gauche-config --sitelibdir`/text/fmt
	gauche-install gauche/lib/text/fmt.scm `gauche-config --sitelibdir`/text/
	gauche-install gauche/lib/text/fmt/c.scm `gauche-config --sitelibdir`/text/fmt/
	gauche-install gauche/lib/text/fmt/js.scm `gauche-config --sitelibdir`/text/fmt/
	gauche-install gauche/lib/text/fmt/unicode.scm `gauche-config --sitelibdir`/text/fmt/
	gauche-install gauche/lib/text/fmt/color.scm `gauche-config --sitelibdir`/text/fmt/

fmt/let-optionals.ss: let-optionals.scm
	(echo '(module let-optionals mzscheme (provide let-optionals*)'; cat $+; echo ')') > $@
fmt/mantissa.ss: mantissa.scm
	(echo '(module mantissa mzscheme (require "let-optionals.ss") (provide mantissa+exponent)'; cat $+; echo ')') > $@
fmt/fmt.ss: fmt-mzscheme.scm fmt.scm fmt-pretty.scm fmt-column.scm
	(cat $+; echo ')') > $@
fmt/fmt-c.ss: fmt-c-mzscheme.scm fmt-c.scm
	(cat $+; echo ')') > $@
fmt/fmt-js.ss: fmt-js-mzscheme.scm fmt-js.scm
	(cat $+; echo ')') > $@
fmt/fmt-color.ss: fmt-color-mzscheme.scm fmt-color.scm
	(cat $+; echo ')') > $@
fmt/fmt-unicode.ss: fmt-unicode-mzscheme.scm fmt-unicode.scm
	(cat $+; echo ')') > $@

fmt.plt: fmt/let-optionals.ss fmt/mantissa.ss fmt/fmt.ss fmt/fmt-c.ss fmt/fmt-js.ss fmt/fmt-color.ss fmt/fmt-unicode.ss
	mkdir -p fmt
	mzc --at-plt --plt fmt.plt fmt/

mzscheme: fmt.plt

planet: fmt/let-optionals.ss fmt/mantissa.ss fmt/fmt.ss fmt/fmt-c.ss fmt/fmt-js.ss fmt/fmt-color.ss fmt/fmt-unicode.ss
	planet create fmt

fmt-base.html: fmt.doc fmt.mistie fmt.css
	csi -R mistie -e '(mistie-load "plain.mistie")' \
	              -e '(mistie-load "scmhilit.mistie")' \
	              -e '(define h-page-count 0)' \
	              -e '(define (h-page-numbered . x) "")' \
	              -e '(mistie-load "xref.mistie")' \
	              -e '(mistie-load "fmt.mistie")' \
	              -e '(mistie-main "$<")' > $@

# you didn't see this
fmt.html: fmt-base.html
	grep '^<a name="SECTION_' $< |\
	  perl -ape 's{<a name="(SECTION_(\d+)(\.\d+)?)"><h[12]>(?:[.\d]+)(?:\s|&nbsp;)*([^<>]*).*}{($$3?($$3==.1?($$sub=1,"<ol>\n"):""):(($$x=$$sub,$$sub=0,$$x>0)?"</ol>\n":""))."<li><a href=\"#$$1\">$$4</a>"}ge;' > fmt-toc.html
	perl -ape 's{^<!--\s*TOC\s*-->}{"<ol>\n".`cat fmt-toc.html`."</ol>"}e' $< > $@
	rm -f $< fmt-toc.html

doc: fmt.html

clean:
	rm -f *~ */*~ *.so

distclean: clean
	rm -f fmt/f*.scm gauche/lib/text/fmt/*

allclean: distclean
	rm -f fmt.html fmt.plt

dist: doc distclean
	rm -f fmt-`cat VERSION`.tar.gz
	mkdir fmt-`cat VERSION`
	for f in `hg manifest`; do mkdir -p fmt-`cat VERSION`/`dirname $$f`; ln -s `pwd`/$$f fmt-`cat VERSION`/$$f; done
	cd fmt-`cat VERSION`; for f in `echo ../*.html`; do ln -s $$f; done; cd ..
	tar cphzvf fmt-`cat VERSION`.tar.gz fmt-`cat VERSION`
	rm -rf fmt-`cat VERSION`

CHICKEN_FMT_DIR=~/src/scheme/chicken-eggs/fmt/trunk

chicken:
	@for f in fmt-c-chicken.scm fmt-column.scm fmt.scm fmt-c.scm fmt-js.scm fmt-js-chicken.scm fmt-pretty.scm fmt.setup fmt-chicken.scm fmt-unicode-chicken.scm test-fmt-c.scm fmt-color-chicken.scm fmt.html test-fmt.scm fmt-color.scm fmt.meta test-round.scm; do if ! diff -q $$f $(CHICKEN_FMT_DIR)/$$f; then cp $$f $(CHICKEN_FMT_DIR)/; fi; done

chezversion ::= $(shell echo '(call-with-values scheme-version-number (lambda (a b c) (format \#t "~d.~d" a b)))' | ${CHEZ} -q)
schemedir = ${LIBDIR}/csv${chezversion}-site

chez-build:
	$(CHEZ) --program compile-all.chezscheme.sls

chez-install:
	${INSTALL} -t ${schemedir} fmt.so fmt-c.so fmt-js.so
	${INSTALL} -t ${DOCDIR} README VERSION fmt.html fmt.css

chez-install-src:
	${INSTALL} -t ${schemedir} fmt.sls fmt.scm fmt-c.sls fmt-c.scm fmt-js.sls fmt-js.scm fmt-utils.chezscheme.sls fmt-pretty.scm let-optionals.scm fmt-column.scm

chez-check:
	$(CHEZ) --script test-fmt.chezscheme.sls
	$(CHEZ) --script test-fmt-c.chezscheme.sls
	$(CHEZ) --script test-fmt-js.chezscheme.sls
