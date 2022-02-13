## ----------------------------------------------------------------------------
## Document/Build/Check/Install R package (depends on devtools and roxygen2)
## Author: Samuel Pawel
## adapted from Manuela Ott, Sebastian Meyer, Florian Gerber
## ----------------------------------------------------------------------------

PACKAGE = bayesROE
VERSION = 0.42
TAR = $(PACKAGE)_$(VERSION).tar.gz

all: build

description:
	sed -i -r -- 's/^Version:.*/Version: '$(VERSION)'/g' pkg/DESCRIPTION ;      
	sed -i -r -- 's/^Date:.*/Date: '`date +'%F'`'/g' pkg/DESCRIPTION ;
	R -e 'roxygen2::roxygenize("pkg/")'

document: description
	R -e 'devtools::document(pkg = "pkg/")'

manual: document
	R -e 'devtools::build_manual(pkg = "pkg/")'

$(TAR): manual
	R -e 'devtools::build(pkg = "pkg/")'

build: $(TAR)

install: $(TAR)
	R -e 'devtools::install(pkg = "pkg/", build = FALSE)'

check: $(TAR)
	R -e 'devtools::check_built(path = "$(TAR)", cran = FALSE)'

cran: $(TAR)
	R -e 'devtools::check_built(path = "$(TAR)", cran = TRUE, remote = TRUE)'

test:
	R -e 'devtools::test(pkg = "pkg/")'

.PHONY: all document manual build install check cran description test
