# h/t to @jimhester and @yihui for this parse block:
# https://github.com/yihui/knitr/blob/dc5ead7bcfc0ebd2789fe99c527c7d91afb3de4a/Makefile#L1-L4
# Note the portability change as suggested in the manual:
# https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Writing-portable-packages
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
RM = rm -rf
RCMD = R --vanilla CMD
RSCRIPT = Rscript --vanilla


all: check clean

docs:
	@ $(RSCRIPT) \
	-e "devtools::document(roclets = c('rd', 'collate', 'namespace'))"

readme:
	@ echo "Rendering README.Rmd"
	@ $(RSCRIPT) \
	-e "Sys.setenv(RSTUDIO_PANDOC='/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools')" \
	-e "options(cli.width = 80L)" \
	-e "rmarkdown::render('README.Rmd', quiet = TRUE)"
	@ $(RM) README.html

test:
	@ $(RSCRIPT) \
	-e "Sys.setenv(ON_JENKINS = 'true', TZ = 'America/Denver')" \
	-e "devtools::test(reporter = 'summary', stop_on_failure = TRUE)"

test_file:
	@ $(RSCRIPT) \
	-e "Sys.setenv(ON_JENKINS = 'true', TZ = 'America/Denver', NOT_CRAN = 'true')" \
	-e "devtools::load_all()" \
	-e "testthat::test_file('$(FILE)', reporter = 'progress', stop_on_failure = TRUE)"

build: docs
	@ cd ..;\
	$(RCMD) build --resave-data $(PKGSRC)

check: build
	@ cd ..;\
	$(RCMD) check --no-manual $(PKGNAME)_$(PKGVERS).tar.gz

objects:
	@ echo "Creating 'data/plotr.rda' ..."
	@ $(RSCRIPT) inst/create-plotr-objects.R
	@ echo "Saving 'data/plotr.rda' ..."

install:
	@ R CMD INSTALL --use-vanilla --preclean --resave-data .

clean:
	@ cd ..;\
	$(RM) $(PKGNAME)_$(PKGVERS).tar.gz $(PKGNAME).Rcheck
