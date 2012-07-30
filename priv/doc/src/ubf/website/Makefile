
ASCIIDOC_PATH = ../../../make
include $(ASCIIDOC_PATH)/asciidoc.mk

all: html

html: public-html-hack

public-html-hack:
	mkdir -p ./public_html/images
	(cd ./public_html && ln -sf ../misc-codes ./)

stage-gh-pages: html
	-rm -rf gh-pages
	git clone -b gh-pages git@github.com:ubf/ubf.git gh-pages
	rsync -aL --delete --exclude=.git public_html/ gh-pages/
	(cd gh-pages && git status)
