
ASCIIDOC_PATH = ../../make
include $(ASCIIDOC_PATH)/asciidoc.mk

all: html

html: public-html-hack

public-html-hack:
	mkdir -p ./public_html/images
	(cd ./public_html && ln -sf ../misc-codes ./)
