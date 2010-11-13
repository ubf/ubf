
ASCIIDOC_PATH = ../../../make
include $(ASCIIDOC_PATH)/asciidoc.mk

all: html

html: public-html-hack

public-html-hack:
	mkdir -p ./public_html/images
	#(cd ./public_html/images && ln -sf ../../images-other/*.svg ./)
	#(cd ./public_html/images && ln -sf ../../images-other/*.png ./)
	(cd ./public_html && ln -sf ../misc-codes ./)
	(cd ./public_html && ln -sf ../BERTandBERT-RPC1.0Specification.mht ./)
