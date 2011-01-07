##
## Copyright (c) 2005-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http:##www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##

.PHONY: html man pdf text chunked outputs img tbl clean realclean imagesclean outputsclean alldoc

# use 'RM_TEMP_FILES=:' to reserve temp files
RM_TEMP_FILES ?= rm -f

ASCIIDOC_EXT ?= txt
SOURCES = $(wildcard *.$(ASCIIDOC_EXT))
SVG_IMG_SOURCE_EXTS ?= svg msc dot dia
SVG_IMG_SOURCE_LOGS ?= vmstat-*.log
PNG_IMG_SOURCE_EXTS ?= png svg msc dot dia
PNG_IMG_SOURCE_LOGS ?= vmstat-*.log
CSV_TBL_SOURCE_EXT ?= csv

export OUTPUTS = ./public_html
IMAGES = ./public_html/images

SVG_IMG_SOURCES = \
	$(foreach ext,$(SVG_IMG_SOURCE_EXTS),$(patsubst images-src/%.$(ext),%,$(wildcard images-src/*.$(ext)))) \
	$(foreach pat,$(SVG_IMG_SOURCE_LOGS),$(patsubst images-src/%.log,%,$(wildcard images-src/$(pat))))
PNG_IMG_SOURCES = \
	$(foreach ext,$(PNG_IMG_SOURCE_EXTS),$(patsubst images-src/%.$(ext),%,$(wildcard images-src/*.$(ext)))) \
	$(foreach pat,$(PNG_IMG_SOURCE_LOGS),$(patsubst images-src/%.log,%,$(wildcard images-src/$(pat))))

HTML_TARGETS = $(patsubst %,$(OUTPUTS)/%,$(subst .$(ASCIIDOC_EXT),.html,$(SOURCES)))
MAN_TARGETS = $(patsubst %,$(OUTPUTS)/%,$(subst .$(ASCIIDOC_EXT),.man,$(filter manpage-%,$(SOURCES))))
PDF_TARGETS = $(patsubst %,$(OUTPUTS)/%,$(subst .$(ASCIIDOC_EXT),.pdf,$(SOURCES)))
TEXT_TARGETS = $(patsubst %,$(OUTPUTS)/%,$(subst .$(ASCIIDOC_EXT),.txt,$(SOURCES)))
CHUNKED_TARGETS = $(patsubst %,$(OUTPUTS)/%,$(subst .$(ASCIIDOC_EXT),.chunked,$(filter book-%,$(SOURCES))))
IMG_TARGETS = \
	$(patsubst %,$(IMAGES)/%.svg,$(SVG_IMG_SOURCES)) \
	$(patsubst %,$(IMAGES)/%.png,$(PNG_IMG_SOURCES))

all: html
alldoc: html man pdf text chunked

html: $(OUTPUTS) img tbl $(HTML_TARGETS)
man: $(OUTPUTS) img tbl $(MAN_TARGETS)
pdf: $(OUTPUTS) img tbl $(PDF_TARGETS)
text: $(OUTPUTS) img tbl $(TEXT_TARGETS)
chunked: $(OUTPUTS) img tbl $(CHUNKED_TARGETS)

img: $(OUTPUTS) $(IMAGES) $(IMG_TARGETS)
tbl: $(OUTPUTS) $(wildcard table-src/*.$(CSV_TBL_SOURCE_EXT))

$(IMAGES):
	if [ ! -d $(IMAGES) ]; then mkdir $(IMAGES); fi
	rm -fr ./images
	ln -s $(IMAGES) .
	if ls *.def >/dev/null 2>&1; then \
		cp $(ASCIIDOC_PATH)/common-images/* $(IMAGES)/; \
	fi
$(OUTPUTS):
	if [ ! -d $(OUTPUTS) ]; then mkdir $(OUTPUTS); fi

clean:
	rm -f $(HTML_TARGETS) $(MAN_TARGETS) $(PDF_TARGETS) $(TEXT_TARGETS)
	rm -fr $(CHUNKED_TARGETS)
	rm -f $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$(SOURCES)))
	rm -f docbook-xsl.css
	rm -fr .deps
outputsclean:
	rm -fr $(OUTPUTS)
imagesclean:
	rm -fr $(IMAGES)
	rm -fr ./images
gomiclean:
	rm -f *~ */*~

realclean: clean imagesclean outputsclean gomiclean

ASCIIDOC_PATH ?= ../../tools/asciidoc

ifdef COMSPEC
  FOP_JA_CONF = $(shell cygpath -ma $(ASCIIDOC_PATH)/fop-ja-cygwin.conf)
else
  FOP_JA_CONF = $(ASCIIDOC_PATH)/fop-ja.conf
endif
FOP_JA_OPTS = --fop-opts="-c $(FOP_JA_CONF)"
TR_JA_FONT = $(ASCIIDOC_PATH)/tr-ja-font
MKG_VMSTAT = $(ASCIIDOC_PATH)/utils/mkg-vmstat.sh

ADOC_CONF = -f $(ASCIIDOC_PATH)/asciidoc-base.conf -f $(ASCIIDOC_PATH)/base-docbook.conf

LANG_JA = -a lang=ja -a lang-ja
LANG_EN = -a lang=en -a lang-en

export XSLSTYLE_PATH = $(ASCIIDOC_PATH)

A2XP = $(ASCIIDOC_PATH)/utils/a2xp a2x --no-xmllint

#--------------------------------------------------------------------------------
# DOCUMENT PROCESSING
#--------------------------------------------------------------------------------
ADOC_OPTS = --unsafe -b docbook $(ADOC_CONF) $(ADOCFLAGS)
A2X_OPTS = --copy --icons -D $(OUTPUTS) $(A2XFLAGS)


#--------------------
# html
#--------------------
ADOC_OPTS_INDEX_HTML = $(ADOC_OPTS) -d article
ADOC_ATRS_INDEX_HTML = -a icons -a theme=xhtml11

ADOC_OPTS_BOOK_HTML = $(ADOC_OPTS) -d book
ADOC_ATRS_BOOK_HTML = -a toc -a icons -a numbered -a theme=xhtml11

ADOC_OPTS_ARTICLE_HTML = $(ADOC_OPTS) -d article
ADOC_ATRS_ARTICLE_HTML = -a toc -a icons -a numbered -a theme=xhtml11

ADOC_OPTS_WEBPAGE_HTML = $(ADOC_OPTS) -d webpage
ADOC_ATRS_WEBPAGE_HTML = -a icons -a theme=xhtml11

ADOC_OPTS_MANPAGE_HTML = $(ADOC_OPTS) -d manpage
ADOC_ATRS_MANPAGE_HTML =

A2X_OPTS_INDEX_HTML = -f xhtml $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_INDEX_HTML)" $(ADOC_ATRS_INDEX_HTML)
A2X_OPTS_BOOK_HTML = -f xhtml $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_BOOK_HTML)" $(ADOC_ATRS_BOOK_HTML)
A2X_OPTS_ARTICLE_HTML = -f xhtml $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_ARTICLE_HTML)" $(ADOC_ATRS_ARTICLE_HTML)
A2X_OPTS_WEBPAGE_HTML = -f xhtml $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_INDEX_HTML)" $(ADOC_ATRS_INDEX_HTML)
A2X_OPTS_MANPAGE_HTML = -f xhtml $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_MANPAGE_HTML)" $(ADOC_ATRS_MANPAGE_HTML)

$(OUTPUTS)/index.ja.html: index.ja.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_INDEX_HTML) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/index.html: index.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_INDEX_HTML) $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/book-%.ja.html: book-%.ja.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_BOOK_HTML) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/book-%.html: book-%.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_BOOK_HTML) $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/%.ja.html: %.ja.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_ARTICLE_HTML) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/%.html: %.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_ARTICLE_HTML) $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/webpage-%.ja.html: webpage-%.ja.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_WEBPAGE_HTML) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/webpage-%.html: webpage-%.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_WEBPAGE_HTML) $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)

$(OUTPUTS)/manpage-%.html: manpage-%.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_MANPAGE_HTML) $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(patsubst %.$(ASCIIDOC_EXT),%.xml,$<)


#--------------------
# man
#--------------------
ADOC_OPTS_MANPAGE_MAN = $(ADOC_OPTS) -d manpage
ADOC_ATRS_MANPAGE_MAN =

A2X_OPTS_MANPAGE_MAN = -f manpage $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_MANPAGE_PDF)" $(ADOC_ATRS_MANPAGE_PDF)

$(OUTPUTS)/%.man: %.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_MANPAGE_MAN) $(LANG_EN) $<
	H=`head -1 $<` && \
		SEC=`expr $$H : ".*(\([1-9]\))"` && \
		NAME=`expr $@ : ".*/manpage-\(.*\).man"` && \
		mv $(OUTPUTS)/$$NAME.$$SEC $@


#--------------------
# pdf
#--------------------
ADOC_OPTS_BOOK_PDF = $(ADOC_OPTS) -d book
ADOC_ATRS_BOOK_PDF = -a toc -a icons

ADOC_OPTS_ARTICLE_PDF = $(ADOC_OPTS) -d article
ADOC_ATRS_ARTICLE_PDF = -a toc -a icons

ADOC_OPTS_MANPAGE_PDF = $(ADOC_OPTS) -d manpage
ADOC_ATRS_MANPAGE_PDF =

A2X_OPTS_BOOK_PDF = -f pdf $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_BOOK_PDF)" $(ADOC_ATRS_BOOK_PDF)
A2X_OPTS_ARTICLE_PDF = -f pdf $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_ARTICLE_PDF)" $(ADOC_ATRS_ARTICLE_PDF)
A2X_OPTS_MANPAGE_PDF = -f pdf $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_MANPAGE_PDF)" $(ADOC_ATRS_MANPAGE_PDF)

MK_ICONS = if [ ! -d $(IMAGES)/icons ]; then \
		echo -e "= tmp\n\ntmp" > tmp-$$$$.tmp; \
		$(A2XP) -f xhtml --copy --icons tmp-$$$$.tmp; \
		rm -f tmp-$$$$.*; \
	fi

$(OUTPUTS)/book-%.ja.pdf: book-%.ja.$(ASCIIDOC_EXT)
	$(MK_ICONS)
	$(A2XP) $(A2X_OPTS_BOOK_PDF) $(FOP_JA_OPTS) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$<))

$(OUTPUTS)/book-%.pdf: book-%.$(ASCIIDOC_EXT)
	$(MK_ICONS)
	$(A2XP) $(A2X_OPTS_BOOK_PDF) --fop-opts="" $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$<))

$(OUTPUTS)/%.ja.pdf: %.ja.$(ASCIIDOC_EXT)
	$(MK_ICONS)
	$(A2XP) $(A2X_OPTS_ARTICLE_PDF) $(FOP_JA_OPTS) $(LANG_JA) $< \
		&& $(RM_TEMP_FILES) $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$<))

$(OUTPUTS)/%.pdf: %.$(ASCIIDOC_EXT)
	$(MK_ICONS)
	$(A2XP) $(A2X_OPTS_ARTICLE_PDF) --fop-opts="" $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$<))

$(OUTPUTS)/manpage-%.pdf: manpage-%.$(ASCIIDOC_EXT)
	$(A2XP) $(A2X_OPTS_MANPAGE_PDF) --fop-opts="" $(LANG_EN) $< \
		&& $(RM_TEMP_FILES) $(foreach ext,fo xml,$(subst .$(ASCIIDOC_EXT),.$(ext),$<))


#--------------------
# text
#--------------------
ADOC_OPTS_BOOK_TEXT = $(ADOC_OPTS) -d book
ADOC_ATRS_BOOK_TEXT = -a toc -a icons -a numbered -a theme=xhtml11

ADOC_OPTS_ARTICLE_TEXT = $(ADOC_OPTS) -d article
ADOC_ATRS_ARTICLE_TEXT = -a toc -a icons -a numbered -a theme=xhtml11

ADOC_OPTS_MANPAGE_TEXT = $(ADOC_OPTS) -d manpage
ADOC_ATRS_MANPAGE_TEXT =

A2X_OPTS_BOOK_TEXT = -f text $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_BOOK_TEXT)" $(ADOC_ATRS_BOOK_TEXT)
A2X_OPTS_ARTICLE_TEXT = -f text $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_ARTICLE_TEXT)" $(ADOC_ATRS_ARTICLE_TEXT)
A2X_OPTS_MANPAGE_TEXT = -f text $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_MANPAGE_TEXT)" $(ADOC_ATRS_MANPAGE_TEXT)

$(OUTPUTS)/book-%.ja.txt: book-%.ja.$(ASCIIDOC_EXT)
	-rm -f $@ $(patsubst %.txt,%.text,$@)
	$(A2XP) $(A2X_OPTS_BOOK_TEXT) $(LANG_JA) $<
	mv $(patsubst %.txt,%.text,$@) $@

$(OUTPUTS)/book-%.txt: book-%.$(ASCIIDOC_EXT)
	-rm -f $@ $(patsubst %.txt,%.text,$@)
	$(A2XP) $(A2X_OPTS_BOOK_TEXT) $(LANG_EN) $<
	mv $(patsubst %.txt,%.text,$@) $@

$(OUTPUTS)/%.ja.txt: %.ja.$(ASCIIDOC_EXT)
	-rm -f $@ $(patsubst %.txt,%.text,$@)
	$(A2XP) $(A2X_OPTS_ARTICLE_TEXT) $(LANG_JA) $<
	mv $(patsubst %.txt,%.text,$@) $@

$(OUTPUTS)/%.txt: %.$(ASCIIDOC_EXT)
	-rm -f $@ $(patsubst %.txt,%.text,$@)
	$(A2XP) $(A2X_OPTS_ARTICLE_TEXT) $(LANG_EN) $<
	mv $(patsubst %.txt,%.text,$@) $@

$(OUTPUTS)/manpage-%.txt: manpage-%.$(ASCIIDOC_EXT)
	-rm -f $@ $(patsubst %.txt,%.text,$@)
	$(A2XP) $(A2X_OPTS_MANPAGE_TEXT) $(LANG_EN) $<
	mv $(patsubst %.txt,%.text,$@) $@


#--------------------
# chunked
#--------------------
ADOC_OPTS_BOOK_CHUNKED = $(ADOC_OPTS) -d book
ADOC_ATRS_BOOK_CHUNKED = -a toc -a icons -a numbered -a theme=xhtml11

A2X_OPTS_BOOK_CHUNKED = -f chunked $(A2X_OPTS) --asciidoc-opts="$(ADOC_OPTS_BOOK_CHUNKED)" $(ADOC_ATRS_BOOK_CHUNKED)


$(OUTPUTS)/book-%.ja.chunked: book-%.ja.$(ASCIIDOC_EXT)
	if [ ! -d $@/images ]; then mkdir $@; ln -s ../images $@/images; fi
	$(A2XP) $(A2X_OPTS_BOOK_CHUNKED) $(LANG_JA) $<
	touch $@

$(OUTPUTS)/book-%.chunked: book-%.$(ASCIIDOC_EXT)
	if [ ! -d $@/images ]; then mkdir $@; ln -s ../images $@/images; fi
	$(A2XP) $(A2X_OPTS_BOOK_CHUNKED) $(LANG_EN) $<
	touch $@


#--------------------------------------------------------------------------------
# IMAGES PROCESSING
#--------------------------------------------------------------------------------


#--------------------
# vmstat-*.log -> SVG
#--------------------
$(IMAGES)/vmstat-%.svg: images-src/vmstat-%.log
	-rm -f $@
	$(MKG_VMSTAT) $< $@


#--------------------
# msc -> SVG
#--------------------
$(IMAGES)/%.png: images-src/%.msc
	-rm -f $@
	mscgen -T png -i $< -o $@

$(IMAGES)/%.svg: images-src/%.msc
	-rm -f $@
	mscgen -T svg -i $< -o $@-tmp.svg
	echo '<?xml version="1.0" encoding="utf-8" standalone="no"?>' > $@
	SIZE=`identify $@-tmp.svg | awk '{print $$3}' | tr x ' '` && \
	W=`echo $$SIZE | awk '{print $$1}'` && \
	H=`echo $$SIZE | awk '{print $$2}'` && \
		sed -e 's/^\(<svg .*\)/<svg width="'"$${W}pt"'" height="'"$${H}pt"'" version="1.1" viewBox="0 0 '"$$SIZE"'"/g' \
		    < $@-tmp.svg >> $@


#--------------------
# dot -> PNG, SVG
#--------------------
ifdef COMSPEC
  DOT=$(shell ls /cygdrive/*/Program*Files/Graphviz*/bin/dot.exe)
else
  DOT=dot
endif

$(IMAGES)/%.png: images-src/%.dot
	-rm -f $@
	"$(DOT)" -Tpng -o $@ $<

$(IMAGES)/%.svg: images-src/%.dot
	-rm -f $@
	"$(DOT)" -Tsvg -o $@ $<


#--------------------
# dia -> PNG, SVG
#--------------------
ifdef COMSPEC
  DIA=$(shell ls /cygdrive/*/Program*Files/Dia/bin/dia.exe)
else
  DIA=dia
endif

$(IMAGES)/%.png: images-src/%.dia
	-rm -f $@
	"$(DIA)" --export $@ $<

$(IMAGES)/%.svg: images-src/%.dia
	-rm -f $@
	"$(DIA)" --export $@ $<


#--------------------
# others
#--------------------
$(IMAGES)/%.svg: images-src/%.svg
	-rm -f $@
	cp $< $@

$(IMAGES)/%.png: images-src/%.png
	-rm -f $@
	cp $< $@

$(IMAGES)/%.png: $(IMAGES)/%.svg
	-rm -f $@
	-convert $< $@

$(IMAGES)/%.png: images-src/%.svg
	-rm -f $@
	-convert $< $@

# NOTE:
# convert on maguro sometimes dumps core even if it converts the
# image.  just ignore for now.

#
# auto depends
#
ADOCDEPS=$(ASCIIDOC_PATH)/utils/adocdeps
.deps/%.P : %.$(ASCIIDOC_EXT)
	$(ADOCDEPS) $< .P .deps $(OUTPUTS)/ .html .pdf .txt

ifneq "$(MAKECMDGOALS)" "clean"
include $(patsubst %.$(ASCIIDOC_EXT),.deps/%.P,$(SOURCES))
endif

# END
