HTML = $(patsubst %.asciidoc,%.html,$(shell find . -depth 1 -name \*.asciidoc))
PDF = $(patsubst %.asciidoc,%.pdf,$(shell find . -depth 1 -name \*.asciidoc))

all: html

html: $(HTML)

pdf: $(PDF)

gh-pages: clean html
	git co gh-pages
	rsync -avz ./*.html ../../../
	rsync -avz --exclude=.git --delete ./bootstrap-docs/bootstrap/ ../../../bootstrap/

clean:
	rm -f $(HTML) $(PDF)

%.html: %.asciidoc Makefile
	cd $(<D) && asciidoc $(<F)
	@perl -i -pe 's~class="brand" href="#"~class="brand" href="http://ubf.github.com/ubf/"~;' *.html

%.pdf: %.asciidoc Makefile
	cd $(<D) && a2x -f pdf $(<F)

.PHONY: all html pdf clean
