
## Add your modules here

EHTML = downloads eserve messagebox \
	javac home white irc file ubfc ubfb ubfb_quick ubfa ubfa_quick quick

## Do not change below this line

.SUFFIXES: .erl .beam .html .ehtml

TARGETS= $(patsubst %,../site/%.html, $(EHTML))

.erl.jam:	
	erlc $<

../site/%.html: %.ehtml menus.inc
	ehtml2html $<
	# echo here $(notdir $@)
	mv $(notdir $@) ../site

all:  $(TARGETS)

publish:
	cp ../site/* $(HOME)/public_html/ubf/site
	cp ../images/* $(HOME)/public_html/ubf/images

clean:
	- rm -f $(TARGETS)




