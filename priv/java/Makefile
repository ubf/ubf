
JAVAC = javac
JAVAC_FLAGS = -d classes -classpath classes:src

SOURCES := $(shell find src -name "*.java")
CLASSES = $(patsubst src/%.java,classes/%.class,$(SOURCES))

VERSION=0.3

.PHONY: clean

test:	all
	java -classpath classes ubf.UBFTest

all:	$(CLASSES)

classes/%.class: src/%.java
	$(JAVAC) $(JAVAC_FLAGS) $^

clean:
	-rm -r classes/*
	find . -name "*~" -exec rm {} \;

tarball: clean
	(cd ..; \
	 ln -s ubf ubf-java-${VERSION}; \
	 tar hczf ubf-java-${VERSION}.tar.gz ubf-java-${VERSION}; \
	 rm ubf-java-${VERSION})

