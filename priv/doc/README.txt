
ADOC is a documentation environment using AsciiDoc as core component.
ADOC makes it easy to construct AsciiDoc documentation system.
ADOC can make single HTML, chunked HTML, PDF from one plain text data.
ADOC supports English and Japanese.

See http://www.methods.co.nz/asciidoc/ for further details.


== Install ADOC environment

=== Mac OS X - 10.5

1. install brew (http://mxcl.github.com/homebrew/)
2. install git brew package
   $ brew install git
3. install the following brew packages for ADOC
   $ brew install ImageMagick
   $ brew install asciidoc
   $ brew install docbook
   $ brew install graphviz
   $ brew install mscgen
   $ brew install xmlto

=== Linux - Fedora 14

TO BE ADDED

=== Windows - CYGWIN

TO BE ADDED


== Download UBF's (source code & documentation) repository

$ mkdir working-directory-name
$ cd working-directory-name
$ git clone git://github.com/norton/ubf.git


== Build UBF's documentation guides

$ mkdir working-directory-name
$ cd working-directory-name/priv/doc/src/ubf
$ make clean -OR- make realclean
$ make

HTML documentation is written in the "./public_html" directory.


== Build UBF's website pages

$ mkdir working-directory-name
$ cd working-directory-name/priv/doc/src/ubf/website
$ make clean -OR- make realclean
$ make

HTML documentation is written in the "./public_html" directory.


== NOTES

1. The above recipe was tested on Mac OS X 10.5 using asciidoc 8.6.1
   with a modified version of the a2x tool.

$ diff -u /usr/local/Cellar/asciidoc/8.6.1/bin/a2x.orig /usr/local/Cellar/asciidoc/8.6.1/bin/a2x
--- /usr/local/Cellar/asciidoc/8.6.1/bin/a2x.orig	2010-12-22 00:01:56.000000000 +0900
+++ /usr/local/Cellar/asciidoc/8.6.1/bin/a2x	2010-12-22 00:01:41.000000000 +0900
@@ -156,7 +156,10 @@
 def shell_copy(src, dst):
     verbose('copying "%s" to "%s"' % (src,dst))
     if not OPTIONS.dry_run:
-        shutil.copy(src, dst)
+        try:
+		shutil.copy(src, dst)
+	except shutil.Error:
+		return

 def shell_rm(path):
     if not os.path.exists(path):
