<?xml version='1.0'?> 
<xsl:stylesheet  
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform" 
    xmlns:fo="http://www.w3.org/1999/XSL/Format"
    version="1.0"> 

<!-- <xsl:import href="fo/docbook.xsl"/> -->
<xsl:import href="http://asciidoc.local/docbook-xsl/fo.xsl"/> 

<xsl:template match="lineannotation">
    <fo:inline font-style="italic">
        <xsl:call-template name="inline.charseq"/>
    </fo:inline>
</xsl:template>
<xsl:param name="page.margin.top" select="'0.5in'"/>
<xsl:param name="page.margin.bottom" select="'0.5in'"/>

<!-- <xsl:param name="draft.mode" select="'yes'"/> -->
<xsl:param name="header.logo.image.filename" select="'./images/gmt-logo.png'"/>
<xsl:param name="footer.confidential" select="'Confidential'" />
<xsl:param name="footer.copyright" select="'&#x00a9; Gemini Mobile Technologies.  All rights reserved.'"/>
<xsl:param name="corporate.documentid" select="'TBD_ID'"/>


<xsl:attribute-set name="footer.content.properties">
  <xsl:attribute name="font-family">Helvetica</xsl:attribute>
  <xsl:attribute name="font-size">9pt</xsl:attribute>
  <xsl:attribute name="font-weight">normal</xsl:attribute>
  <xsl:attribute name="text-align">center</xsl:attribute>
</xsl:attribute-set>

<xsl:param name="footer.column.widths">1 6 1</xsl:param>

<xsl:attribute-set name="revhistory.table.cell.properties">
  <xsl:attribute name="border-style">solid</xsl:attribute>
  <xsl:attribute name="border-width">0.2mm</xsl:attribute>
  <xsl:attribute name="padding">1mm</xsl:attribute>
  <xsl:attribute name="font-size">9pt</xsl:attribute>
</xsl:attribute-set>

<xsl:param name="body.font.master">10</xsl:param>
<xsl:param name="line-height">1.5</xsl:param>


<xsl:attribute-set name="list.item.spacing">
  <xsl:attribute name="space-before.optimum">0.2em</xsl:attribute>
  <xsl:attribute name="space-before.minimum">0.0em</xsl:attribute>
  <xsl:attribute name="space-before.maximum">0.4em</xsl:attribute>
  <xsl:attribute name="space-after.optimum">0.0em</xsl:attribute>
  <xsl:attribute name="space-after.minimum">0.0em</xsl:attribute>
  <xsl:attribute name="space-after.maximum">0.0em</xsl:attribute>
</xsl:attribute-set>

<xsl:attribute-set name="monospace.verbatim.properties" use-attribute-sets="verbatim.properties monospace.properties">
  <xsl:attribute name="text-align">start</xsl:attribute>
  <xsl:attribute name="wrap-option">wrap</xsl:attribute>
  <xsl:attribute name="line-height">1.1</xsl:attribute>
</xsl:attribute-set>

<!-- ============================================================ -->

<!--
<xsl:template match="title" mode="article.titlepage.recto.auto.mode">
  <fo:block xmlns:fo="http://www.w3.org/1999/XSL/Format" xsl:use-attribute-sets="article.titlepage.recto.style" keep-with-next.within-column="always" font-size="24.8832pt" font-weight="bold">
    <fo:external-graphic content-height="1.8cm">
      <xsl:attribute name="src">
        <xsl:call-template name="fo-external-image">
          <xsl:with-param name="filename" select="$header.logo.image.filename"/>
        </xsl:call-template>
      </xsl:attribute>
    </fo:external-graphic>
    <xsl:call-template name="component.title">
      <xsl:with-param name="node" select="ancestor-or-self::article[1]"/>
    </xsl:call-template>
  </fo:block>
</xsl:template>
-->
<xsl:template name="article.titlepage.before.recto">
    <fo:external-graphic content-height="1.8cm">
      <xsl:attribute name="src">
        <xsl:call-template name="fo-external-image">
          <xsl:with-param name="filename" select="$header.logo.image.filename"/>
        </xsl:call-template>
      </xsl:attribute>
    </fo:external-graphic>
</xsl:template>

<xsl:template name="book.titlepage.before.recto">
    <fo:external-graphic content-height="1.8cm">
      <xsl:attribute name="src">
        <xsl:call-template name="fo-external-image">
          <xsl:with-param name="filename" select="$header.logo.image.filename"/>
        </xsl:call-template>
      </xsl:attribute>
    </fo:external-graphic>
</xsl:template>

<!-- ============================================================ -->
<xsl:template match="revhistory" mode="titlepage.mode">

  <xsl:variable name="explicit.table.width">
    <xsl:call-template name="pi.dbfo_table-width"/>
  </xsl:variable>

  <xsl:variable name="table.width">
    <xsl:choose>
      <xsl:when test="$explicit.table.width != ''">
        <xsl:value-of select="$explicit.table.width"/>
      </xsl:when>
      <xsl:when test="$default.table.width = ''">
        <xsl:text>100%</xsl:text>
      </xsl:when>
      <xsl:otherwise>
        <xsl:value-of select="$default.table.width"/>
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable>

  <fo:block font-size="24pt" color="white">
  <xsl:text>.</xsl:text>
  </fo:block>
  <fo:table border-style="solid" border-width="0.5mm" padding="1mm" table-layout="fixed" width="{$table.width}" xsl:use-attribute-sets="revhistory.table.properties">
    <fo:table-column column-number="1" column-width="12mm"/>
    <fo:table-column column-number="2" column-width="30mm"/>
    <fo:table-column column-number="3" column-width="12mm"/>
    <fo:table-column column-number="4" />
    <fo:table-body start-indent="0pt" end-indent="0pt">
      <fo:table-row>
        <fo:table-cell number-columns-spanned="4" border-width="0.5mm" font-size="10.5pt" xsl:use-attribute-sets="revhistory.table.cell.properties">
          <fo:block xsl:use-attribute-sets="revhistory.title.properties">
            <xsl:call-template name="gentext">
              <xsl:with-param name="key" select="'RevHistory'"/>
            </xsl:call-template>
          </fo:block>
        </fo:table-cell>
      </fo:table-row>
      <xsl:apply-templates mode="titlepage.mode"/>
    </fo:table-body>
  </fo:table>
</xsl:template>

<xsl:template match="revhistory/revision" mode="titlepage.mode">
  <xsl:variable name="revnumber" select="revnumber"/>
  <xsl:variable name="revdate"   select="date"/>
  <xsl:variable name="revauthor" select="authorinitials|author"/>
  <xsl:variable name="revremark" select="revremark|revdescription"/>
  <fo:table-row>
    <fo:table-cell text-align="left" xsl:use-attribute-sets="revhistory.table.cell.properties">
      <fo:block>
        <xsl:if test="$revnumber">
        <!--
          <xsl:call-template name="gentext">
            <xsl:with-param name="key" select="'Revision'"/>
          </xsl:call-template>
          <xsl:call-template name="gentext.space"/>
        -->
          <xsl:apply-templates select="$revnumber[1]" mode="titlepage.mode"/>
        </xsl:if>
      </fo:block>
    </fo:table-cell>
    <fo:table-cell text-align="left" xsl:use-attribute-sets="revhistory.table.cell.properties">
      <fo:block>
        <xsl:apply-templates select="$revdate[1]" mode="titlepage.mode"/>
      </fo:block>
    </fo:table-cell>
    <fo:table-cell text-align="left" xsl:use-attribute-sets="revhistory.table.cell.properties">
      <fo:block>
        <xsl:for-each select="$revauthor">
          <xsl:apply-templates select="." mode="titlepage.mode"/>
          <xsl:if test="position() != last()">
            <xsl:text>, </xsl:text>
          </xsl:if>
        </xsl:for-each>
      </fo:block>
    </fo:table-cell>
    <xsl:if test="$revremark">
      <fo:table-cell text-align="left" xsl:use-attribute-sets="revhistory.table.cell.properties">
        <fo:block>
          <xsl:apply-templates select="$revremark[1]" mode="titlepage.mode"/>
        </fo:block>
      </fo:table-cell>
    </xsl:if>
<!--
    <xsl:otherwise>
        <fo:table-cell>
        <fo:block> </fo:block>
        </fo:table-cell>
    </xsl:otherwise>
-->
  </fo:table-row>
</xsl:template>

<!-- ============================================================ -->

<xsl:template name="header.content">  
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="position" select="''"/>
  <xsl:param name="gentext-key" select="''"/>

  <fo:block>
    <!-- sequence can be odd, even, first, blank -->
    <!-- position can be left, center, right -->
    <xsl:choose>

      <xsl:when test="$sequence = 'odd' and $position = 'left'">
        <!--<xsl:call-template name="draft.text"/>-->
        <fo:block>
		  <fo:external-graphic content-height="0.6cm">
		    <xsl:attribute name="src">
		      <xsl:call-template name="fo-external-image">
		        <xsl:with-param name="filename" select="$header.logo.image.filename"/>
		      </xsl:call-template>
		    </xsl:attribute>
		  </fo:external-graphic>
		</fo:block>
      </xsl:when>

      <xsl:when test="$sequence = 'odd' and $position = 'center'">
        <fo:retrieve-marker retrieve-class-name="section.head.marker"
                            retrieve-position="first-including-carryover"
                            retrieve-boundary="page-sequence"/>
      </xsl:when>

      <xsl:when test="$sequence = 'odd' and $position = 'right'">
        <fo:page-number/>
      </xsl:when>

      <xsl:when test="$sequence = 'even' and $position = 'left'">
        <fo:page-number/>
      </xsl:when>

      <xsl:when test="$sequence = 'even' and $position = 'center'">
        <!--<xsl:call-template name="draft.text"/>-->
  <fo:external-graphic content-height="0.6cm">
    <xsl:attribute name="src">
      <xsl:call-template name="fo-external-image">
        <xsl:with-param name="filename" select="$header.logo.image.filename"/>
      </xsl:call-template>
    </xsl:attribute>
  </fo:external-graphic>
  <fo:external-graphic content-height="0.6cm">
    <xsl:attribute name="src">
      <xsl:call-template name="fo-external-image">
        <xsl:with-param name="filename" select="$header.logo.image.filename"/>
      </xsl:call-template>
    </xsl:attribute>
  </fo:external-graphic>
      </xsl:when>

      <xsl:when test="$sequence = 'even' and $position = 'right'">
        <xsl:apply-templates select="." mode="titleabbrev.markup"/>
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'left'">
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'right'">
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'center'">
        <xsl:value-of select="ancestor-or-self::book/bookinfo/corpauthor"/>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'left'">
        <fo:page-number/>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'center'">
        <xsl:text>This page intentionally left blank</xsl:text>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'right'">
      </xsl:when>


    </xsl:choose>
  </fo:block>
</xsl:template>

<xsl:template name="footer.content">  
  <xsl:param name="pageclass" select="''"/>
  <xsl:param name="sequence" select="''"/>
  <xsl:param name="position" select="''"/>
  <xsl:param name="gentext-key" select="''"/>

  <fo:block>
    <!-- sequence can be odd, even, first, blank -->
    <!-- position can be left, center, right -->
    <xsl:choose>

      <xsl:when test="$position = 'left'">
        <fo:block color="blue" font-size="12pt" font-weight="bold" font-style="italic">
          <xsl:value-of select="$footer.confidential"/>
        </fo:block>
      </xsl:when>

      <xsl:when test="$position = 'center'">
        <xsl:value-of select="$footer.copyright"/>
      </xsl:when>

      <xsl:when test="$position = 'right'">
        ID:<xsl:value-of select="$corporate.documentid"/>
      </xsl:when>

<!--
      <xsl:when test="$sequence = 'even' and $position = 'left'">
        <fo:page-number/>
      </xsl:when>

      <xsl:when test="$sequence = 'even' and $position = 'center'">
  <fo:external-graphic content-height="0.8cm">
    <xsl:attribute name="src">
      <xsl:call-template name="fo-external-image">
        <xsl:with-param name="filename" select="$header.logo.image.filename"/>
      </xsl:call-template>
    </xsl:attribute>
  </fo:external-graphic>
  <fo:external-graphic content-height="0.8cm">
    <xsl:attribute name="src">
      <xsl:call-template name="fo-external-image">
        <xsl:with-param name="filename" select="$header.logo.image.filename"/>
      </xsl:call-template>
    </xsl:attribute>
  </fo:external-graphic>
      </xsl:when>

      <xsl:when test="$sequence = 'even' and $position = 'right'">
        <xsl:apply-templates select="." mode="titleabbrev.markup"/>
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'left'">
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'right'">
      </xsl:when>

      <xsl:when test="$sequence = 'first' and $position = 'center'">
        <xsl:value-of select="ancestor-or-self::book/bookinfo/corpauthor"/>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'left'">
        <fo:page-number/>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'center'">
        <xsl:text>This page intentionally left blank</xsl:text>
      </xsl:when>

      <xsl:when test="$sequence = 'blank' and $position = 'right'">
      </xsl:when>
-->


    </xsl:choose>
  </fo:block>
</xsl:template>

</xsl:stylesheet>
