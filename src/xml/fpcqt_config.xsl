<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:variable name="lang"  select="'de'"/>
    <xsl:variable name="theme" select="'dark'"/>

    <xsl:variable name="namespace_text_en">Namespace</xsl:variable>
    <xsl:variable name="namespace_text_de">Namensraum</xsl:variable>

    <xsl:variable name="contains_text_en">Contains</xsl:variable>
    <xsl:variable name="contains_text_de">Enthält</xsl:variable>

    <xsl:variable name="remarks_text_en">Remarks</xsl:variable>
    <xsl:variable name="remarks_text_de">Bemerkungen</xsl:variable>

    <xsl:template name="project-name">
        <xsl:choose>
            <xsl:when test="$lang = 'en'">
                Delphi Documentation for Project
            </xsl:when>
            <xsl:otherwise>
                Delphi Dokumentation für Projekt
            </xsl:otherwise>
        </xsl:choose>
    </xsl:template>

    <xsl:template name="html-header">
    <head>
        <title>111
            <xsl:call-template name="project-name">
                aaa
            </xsl:call-template>
        </title>
        <xsl:choose>
            <xsl:when test="$theme = 'light'">
            lll
                <link rel="stylesheet" type="text/css" href="style_light.css" />
            </xsl:when>
            <xsl:when test="$theme = 'dark'">
            ddd
                <link rel="stylesheet" type="text/css" href="style_dark.css" />
            </xsl:when>
        </xsl:choose>
    </head>
    </xsl:template>

</xsl:stylesheet>
