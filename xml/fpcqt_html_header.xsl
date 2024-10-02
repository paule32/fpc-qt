<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8" />

<!-- Template für Html Header -->
<xsl:template match="html-header">
<head>
    <title>
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            Delphi Documentation for Project: <xsl:value-of select="@name"/>
        </xsl:when>
        <xsl:otherwise>
            Delphi Dokumentation für Projekt: <xsl:value-of select="@name"/>
        </xsl:otherwise>
    </xsl:choose>
    </title>
    <!-- Einbinden der CSS-Datei -->
    <xsl:choose>
        <xsl:when test="$theme = 'light'">
            <link rel="stylesheet" type="text/css" href="style_light.css" />
        </xsl:when>
        <xsl:when test="$theme = 'dark'">
            <link rel="stylesheet" type="text/css" href="style_dark.css" />
        </xsl:when>
    </xsl:choose>
</head>
</xsl:template>

</xsl:stylesheet>
