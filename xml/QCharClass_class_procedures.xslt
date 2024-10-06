<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8" />

    <xsl:variable name="lang"  select="'de'"/>
    <xsl:variable name="theme" select="'dark'"/>

    <xsl:variable name="namespace_text_en">Namespace</xsl:variable>
    <xsl:variable name="namespace_text_de">Namensraum</xsl:variable>

    <xsl:variable name="contains_text_en">Contains</xsl:variable>
    <xsl:variable name="contains_text_de">Enthält</xsl:variable>

    <xsl:variable name="remarks_text_en">Remarks</xsl:variable>
    <xsl:variable name="remarks_text_de">Bemerkungen</xsl:variable>

<!-- Template für Hauptseite -->
<xsl:template match="/">
<html>
<head>
    <title>
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <xsl:text>Delphi Documentation for Project</xsl:text>
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>Delphi Dokumentation für Projekt</xsl:text>
        </xsl:otherwise>
    </xsl:choose>
    </title>
    <xsl:choose>
        <xsl:when test="$theme = 'light'">
            <link rel="stylesheet" type="text/css" href="style_light.css" />
        </xsl:when>
        <xsl:when test="$theme = 'dark'">
            <link rel="stylesheet" type="text/css" href="style_dark.css" />
        </xsl:when>
    </xsl:choose>
</head>
<body>
    <div id="container">
    <h1>
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <xsl:text>Delphi Documentation for Project</xsl:text>
        </xsl:when>
        <xsl:otherwise>
            <xsl:text>Delphi Dokumentation für Projekt</xsl:text>
        </xsl:otherwise>
    </xsl:choose>
    </h1>
    <xsl:apply-templates select="namespace" />
    <!-- comntainer -->
    </div>
</body>
</html>
</xsl:template>

<xsl:template match="namespace">
    <div class="namespace">
        <h2>
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <xsl:value-of select="$namespace_text_en"/>: <xsl:value-of select="@name"/>
                </xsl:when>
                <xsl:otherwise>
                    <xsl:value-of select="$namespace_text_de"/>: <xsl:value-of select="@name"/>
                </xsl:otherwise>
            </xsl:choose>
        </h2>
        <div class="platform">Platform: <xsl:value-of select="@platform"/></div>
    </div>
    <xsl:apply-templates select="class" />
</xsl:template>

<xsl:template match="class">        
    <!-- Tabelle für Prozeduren -->
    <xsl:if test="members/procedure">
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <h3>Procedures</h3>
        </xsl:when>
        <xsl:otherwise>
            <h3>Prozeduren</h3>
        </xsl:otherwise>
    </xsl:choose>
    <table>
        <tr>
        <xsl:choose>
            <xsl:when test="$lang = 'en'">
                <th>Procedure</th>
                <th>Summary</th>
                <th>Parameters</th>
            </xsl:when>
            <xsl:otherwise>
                <th>Prozedur</th>
                <th>Kurzbeschreibung</th>
                <th>Parameter</th>
            </xsl:otherwise>
        </xsl:choose>
        </tr>
        <xsl:for-each select="members/procedure">
            <tr>
                <td><xsl:value-of select="@name"/></td>
                <td>
                    <xsl:if test="devnotes/summary">
                        <xsl:value-of select="devnotes/summary"/>
                    </xsl:if>
                </td>
                <td>
                    <xsl:for-each select="parameters/parameter">
                        <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
                        <xsl:if test="position() != last()">, </xsl:if>
                    </xsl:for-each>
                </td>
            </tr>
        </xsl:for-each>
    </table>
    </xsl:if>
</xsl:template>

</xsl:stylesheet>
