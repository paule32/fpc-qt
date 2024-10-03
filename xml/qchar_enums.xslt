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
    xxx
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
        <xsl:if test="class/members/enum">
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <h2>enum: <xsl:value-of select="@name"/></h2>
                </xsl:when>
                <xsl:otherwise>
                    <h2>Aufzählung: <xsl:value-of select="@name"/></h2>
                </xsl:otherwise>
            </xsl:choose>
            <xsl:if test="devnotes/enum">
                <p>
                <div class="devnotes">
                    <strong>Hinweise:</strong><br/>
                    <xsl:value-of select="devnotes/enum"/>
                </div>
                </p>
            </xsl:if>
            <table>
                <tr>
                    <xsl:choose>
                        <xsl:when test="$lang = 'en'">
                            <th>Constant</th>
                            <th>Value</th>
                        </xsl:when>
                        <xsl:otherwise>
                            <th>Konstante</th>
                            <th>Wert</th>
                        </xsl:otherwise>
                    </xsl:choose>
                </tr>
                <xsl:for-each select="class/members/enum/element">
                <tr>
                    <td class="name-cell"><xsl:value-of select="@name"/></td>
                    <td class="value-cell"><xsl:value-of select="@value"/></td>
                </tr>
                </xsl:for-each>
            </table>
        </xsl:if>
    </div>
</xsl:template>

</xsl:stylesheet>
