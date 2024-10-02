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


<!-- Template für Html Header -->
<xsl:template match="/">
<html>
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
<body>
    <h1><xsl:value-of select="@name"/></h1>
    <h2>Platform: <xsl:value-of select="@platform"/></h2>

    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <h3>Functions</h3>
        </xsl:when>
        <xsl:otherwise>
            <h3>Funktionen</h3>
        </xsl:otherwise>
    </xsl:choose>
    <table>
        <tr>
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <th>Return Type</th>
                    <th>Function Name</th>
                    <th>Summary</th>
                </xsl:when>
                <xsl:otherwise>
                    <th>Rückgabetyp</th>
                    <th>Funktion-Name</th>
                    <th>Kurzbeschreibung</th>
                </xsl:otherwise>
            </xsl:choose>
        </tr>
        <tr>
            <!-- Erste Zeile für Rückgabetyp, Funktionsname und Zusammenfassung -->
            <td><xsl:value-of select="@name" />yyy</td>
            <td><xsl:value-of select="@name" /></td>
            <td>
                <xsl:if test="devnotes/summary">
                    <xsl:value-of select="devnotes/summary"/>
                </xsl:if>
            </td>
        </tr>
        <tr>
            <!-- Zweite Zeile für Parameter -->
            <td colspan="3">
                <strong>
                    <xsl:choose>
                        <xsl:when test="$lang = 'en'">Parameters:</xsl:when>
                        <xsl:otherwise>Parameter:</xsl:otherwise>
                    </xsl:choose>
                </strong>
                <xsl:for-each select="parameters/parameter">
                    <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
                    <xsl:if test="position() != last()">, </xsl:if>
                </xsl:for-each>
                <xsl:if test="not(parameters/parameter)">
                    <!-- Falls keine Parameter vorhanden sind -->
                    <xsl:choose>
                        <xsl:when test="$lang = 'en'">no param</xsl:when>
                        <xsl:otherwise>
                            <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                            <span class="no_params">keine</span>
                        </xsl:otherwise>
                    </xsl:choose>
                </xsl:if>
            </td>
        </tr>
        <tr>
            <!-- Zusätzliche Zeile nach den Parametern -->
            <td colspan="3"> <!-- Hier kannst du zusätzlichen Inhalt einfügen -->
                <xsl:choose>
                    <xsl:when test="$lang = 'en'">Additional Information or Notes</xsl:when>
                    <xsl:otherwise>
                        <div class="addi_td">
                            <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                        </div>
                    </xsl:otherwise>
                </xsl:choose>
            </td>
        </tr>
    </table>
</body>
</html>
</xsl:template>

</xsl:stylesheet>
