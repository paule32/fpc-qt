<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE xsl:stylesheet>
<xsl:stylesheet
    version="1.0"
    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<!-- Generate HTML -->
<xsl:output
    method      = "html"
    version     = "4.0"
    encoding    = "UTF-8"
    strip-space = "*" />

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
            <xsl:when test="$lang = 'de'">
                <xsl:text>Delphi Dokumentation für Projekt</xsl:text>
            </xsl:when>
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
                    <xsl:text>Delphi Dokumentation11 für Projekt</xsl:text>
                </xsl:otherwise>
            </xsl:choose>
        </h1>
        <xsl:for-each select="//members/enum">
            <xsl:variable name="filename1" select="'QCharClass_class_enums_'" />
            <xsl:variable name="filename2" select="concat($filename1, @name)" />
            <xsl:variable name="filename3" select="concat($filename2, '.html')" />
            <xsl:document href="{$filename3}" method="html">
                <html>
                <head>
                    <title>
                        <xsl:choose>
                            <xsl:when test="$lang = 'en'">
                                <xsl:text>Delphi Documentation for Project</xsl:text>
                            </xsl:when>
                            <xsl:when test="$lang = 'de'">
                                <xsl:text>Delphi Dokumentation für Projekt</xsl:text>
                            </xsl:when>
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
                    <h2>
                        <xsl:choose>
                            <xsl:when test="$lang = 'en'">
                                Enum: <xsl:value-of select="@name" />
                            </xsl:when>
                            <xsl:otherwise>
                                Aufzählung: <xsl:value-of select="@name" />
                            </xsl:otherwise>
                        </xsl:choose>
                    </h2>
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
                        <xsl:for-each select="element">
                            <tr>
                                <td><xsl:value-of select="@name" /></td>
                                <td><xsl:value-of select="@value" /></td>
                            </tr>
                        </xsl:for-each>
                    </table>
                </body>
                </html>
            </xsl:document>
        </xsl:for-each>
    </div>
</body>
</html>
</xsl:template>

</xsl:stylesheet>
