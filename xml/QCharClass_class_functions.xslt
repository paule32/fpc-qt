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
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <h3>Functions</h3>
        </xsl:when>
        <xsl:otherwise>
            <h3>Funktionen</h3>
        </xsl:otherwise>
    </xsl:choose>
        <table>
            <!-- Durchlaufe jede Funktion -->
            <xsl:for-each select="//members/function">
            <tr>
                <xsl:choose>
                    <xsl:when test="$lang = 'en'">
                        <td class="func">Return Type</td>
                        <td class="func">Function Name</td>
                        <td class="func">Summary</td>
                        <td class="func">Visibility</td>
                    </xsl:when>
                    <xsl:otherwise>
                        <td class="func">Rückgabetyp</td>
                        <td class="func">Funktion-Name</td>
                        <td class="func">Kurzbeschreibung</td>
                        <td class="func">Sichtbarkeit</td>
                    </xsl:otherwise>
                </xsl:choose>
            </tr>
            <tr>
                <!-- Erste Zeile für Rückgabetyp, Funktionsname und Zusammenfassung -->
                <td><xsl:value-of select="parameters/retval/@type"/></td>
                <td><xsl:value-of select="@name"/></td>
                <td>
                    <xsl:if test="devnotes/summary">
                        <xsl:value-of select="devnotes/summary"/>
                        <!-- Link zum Remarks-Anker -->
                        <a class="more-link" href="#remarks-{@name}">Mehr...</a>
                    </xsl:if>
                </td>
                <td style="text-align:right;">
                    <xsl:value-of select="@visibility"/>
                </td>
            </tr>
            <tr>
                <!-- Zweite Zeile für Parameter -->
                <td colspan="4"> <!-- colspan="3" sorgt dafür, dass die Parameter
                über die gesamte Breite der Tabelle angezeigt werden -->
                    <strong>
                        <xsl:choose>
                            <xsl:when test="$lang = 'en'">Parameters:</xsl:when>
                            <xsl:otherwise>Parameter:
                                <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                            </xsl:otherwise>
                        </xsl:choose>
                    </strong>
                    <xsl:for-each select="parameters/parameter">
                        <xsl:value-of select="@name"/>:
                            <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                        <xsl:value-of select="@type"/>
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
                <td colspan="4" class="bggray">
                    <xsl:choose>
                        <xsl:when test="$lang = 'en'"><strong>Remarks:</strong></xsl:when>
                        <xsl:when test="$lang = 'de'"><strong>Bemerkungen:</strong></xsl:when>
                        <xsl:otherwise>
                            <div class="addi_td">
                                <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                            </div>
                        </xsl:otherwise>
                    </xsl:choose>
                </td>
            </tr>
            <tr>
            <!-- Remarks unter der Funktion anzeigen -->
            <xsl:if test="not(devnotes/remarks)">
                <td colspan="3" class="bgblack">
                <xsl:choose>
                    <xsl:when test="$lang = 'en'"><span class="bgblack;">no remarks</span></xsl:when>
                    <xsl:when test="$lang = 'de'"><span class="bgblack;">keine Bemerkungen</span></xsl:when>
                    <xsl:otherwise></xsl:otherwise>
                </xsl:choose>
                </td>
            </xsl:if>
            <xsl:if test="devnotes/remarks">
                <td colspan="4">
                    <a name="remarks-{@name}"></a>
                    <div class="remarks">
                        <p><xsl:value-of select="devnotes/remarks"/></p>
                    </div>
                </td>
            </xsl:if>
            </tr>
            <tr>
                <td colspan="4" class="bgblack"></td>
            </tr>
            <tr>
                <td colspan="4" style="background-color:orange;"></td>
            </tr>
            </xsl:for-each>
        </table>
</body>
</html>
    <xsl:for-each select="//members/function">
    <xsl:variable name="filename1" select="'QCharClass_class_functions_'" />
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
                                <td class="func">Return Type</td>
                                <td class="func">Function Name</td>
                                <td class="func">Summary</td>
                                <td class="func">Visibility</td>
                            </xsl:when>
                            <xsl:otherwise>
                                <td class="func">Rückgabetyp</td>
                                <td class="func">Funktion-Name</td>
                                <td class="func">Kurzbeschreibung</td>
                                <td class="func">Sichtbarkeit</td>
                            </xsl:otherwise>
                        </xsl:choose>
                    </tr>
                    <tr>
                        <!-- Erste Zeile für Rückgabetyp, Funktionsname und Zusammenfassung -->
                        <td><xsl:value-of select="parameters/retval/@type"/></td>
                        <td><xsl:value-of select="@name"/></td>
                        <td>
                            <xsl:if test="devnotes/summary">
                                <xsl:value-of select="devnotes/summary"/>
                                <!-- Link zum Remarks-Anker -->
                                <a class="more-link" href="#remarks-{@name}">Mehr...</a>
                            </xsl:if>
                        </td>
                        <td style="text-align:right;">
                            <xsl:value-of select="@visibility"/>
                        </td>
                    </tr>
                    <tr>
                        <!-- Zweite Zeile für Parameter -->
                        <td colspan="4"> <!-- colspan="3" sorgt dafür, dass die Parameter
                        über die gesamte Breite der Tabelle angezeigt werden -->
                            <strong>
                                <xsl:choose>
                                    <xsl:when test="$lang = 'en'">Parameters:</xsl:when>
                                    <xsl:otherwise>Parameter:
                                        <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                                    </xsl:otherwise>
                                </xsl:choose>
                            </strong>
                            <xsl:for-each select="parameters/parameter">
                                <xsl:value-of select="@name"/>:
                                    <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                                <xsl:value-of select="@type"/>
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
                        <td colspan="4" class="bggray">
                            <xsl:choose>
                                <xsl:when test="$lang = 'en'"><strong>Remarks:</strong></xsl:when>
                                <xsl:when test="$lang = 'de'"><strong>Bemerkungen:</strong></xsl:when>
                                <xsl:otherwise>
                                    <div class="addi_td">
                                        <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                                    </div>
                                </xsl:otherwise>
                            </xsl:choose>
                        </td>
                    </tr>
                    <tr>
                    <!-- Remarks unter der Funktion anzeigen -->
                    <xsl:if test="not(devnotes/remarks)">
                        <td colspan="3" class="bgblack">
                        <xsl:choose>
                            <xsl:when test="$lang = 'en'"><span class="bgblack;">no remarks</span></xsl:when>
                            <xsl:when test="$lang = 'de'"><span class="bgblack;">keine Bemerkungen</span></xsl:when>
                            <xsl:otherwise></xsl:otherwise>
                        </xsl:choose>
                        </td>
                    </xsl:if>
                    <xsl:if test="devnotes/remarks">
                        <td colspan="4">
                            <a name="remarks-{@name}"></a>
                            <div class="remarks">
                                <p><xsl:value-of select="devnotes/remarks"/></p>
                            </div>
                        </td>
                    </xsl:if>
                    </tr>
                    <tr>
                        <td colspan="4" class="bgblack"></td>
                    </tr>
                    <tr>
                        <td colspan="4" style="background-color:orange;"></td>
                    </tr>
                </table>
            </div>
        </body>
        </html>
        </xsl:document>
    </xsl:for-each>
</xsl:template>

</xsl:stylesheet>
