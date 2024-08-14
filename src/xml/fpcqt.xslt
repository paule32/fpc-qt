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
    <xsl:apply-templates select="namespace" />
</body>
</html>
</xsl:template>

<!-- Template für Funktionen1 -->
<xsl:template match="mups/functions">
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
    xxxx
    <xsl:apply-templates select="members/function" />
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
    
    <xsl:apply-templates select="enum" />
    <xsl:apply-templates select="class" />
</div>
</xsl:template>

<!-- Template für Contains -->
<xsl:template match="contains">
<div class="contains">
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <xsl:value-of select="$contains_text_en"/>: <xsl:value-of select="@name"/>.pas
        </xsl:when>
        <xsl:otherwise>
            <xsl:value-of select="$contains_text_de"/>: <xsl:value-of select="@name"/>.pas
        </xsl:otherwise>
    </xsl:choose>
</div>
</xsl:template>
    
<!-- Template für Enum -->
<xsl:template match="enum">
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
    <xsl:for-each select="element">
    <tr>
        <td class="name-cell"><xsl:value-of select="@name"/></td>
        <td class="value-cell"><xsl:value-of select="@value"/></td>
    </tr>
    </xsl:for-each>
</table>
</xsl:template>

<!-- Template für Class -->
<xsl:template match="class">
<xsl:choose>
    <xsl:when test="$lang = 'en'">
        <h2>Class: <xsl:value-of select="@name"/></h2>
    </xsl:when>
    <xsl:otherwise>
        <h2>Klasse: <xsl:value-of select="@name"/></h2>
    </xsl:otherwise>
</xsl:choose>

<!-- Klasse Zusammenfassung -->
<xsl:if test="summary">
    <p class="summary"><xsl:value-of select="summary"/></p>
</xsl:if>

<!-- Tabelle für vererbte Klasse -->
<xsl:if test="ancestor">
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <h3>Inherited from</h3>
        </xsl:when>
        <xsl:otherwise>
            <h3>Erbt von</h3>
        </xsl:otherwise>
    </xsl:choose>
    <table>
        <tr>
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <th>Ancestor Class</th>
                    <th>Namespace</th>
                </xsl:when>
                <xsl:otherwise>
                    <th>Vererbter-Klassen-Name</th>
                    <th>Namensraum</th>
                </xsl:otherwise>
            </xsl:choose>
        </tr>
        <tr>
            <td>
                <xsl:value-of select="ancestor/@name"/>
            </td>
            <td>
                <xsl:value-of select="ancestor/@namespace"/>
            </td>
        </tr>
    </table>
</xsl:if>

<!-- Tabelle für Konstruktoren 'Create' -->
<xsl:if test="members/constructor">
    <xsl:choose>
        <xsl:when test="$lang = 'en'">
            <h3>Constructors</h3>
        </xsl:when>
        <xsl:otherwise>
            <h3>Konstruktoren</h3>
        </xsl:otherwise>
    </xsl:choose>
    <table>
        <tr>
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <th>Constructor</th>
                    <th>Parameters</th>
                    <th>Summary</th>
                </xsl:when>
                <xsl:otherwise>
                    <th>Konstruktor</th>
                    <th>Parameter</th>
                    <th>Kurzbeschreibung</th>
                </xsl:otherwise>
            </xsl:choose>
        </tr>
        <xsl:for-each select="members/constructor">
            <tr>
                <td>Create</td>
                <td>
                    <xsl:for-each select="parameters/parameter">
                        <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
                        <xsl:if test="position() != last()">, </xsl:if>
                    </xsl:for-each>
                </td>
                <td>
                    <xsl:if test="devnotes/summary">
                        <xsl:value-of select="devnotes/summary"/>
                        <!-- Link zum Remarks-Anker -->
                        <xsl:if test="devnotes/remarks">
                            <a class="more-link" href="#remarks-{@name}">Mehr...</a>
                        </xsl:if>
                    </xsl:if>
                </td>
            </tr>
        </xsl:for-each>
    </table>
</xsl:if>

<!-- Ausführliche Beschreibung für Konstruktoren unter der Tabelle anzeigen -->
<xsl:for-each select="members/constructor">
    <div>
        <div class="constructor_div">
            <div class="ctor_left">
                <span style="color:white;font-wight:normal;">constructor</span>
                <xsl:text>&#160;</xsl:text>
                <span style="color:orange;font-weight:bold;">Create</span>
            </div>
            <div class="ctor_right">
                Pascal
            </div>
        </div>
        <xsl:choose>
            <!-- Prüfe, ob Parameter vorhanden sind -->
            <xsl:when test="parameters/parameter">
                <!-- Falls Parameter vorhanden sind, liste sie auf -->
                <div class="ctor_container">
                <xsl:for-each select="parameters/parameter">
                    <div class="param_div">
                        <li>
                            <!-- Zeige die aktuelle Position an -->
                            <xsl:text>Parameter </xsl:text><xsl:value-of select="position()"/>
                            <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                            
                            <!-- Zeige den Namen und Typ des Parameters an -->
                            <span class="parameter"><xsl:value-of select="@name"/></span>
                            <xsl:text>&#160;&#160;&#160;&#160;</xsl:text>
                            <span class="parameter_name"><xsl:value-of select="@type"/></span>
                            
                            <xsl:if test="position() != last()">, <br/></xsl:if>
                        </li>
                    </div>
                </xsl:for-each>
                </div>
            </xsl:when>
            <!-- Falls keine Parameter vorhanden sind, zeige "no param" -->
            <xsl:otherwise>
                <div class="ctor_container">
                    <div class="param_div">no param</div>
                </div>
                <p> </p>
            </xsl:otherwise>
        </xsl:choose>
    </div>
    
    <xsl:if test="devnotes/remarks">
        <a name="remarks-{@name}"></a>
        <p class="remarks">
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    <strong>Remarks:</strong>
                </xsl:when>
                <xsl:otherwise>
                    <strong>Bemerkungen:</strong>
                </xsl:otherwise>
            </xsl:choose>
            <br/>
            <xsl:value-of select="devnotes/remarks"/>
        </p>
    </xsl:if>
</xsl:for-each>
        
<!-- Tabelle für Funktionen -->
<xsl:if test="members/function">
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
        <!-- Durchlaufe jede Funktion -->
        <xsl:for-each select="members/function">
            <tr>
                <!-- Erste Zeile für Rückgabetyp, Funktionsname und Zusammenfassung -->
                <td><xsl:value-of select="parameters/retval/@type"/></td>
                <td><xsl:value-of select="@name"/></td>
                <td>
                    <xsl:if test="devnotes/summary">
                        <xsl:value-of select="devnotes/summary"/>
                    </xsl:if>
                </td>
            </tr>
            <tr>
                <!-- Zweite Zeile für Parameter -->
                <td colspan="3"> <!-- colspan="3" sorgt dafür, dass die Parameter
                über die gesamte Breite der Tabelle angezeigt werden -->
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
        </xsl:for-each>
    </table>
</xsl:if>

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
