<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" encoding="UTF-8" />
    
    <!-- Definiere Sprachvariablen -->
    <xsl:variable name="lang" select="'de'" />
    <xsl:variable name="theme" select="'dark'" />
    
    <xsl:variable name="namespace_text_en">Namespace</xsl:variable>
    <xsl:variable name="namespace_text_de">Namensraum</xsl:variable>
    
    <xsl:variable name="contains_text_en">Contains</xsl:variable>
    <xsl:variable name="contains_text_de">Enthält</xsl:variable>
    
    <!-- Template für Hauptseite -->
    <xsl:template match="/">
        <xsl:apply-templates select="namespace" />
    </xsl:template>
    
<xsl:template match="namespace">
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
        <style>
        <xsl:choose>
        <xsl:when test="$theme = 'light'">
            body {
            font-family: Arial, sans-serif;
            margin: 10px;
            background-color: #FFFFFF;
            color: black;
            }
            h1 { color: #2c3e50; }
            h2 { color: #2980b9; }
            table {
                width: 40%;
                border-collapse: collapse;
                margin-bottom: 20px;
            }
            th, td {
                padding: 8px;
                text-align: left;
                border-bottom: 1px solid #ddd;
            }
            th {
                background-color: #f2f2f2;
                color: black;
                font-weight: bold;
            }
            td.value-cell {
                width: 20%;
                text-align: left;
            }
            td.name-cell {
                width: 20%;
                text-align: left;
            }
        </xsl:when>
        <xsl:when test="$theme = 'dark'">
            body {
            font-family: Arial, sans-serif;
            margin: 10px;
            background-color: #000000;
            color: white;
            }
            h1 { color: #2c3e50; }
            h2 { color: #2980b9; }
            table {
                width: 40%;
                border-collapse: collapse;
                margin-bottom: 20px;
            }
            th, td {
                padding: 8px;
                text-align: left;
                border-bottom: 1px solid #ddd;
            }
            th {
                background-color: gray;
                color: white;
                font-weight: bold;
            }
            td.value-cell {
                width: 20%;
                text-align: left;
            }
            td.name-cell {
                width: 20%;
                text-align: left;
            }
            .constructor_div {
                width: 50%;
                padding: 8px;
                background-color: #505050;
                color: white;
            }
        </xsl:when>
        </xsl:choose>
            .namespace { margin-bottom: 10px; }
            .namespace h2 { margin-top: 0; }
            .const { margin-left: 10px; }
        </style>
    </head>
    <body>
        <h1>
            <xsl:choose>
                <xsl:when test="$lang = 'en'">
                    Delphi Documentation for Project: <xsl:value-of select="@name"/>
                </xsl:when>
                <xsl:otherwise>
                    Delphi Dokumentation für Projekt: <xsl:value-of select="@name"/>
                </xsl:otherwise>
            </xsl:choose>
        </h1>
        
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
    </body>
</html>
</xsl:template>

    <xsl:template match="unit">
        <h2><xsl:value-of select="@name" /></h2>
        <xsl:apply-templates/>
    </xsl:template>

    <xsl:template match="description">
        <p><xsl:value-of select="."/></p>
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
                    <span style="color:white;font-wight:normal;">constructor</span>
                    <xsl:text>&#160;</xsl:text>
                    <span style="color:orange;font-weight:bold;">Create</span>
                </div>
                <xsl:choose>
                    <!-- Prüfe, ob Parameter vorhanden sind -->
                    <xsl:when test="parameters/parameter">
                        <!-- Falls Parameter vorhanden sind, liste sie auf -->
                        <xsl:for-each select="parameters/parameter">
                            <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
                            <xsl:if test="position() != last()">, </xsl:if>
                        </xsl:for-each>
                    </xsl:when>
                    <!-- Falls keine Parameter vorhanden sind, zeige "no param" -->
                    <xsl:otherwise>
                        no param
                        <div style="background-color: black;">ss</div>
                    </xsl:otherwise>
                </xsl:choose>
            </div>
            
            <xsl:if test="devnotes/remarks">
                <a name="remarks-{@name}"></a>
                <p class="remarks">
                    <strong>Remarks:</strong>
                    <xsl:value-of select="devnotes/remarks"/>
                </p>
            </xsl:if>
        </xsl:for-each>
                
        <!-- Tabelle für Funktionen -->
        <xsl:if test="members/function">
            <h3>Functions</h3>
            <table>
                <tr>
                <xsl:choose>
                    <xsl:when test="$lang = 'en'">
                        <th>Return Type</th>
                        <th>Function Name</th>
                        <th>Summary</th>
                        <th>Parameters</th>
                    </xsl:when>
                    <xsl:otherwise>
                        <th>Rückgabetyp</th>
                        <th>Funktion-Name</th>
                        <th>Kurzbeschreibung</th>
                        <th>Parameter</th>
                    </xsl:otherwise>
                </xsl:choose>
                </tr>
                <xsl:for-each select="members/function">
                    <tr>
                        <td><xsl:value-of select="parameters/retval/@type"/></td>
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
