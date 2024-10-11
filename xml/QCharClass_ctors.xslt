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
        <xsl:for-each select="//members/constructor">
            <xsl:variable name="filename1" select="'QCharClass_ctors_'" />
            <xsl:variable name="filename2" select="concat($filename1, @name, '_')" />
            <xsl:variable name="filename3" select="concat($filename2, position(), '.html')" />
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
                    <div id="container">
                        <h1>
                            <xsl:choose>
                                <xsl:when test="$lang = 'en'">
                                    <h3>Constructors</h3>
                                </xsl:when>
                                <xsl:otherwise>
                                    <h3>Konstruktoren</h3>
                                </xsl:otherwise>
                            </xsl:choose>
                        </h1>
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
                                    <td style="width:170px;">
                                        <xsl:for-each select="parameters/parameter">
                                        <xsl:value-of select="@name"/>: <xsl:value-of select="@type"/>
                                        <xsl:if test="array/element">Array of
                                        <span style="color:orange;">
                                            <xsl:value-of select="array/element/@type"/>
                                        </span>
                                        </xsl:if>
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
                    
                        <!-- Ausführliche Beschreibung für Konstruktoren unter der Tabelle anzeigen -->
                        <div>
                            <div class="constructor_div">
                                <div class="ctor_left">
                                    <span class="ctor_l">constructor</span>
                                    <xsl:text>&#160;&#160;</xsl:text>
                                    <span class="ctor_r">Create</span>
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
                                            <!-- Zeige die aktuelle Position an -->
                                            <xsl:value-of select="position()"/>.<xsl:text>&#160;&#160;Parameter:</xsl:text>
                                            <!-- Zeige den Namen und Typ des Parameters an -->
                                            <div class="para">
                                                <div style="width:100px;">
                                                    Name:<br/>
                                                    Typ:
                                                </div>
                                                <div>
                                                    <span class="parameter">
                                                        <xsl:value-of select="@name"/><br/>
                                                        <xsl:value-of select="@type"/>
                                                    </span>
                                                    
                                                    <!-- wenn ARRAY OF ... -->
                                                    <xsl:if test="array/element">Array of
                                                    <span style="color:orange;">
                                                        <xsl:value-of select="array/element/@type"/>
                                                    </span>
                                                    </xsl:if>
                                                </div>
                                            </div>
                                        </div>
                                    <xsl:if test="position() != last()">, <br/></xsl:if>
                                    </xsl:for-each>
                                    </div>
                                </xsl:when>
                                <!-- Falls keine Parameter vorhanden sind, zeige "no param" -->
                                <xsl:otherwise>
                                    <div class="ctor_container">
                                        <div class="param_div">keine Parameter</div>
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
                                <div style="width:40vw;">
                                    <xsl:value-of select="devnotes/remarks"/>
                                </div>
                            </p>
                        </xsl:if>
                    </div>
                </body>
                </html>
            </xsl:document>
        </xsl:for-each>
    </div>
</body>
</html>
</xsl:template>

</xsl:stylesheet>
