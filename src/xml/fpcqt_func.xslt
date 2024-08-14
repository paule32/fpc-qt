<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8" />

<xsl:import href="fpcqt_config.xslt" />

<!-- Tabelle für Funktionen -->
<xsl:template match="functions">
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
</xsl:template>
</xsl:stylesheet>


