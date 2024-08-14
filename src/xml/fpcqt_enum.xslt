<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8" />
 
<xsl:import href="fpcqt_config.xslt" />
<!--xsl:import href="fpcqt_html_header.xslt"/-->

<!-- Template für Enum -->
<xsl:template match="/">
<html>
    <!-- Verwende die importierte Vorlage -->
    <!--xsl:call-template name="html-header"/-->
    <body>
        <xsl:apply-templates select="enum" />
    </body>
</html>
</xsl:template>

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

</xsl:stylesheet>
