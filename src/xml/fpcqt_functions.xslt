<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
<xsl:output method="html" encoding="UTF-8" />
 
<xsl:import href="fpcqt_config.xslt"/>
<xsl:import href="fpcqt_html_header.xslt"/>
<xsl:import href="fpcqt_func.xslt"/>

<!-- Template für Enum -->
<xsl:template match="/">
<html>
    <!-- Verwende die importierte Vorlage -->
    <xsl:call-template name="html-header"/>
    <body>
        <xsl:apply-templates select="functions" />
    </body>
</html>
</xsl:template>

</xsl:stylesheet>
