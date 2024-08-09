<?xml version="1.0" encoding="UTF-8"?>
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
    <xsl:output method="html" encoding="UTF-8" />
    <xsl:template match="/">
        <html>
            <head>
                <title>Delphi XML Documentation</title>
                <style>
                    body { font-family: Arial, sans-serif; margin: 20px; }
                    h1 { color: #2c3e50; }
                    h2 { color: #2980b9; }
                    .namespace { margin-bottom: 20px; }
                    .namespace h2 { margin-top: 0; }
                    .contains { margin-left: 20px; }
                </style>
            </head>
            <body>
                <h1>Delphi XML Documentation</h1>
                <xsl:apply-templates/>
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

    <xsl:template match="namespace">
        <div class="namespace">
            <h2>Namespace: <xsl:value-of select="@name"/></h2>
            <div class="platform">Platform: <xsl:value-of select="@platform"/></div>
            <xsl:apply-templates select="contains"/>
        </div>
    </xsl:template>

    <xsl:template match="contains">
        <div class="contains">Contains: <xsl:value-of select="@name"/></div>
    </xsl:template>
    
    <!-- Füge weitere Templates für andere XML-Elemente hinzu, die du umwandeln möchtest -->
</xsl:stylesheet>
