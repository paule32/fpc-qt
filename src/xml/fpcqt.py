import lxml.etree as ET

# Lade die XML-Dokumentation
xml_doc = ET.parse('fpcqt.xml')

# Lade die XSLT-Datei
xslt_doc = ET.parse('fpcqt.xslt')
transform = ET.XSLT(xslt_doc)

# Führe die Transformation durch
html_doc = transform(xml_doc)

# Speichere das Ergebnis als HTML
with open('output.html', 'wb') as output_file:
    output_file.write(ET.tostring(html_doc, pretty_print=True, method="html"))
