import lxml.etree as ET
import os

# Liste der XML-Dateien, die verarbeitet werden sollen
xml_files = ['QCharClass.xml']  # Füge hier weitere Dateien hinzu

# Lade die XSLT-Datei
try:
    xslt_doc = ET.parse('fpcqt.xslt')
    transform = ET.XSLT(xslt_doc)
except ET.XMLSyntaxError as e:
    print(f"XSLT-Datei konnte nicht geladen werden: {e}")
    exit()

# Verarbeite jede XML-Datei in der Liste
for xml_file in xml_files:
    try:
        # Lade die XML-Datei
        xml_doc = ET.parse(xml_file)
    except ET.XMLSyntaxError as e:
        print(f"XML-Datei '{xml_file}' konnte nicht geladen werden: {e}")
        continue
    except FileNotFoundError:
        print(f"Die XML-Datei '{xml_file}' wurde nicht gefunden.")
        continue

    # Führe die Transformation durch
    try:
        result = transform(xml_doc)
        if result is None:
            raise ValueError(f"Die Transformation der Datei '{xml_file}' hat kein Ergebnis zurückgegeben.")
    except (ET.XSLTApplyError, ValueError) as e:
        print(f"Fehler bei der Transformation der Datei '{xml_file}': {e}")
        continue

    # Speichere das Ergebnis als HTML
    output_file_path = os.path.splitext(xml_file)[0] + '.html'
    with open(output_file_path, 'wb') as f:
        f.write(ET.tostring(result, pretty_print=True))

    print(f"Die HTML-Datei wurde erfolgreich erstellt: {output_file_path}")
