# ---------------------------------------------------------------------------
# File:   build.py
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

import lxml.etree as ET
import os
import sys

# Liste der XML-Dateien, die verarbeitet werden sollen
xml_files = ['QCharClass.xml']  # Füge hier weitere Dateien hinzu

# Lade die Haupt-XSLT-Datei
try:
    xslt_main_doc  = ET.parse('qchar_enums.xslt')
    #
    transform_main = ET.XSLT(xslt_main_doc)
    #
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
    
    # Führe die Transformation für die Hauptdatei durch
    try:
        result_main = transform_main(xml_doc)
        if result_main is None:
            raise ValueError(f"Die Transformation der Datei '{xml_file}' hat kein Ergebnis zurückgegeben.")
    except (ET.XSLTApplyError, ValueError) as e:
        print(f"Fehler bei der Transformation der Datei '{xml_file}': {e}")
        continue
    
    # Speichere die Haupt-HTML-Datei
    output_file_path_main = os.path.splitext(xml_file)[0] + '_mem.html'
    with open(output_file_path_main, 'wb') as f:
        f.write(ET.tostring(result_main, pretty_print=True))
    
    print(f"Die Haupt-HTML-Datei wurde erfolgreich erstellt: {output_file_path_main}")
