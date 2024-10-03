import lxml.etree as ET
import os
import sys

# Liste der XML-Dateien, die verarbeitet werden sollen
xml_files = ['QCharClass.xml']  # Füge hier weitere Dateien hinzu

# Lade die Haupt-XSLT-Datei
try:
    xslt_main_doc  = ET.parse('fpcqt.xslt')
    #
    transform_main = ET.XSLT(xslt_main_doc)
    transform_enum = ET.XSLT(xslt_main_doc)
    transform_func = ET.XSLT(xslt_main_doc)
    transform_proc = ET.XSLT(xslt_main_doc)
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
    output_file_path_main = os.path.splitext(xml_file)[0] + '.html'
    with open(output_file_path_main, 'wb') as f:
        f.write(ET.tostring(result_main, pretty_print=True))

    print(f"Die Haupt-HTML-Datei wurde erfolgreich erstellt: {output_file_path_main}")

    # Extrahiere und speichere die enum-Elemente in eine separate Datei
    enums = xml_doc.xpath('//enum')
    for enum in enums:
        enum_name = enum.get('name')
        output_file_path_enum = f"{enum_name}_enums.html"
        try:
            result_enum = transform_enum(enum)
            with open(output_file_path_enum, 'wb') as f:
                f.write(ET.tostring(result_enum, pretty_print=True))
            print(f"Die enum-HTML-Datei wurde erfolgreich erstellt: {output_file_path_enum}")
        except ET.XSLTApplyError as e:
            print(f"Fehler bei der Transformation der enum-Datei '{output_file_path_enum}': {e}")
    
    # Extrahiere und speichere die function-Elemente in eine separate Datei
    functions = xml_doc.xpath('//function')
    for function in functions:
        function_name = function.get('name')
        output_file_path_function = f"{function_name}_functions.html"
        try:
            result_function = transform_func(function)
            with open(output_file_path_function, 'wb') as f:
                f.write(ET.tostring(result_function, pretty_print=True))
            print(f"Die func-HTML-Datei wurde erfolgreich erstellt: {output_file_path_function}")
        except ET.XSLTApplyError as e:
            print(f"Fehler bei der Transformation der enum-Datei '{output_file_path_function}': {e}")

    # Extrahiere und speichere die procedure-Elemente in eine separate Datei
    procedures = xml_doc.xpath('//procedure')
    for procedure in procedures:
        procedure_name = procedure.get('name')
        output_file_path_procedure = f"{procedure_name}_procedure.html"
        try:
            result_procedure = transform_proc(procedure)
            with open(output_file_path_procedure, 'wb') as f:
                f.write(ET.tostring(result_procedure, pretty_print=True))
            print(f"Die func-HTML-Datei wurde erfolgreich erstellt: {output_file_path_procedure}")
        except ET.XSLTApplyError as e:
            print(f"Fehler bei der Transformation der enum-Datei '{output_file_path_procedure}': {e}")
            