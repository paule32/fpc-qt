# ---------------------------------------------------------------------------
# File:   build.py
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

import lxml.etree as ET
import os
import sys

xml_files = [
    [ 'QCharClass', 'QCharClass_all'             , 'all'              ],
    #
    [ 'QCharClass', 'QCharClass_class_enums'     , 'class_enums'      ],
    [ 'QCharClass', 'QCharClass_class_functions' , 'class_functions'  ],
    [ 'QCharClass', 'QCharClass_class_procedures', 'class_procedures' ],
    #
    [ 'QCharClass', 'QCharClass_ctors'           , 'ctors'            ],
    [ 'QCharClass', 'QCharClass_enums'           , 'enums'            ],
    [ 'QCharClass', 'QCharClass_functions'       , 'functions'        ],
    [ 'QCharClass', 'QCharClass_procedures'      , 'procedures'       ]
    #
]

for filelist in xml_files:
    try:
        xml_doc = filelist[0] + '.xml'
        xslt_main_doc  = ET.parse(filelist[1] + '.xslt')
        #
        transform_main = ET.XSLT(xslt_main_doc)
        #
    except ET.XMLSyntaxError as e:
        print(f"XSLT-Datei konnte nicht geladen werden: {e}")
        exit()
    
    try:
        xml_doc_transformer = ET.parse(xml_doc)
    except ET.XMLSyntaxError as e:
        print(f"XML-Datei '{xml_doc}' konnte nicht geladen werden: {e}")
        continue
    except FileNotFoundError:
        print(f"Die XML-Datei '{xml_doc}' wurde nicht gefunden.")
        continue
    
    try:
        result_main = transform_main(xml_doc_transformer)
        if result_main is None:
            raise ValueError(f"Die Transformation der Datei '{xml_doc}' hat kein Ergebnis zurückgegeben.")
    except (ET.XSLTApplyError, ValueError) as e:
        print(f"Fehler bei der Transformation der Datei '{xml_doc}': {e}")
        continue
    
    output_file_path_main = os.path.splitext(filelist[0])[0] + '_' + filelist[2] + '.html'
    with open(output_file_path_main, 'wb') as f:
        f.write(ET.tostring(result_main, pretty_print=True))
    
    print(f"Die Haupt-HTML-Datei wurde erfolgreich erstellt: {output_file_path_main}")
