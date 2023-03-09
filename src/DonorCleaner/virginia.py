def virginia(input_xml):

    import pandas as pd
    from lxml import etree, objectify

    metadata = input_xml
    parser = etree.XMLParser(remove_blank_text=True)
    tree = etree.parse(metadata, parser)
    root = tree.getroot()

    ####    
    for elem in root.getiterator():
        if not hasattr(elem.tag, 'find'): continue  # guard for Comment tags
        i = elem.tag.find('}')
        if i >= 0:
            elem.tag = elem.tag[i+1:]
    objectify.deannotate(root, cleanup_namespaces=True)
    ####

    tree.write(f'{metadata}_cleaned.xml',
            pretty_print=True, xml_declaration=True, encoding='UTF-8')
    
    cleaned = f'{metadata}_cleaned.xml'


    df = pd.read_xml(cleaned, xpath= "//LiA")
    df = df.drop('Contributor', axis = 1)
    df_2 = pd.read_xml(cleaned, xpath= "//Contributor")
    df_3 = pd.read_xml(cleaned, xpath= "//ScheduleA//LiA//Contributor//Address")
    final_df = pd.concat([df, df_2, df_3], axis="columns")

    return final_df
