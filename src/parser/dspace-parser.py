#!/usr/bin/env python

from sys import argv, exit
from elementtree import ElementTree
from elementtree.ElementTree import Element


# xsi is hardcoded for now. extract_namespace method is currenly broken
XSI = '{http://www.w3.org/2001/XMLSchema-instance}'


def main(argv):
    import_dspace_schema(argv[1])

def create_filename(name):
    file_name = name.split('.')
    return file_name[0]+'.pl'
 
# parse the dspace schema 
def import_dspace_schema(dspace_schema):
    tree = ElementTree.parse(dspace_schema)
    ds_product_list = []
    relationship_list = []
    # method is currently broken
    #  extract_namespace(tree)
    ds_product_iterator = tree.findall('DsProduct')
    relationship_iterator = tree.findall('DsRelationship')
    for element in ds_product_iterator:
        ds_product_list.append(parse_dsproduct(element))
    for element in relationship_iterator:
        relationship_list.append(parse_relationship(element)) 
    pl_file_name = create_filename(dspace_schema)
    generate_pl_file(pl_file_name, ds_product_list, relationship_list)

def parse_relationship(element):
    list = []
    relating_structure_id = ''
    global XSI
    rel_type = element.get(XSI+'type')
    if rel_type == 'DsRelContainedInSpatialStructure':
        temp = element.find('relatingStructureGUID')
        text = temp.text
        relating_structure_id = clean_id(text) 
        list = containment(element) 
    return (relating_structure_id, list)

def containment(element):
    iterator = element.findall('relatedElementGUID')
    list = []
    for e in iterator:
        list.append(clean_id(e.text))
    return list

def parse_dsproduct(element):
    global XSI
    ds_product_dict = {}
    type = extract_dstype(element)
    ds_product_dict['type'] = type
    ds_product_dict['globalId'] = clean_id(element.get('globalId'))
    if type != 'dsBuilding' and type != 'dsBuildingStorey':
        ds_product_dict['representation'] = get_representation(element)    
    return ds_product_dict

def extract_dstype(element):
    type = element.get(XSI+'type')
    first_char = type[0]
    if first_char == 'D':
        type = type.replace('D','d',1)
    if not type.startswith('ds'):
        type = 'ds'+type
    return type

def clean_id(id):
    # extracts $ characters and appends a leading 'a' because Prolog does not allow
    # $ chars in atoms and atoms must start with lower case letter
    new_id = id.replace('$', '')
    new_id = 'a'+new_id
    return new_id

def get_representation(element):
    representation = element.find('representation')
    item = representation.find('itemAbsolute')
    ifc_points = []
    for node in item.findall('point'):
        x = node.get('x')
        y = node.get('y')
        if 'e-' in x:
            x = '0'
        if 'e-' in y:
            y = '0'
        ifc_points.append((x, y))
    return ifc_points


# generate the prolog code for dspace schema
def generate_pl_file(file, ds_product_list, rel_list):
    with open(file, 'w') as pl_file:
	#write_header(pl_file)
	write_type(pl_file, ds_product_list)
        write_ifc_geometry(pl_file, ds_product_list)
    with open('../dspace_include.pl', 'w') as include_file:
        write_include(include_file, file)
    include_file.close()
    pl_file.close()

def write_relationships(pl_file, rel_list, ds_product_list):
    for element in rel_list:
        relating_structure_id = element[0]
        relating_elements = element[1]
        type = get_dstype(relating_structure_id, ds_product_list) 
        if type == 'dsSpace':
            write_room_containment(pl_file, relating_structure_id, relating_elements)
        elif type == 'dsWallStandardCase':
            write_wall_containment(pl_file, relating_structure_id, relating_elements)

def write_room_containment(pl_file, relating_structure_id, relating_elements):
    for element in relating_elements:
        pl_file.write(''.join(('room_containment(', element, ', ', relating_structure_id,').\n')))

def get_dstype(id, list):
    type = ''
    for product in list:
        curr_id = product['globalId']
        if curr_id == id:
            type = product['type']    
    return type

def write_include(include_file, pl_name):
    include_file.write(':-consult(\'./space/spatial_reasoner\').\n')
    include_file.write(':-consult(\'./design/architecture\').\n')
    include_file.write(':-consult(\'./design/design_repository\').\n')
    include_file.write(':-consult(\'./parser/'+pl_name.split('.')[0]+'\').\n')

def write_ifc_geometry(pl_file, ds_product_list):
    for dsProduct in ds_product_list:
        if dsProduct['type'] != 'dsBuilding' and dsProduct['type'] != 'dsBuildingStorey':
	        points = output_points(dsProduct['representation'])
	        pl_file.write(''.join(('ifc_geometry(', dsProduct['globalId'], ', G) :-\n')))
	        pl_file.write(''.join(('\tspatial_primitive(G, [', points, ']).\n\n')))

def output_points(points):
    output = ''
    for point in points:
	    output = output+'('+point[0]+','+point[1]+'),'
    return output[:-1]   # need to take off trailing ','

def write_type(pl_file, ds_product_list):
    for ds in ds_product_list:
	    pl_file.write(''.join(('arch_entity(', ds['globalId'], ', ',ds['type'],').\n'))) 

def write_header(pl_file):
    pl_file.write(':-consult(design).\n')
    pl_file.write(':-consult(solver).\n')
    pl_file.write(':-consult(orientationr).\n\n')


# warning this is a hack to get the namespace that
# carl so desperately needed in this well formed xml schema.
# so blame carl when it does not work!!!
def extract_namespace(tree):
    global XSI 
    XSI =  tree.getroot().attrib.keys()[0].split('}')[0]+'}'


if __name__ == "__main__":
    if len(argv) < 1 :
	print 'use ....'
	exit(1)
    main(argv)

