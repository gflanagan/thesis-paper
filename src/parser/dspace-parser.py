#!/usr/bin/env python

from sys import argv, exit

from elementtree import ElementTree
from elementtree.ElementTree import Element

# xsi is hardcoded for now. extract_namespace method
# is currenly broken
XSI = '{http://www.w3.org/2001/XMLSchema-instance}'

def main(argv):
    import_dspace(argv[1])

def create_filename(name):
    file_name = name.split('.')
    return file_name[0]+'.pl'
 
# parse the dspace schema 
def import_dspace(dspace_schema):
    tree = ElementTree.parse(dspace_schema)
    dsProductList = []
    #extract_namespace(tree)
    iterator = tree.findall('DsProduct')
    for element in iterator:
        dsProductList.append(parse_DsProduct(element))
    pl_output_file = create_filename(dspace_schema)
    generate_pl_file(dsProductList, pl_output_file)

def parse_DsProduct(element):
    global XSI
    dsProductDict = {}
    type = element.get(XSI+'type').replace('D','d')
    dsProductDict['type'] = type
    print dsProductDict['type']
    dsProductDict['globalId'] = 'a'+element.get('globalId')
    if type != 'dsBuilding' and type != 'dsBuildingStorey':
        dsProductDict['representation'] = get_representation(element)    
    return dsProductDict

def get_representation(element):
    representation = element.find('representation')
    item = representation.find('itemAbsolute')
    ifc_points = []
    for node in item.findall('point'):
	    ifc_points.append((node.get('x'), node.get('y')))
    return ifc_points


# generate the prolog code for dspace schema
def generate_pl_file(dsProductList, file_name):
    with open(file_name, 'w') as pl_file:
	#write_header(pl_file)
	write_type(pl_file, dsProductList)
     	write_ifc_geometry(pl_file, dsProductList)
    pl_file.close()
    with open('../dspace_include.pl', 'w') as include_file:
        write_include(include_file, file_name)
    include_file.close()

def write_include(include_file, pl_name):
    include_file.write(':-consult(\'./space/spatial_reasoner\').\n')
    include_file.write(':-consult(\'./design/architecture\').\n')
    include_file.write(':-consult(\'./design/design_repository\').\n')
    include_file.write(':-consult(\'./parser/'+pl_name.split('.')[0]+'\').\n')

def write_ifc_geometry(pl_file, dsProductList):
    for dsProduct in dsProductList:
        if dsProduct['type'] != 'dsBuilding' and dsProduct['type'] != 'dsBuildingStorey':
	        points = output_points(dsProduct['representation'])
	        pl_file.write(''.join(('ifc_geometry(', dsProduct['globalId'], ', G) :-\n')))
	        pl_file.write(''.join(('\tspatial_primitive(G, [', points, ']).\n\n')))

def output_points(points):
    output = ''
    for point in points:
	    output = output+'('+point[0]+','+point[1]+'),'
    return output[:-1]   # need to take off trailing ','

def write_type(pl_file, dsProductList):
    for ds in dsProductList:
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

