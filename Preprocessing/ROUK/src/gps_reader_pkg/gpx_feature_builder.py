from PyQt5.QtCore import QVariant
# Initialize Qt resources from file resources.py
from qgis.core import QgsVectorLayer, QgsField, QgsFeature, QgsGeometry
from gps_reader_pkg.datatype_definition import DataTypes
from gps_reader_pkg.vector_file_writer import VectorFileWriter
import os


class GpxFeatureBuilder:
    """ Builds gpx layers and features """

    def __init__(self, layer_name, attribute_definitions, crs=None):
        self.error_message = ''
        layer_definition: str = 'LineString'
        if crs is not None:
            layer_definition = layer_definition + "?crs=epsg:" + str(crs.postgisSrid())
        self.vector_layer = QgsVectorLayer(layer_definition, layer_name, "memory")
        self.data_provider = self.vector_layer.dataProvider()
        # Enter editing mode
        self.vector_layer.startEditing()
        self.add_new_attributes(attribute_definitions)

    def add_new_attributes(self, new_attributes):
        attributes = list()
        for attribute in new_attributes:
            if attribute.selected:  # select attribute [boolean]
                key = str(attribute.attribute_key)
                if attribute.datatype == DataTypes.Integer:  # data type [Integer|Double|String]
                    attributes.append(QgsField(key, QVariant.Int, 'Integer'))
                elif attribute.datatype == DataTypes.Double:
                    attributes.append(QgsField(key, QVariant.Double, 'Real'))
                elif attribute.datatype == DataTypes.Boolean:
                    attributes.append(QgsField(key, QVariant.Int, 'Integer'))
                elif attribute.datatype == DataTypes.String:
                    attributes.append(QgsField(key, QVariant.String, 'String'))

        self.data_provider.addAttributes(attributes)
        self.vector_layer.updateFields()

    def add_feature(self, line_coordinates, attributes):
        feature = QgsFeature()
        feature.setGeometry(QgsGeometry.fromPolyline(line_coordinates))
        feature.setFields(self.vector_layer.fields(), True)
        for attribute_key in list(attributes.keys()):
            try:
                feature.setAttribute(attribute_key, attributes[attribute_key])
            except KeyError:
                pass
        self.data_provider.addFeatures([feature])

    def save_layer(self, output_directory, overwrite, write_file):
        self.vector_layer.commitChanges()

        self.error_message = ''

        if self.vector_layer.featureCount() > 0:

            self.vector_layer.updateExtents()
            #Write vector layer to file
            if output_directory is not None and write_file:
                if os.path.isdir(output_directory):
                    vector_layer_writer = VectorFileWriter(output_directory)
                    output_file_path = vector_layer_writer.writeGPKG(self.vector_layer, overwrite)

                    if output_file_path is not None:
                        return QgsVectorLayer(output_file_path, os.path.basename(output_file_path), 'ogr')
                    else:
                        self.error_message = 'Writing vector layer failed...'
                        return None
                else:
                    self.error_message = 'Cannot find output directory'
                    return None

            return self.vector_layer

        else:
            return False
