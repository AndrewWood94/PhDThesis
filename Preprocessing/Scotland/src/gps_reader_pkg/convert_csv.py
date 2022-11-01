"""
Save a QGIS vector layer in csv format
"""

from qgis.core import (QgsVectorFileWriter, QgsCoordinateTransformContext)


def Convert(vector_layer, output_file_path):
    """
    Method to save a QGIS vector layer in csv format

    :param vector_layer: Layer to be converted
    :param output_file_path: Filepath to save converted csv file
    :return: csv file saved to output_file_path
    """
    options = QgsVectorFileWriter.SaveVectorOptions()
    options.driverName = "csv"
    options.layerOptions=['GEOMETRY=AS_WKT']

    error = QgsVectorFileWriter.writeAsVectorFormatV2(vector_layer, output_file_path,
                                                      QgsCoordinateTransformContext(), options)[0]
    if error == QgsVectorFileWriter.NoError:
        return (output_file_path)
    else:
        print(error)
        return False
