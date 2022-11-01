
from qgis.core import QgsVectorFileWriter, QgsCoordinateTransformContext
import os.path



class VectorFileWriter:
    """ Writes vector layer to file """

    def __init__(self, output_directory):
        self.output_directory = output_directory
        self.options = QgsVectorFileWriter.SaveVectorOptions()
        self.output_file_path = None
        self.output_file_root = None
        self.output_extension = None

    def writeCSV(self, vector_layer, overwrite=True):
        self.output_extension = '.csv'
        self.options.driverName = "csv"
        self.options.layerOptions = ['GEOMETRY=AS_WKT']
        return self.write(vector_layer, overwrite)

    def writeGPKG(self, vector_layer, overwrite=True, name=None):
        self.output_extension = '.gpkg'
        self.options.driverName = "GPKG"
        return self.write(vector_layer, overwrite, name)

    def write(self, vector_layer, overwrite=True, name=None):
        """ Write vector file and return file path """

        if name == None:
            self.output_file_root = self.output_directory + "/" + vector_layer.name()
        else:
            self.output_file_root = self.output_directory + "/" + name
        self.output_file_path = self.output_file_root + self.output_extension

        appendix = 0
        while True and appendix < 9999:
            if os.path.exists(self.output_file_path) is False or overwrite is True:
                error = QgsVectorFileWriter.writeAsVectorFormatV2(vector_layer, self.output_file_path,
                                                                  QgsCoordinateTransformContext(), self.options)[0]
                if error == QgsVectorFileWriter.NoError:
                    return self.output_file_path
                else:
                    print(error)
                    return None
            else:
                appendix = appendix + 1
                self.output_file_path = self.output_file_root + '_' + str(appendix) + self.output_extension

        return None
