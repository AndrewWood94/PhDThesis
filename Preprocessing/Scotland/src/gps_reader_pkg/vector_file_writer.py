
from qgis.core import QgsVectorFileWriter, QgsCoordinateTransformContext
import os.path



class VectorFileWriter:
    """ Writes vector layer to file """

    def __init__(self, output_directory):
        self.output_directory = output_directory

    def write(self, vector_layer, overwrite):
        """ Write vector file and return file path """

        output_file_path = self.output_directory + "/" + vector_layer.name() + '.gpkg'
        options = QgsVectorFileWriter.SaveVectorOptions()
        options.driverName = "GPKG"
        appendix = 0
        while True and appendix < 9999:
            if os.path.exists(output_file_path) is False or overwrite is True:
                error = QgsVectorFileWriter.writeAsVectorFormatV2(vector_layer, output_file_path,
                                                                  QgsCoordinateTransformContext(), options)[0]
                if error == QgsVectorFileWriter.NoError:
                    return output_file_path
                else:
                    print(error)
                    return None
            else:
                appendix = appendix + 1
                output_file_path = self.output_directory + "/" + vector_layer.name() + '_' + str(appendix) + '.gpkg'

        return None
