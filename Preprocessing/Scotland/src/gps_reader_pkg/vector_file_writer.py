"""
/***************************************************************************
 This file was taken and adapted from:

 GpxSegmentImporter
                                 A QGIS plugin
 This plugin imports an GPX file and creates short line segments between track points
                              -------------------
        begin                : 2017-12-01
        git sha              : $Format:%H$
        copyright            : (C) 2018 by Simon Gröchenig @ Salzburg Research
        email                : simon.groechenig@salzburgresearch.at


        MODIFIED:
        begin                : 2019-11-04
        email                : andrew.wood@ed.ac.uk
 ***************************************************************************/
/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
"""
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
