from qgis.core import (QgsApplication)


class QgisSetup:

    def __init__(self):
        self.qgs = QgsApplication([], False)
        self.qgs.initQgis()

    def exit(self):
        self.qgs.exit()

