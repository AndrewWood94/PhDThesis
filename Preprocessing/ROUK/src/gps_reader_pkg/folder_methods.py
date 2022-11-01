import os


def create_folder(path):
    """Method to create path. Will also create parent folders if don't exist.

    Args:
        path (pathlib.Path): path to folder to create
    """
    try:
        path.mkdir(parents=True, exist_ok=False)
    except FileExistsError:
        print(f"{path} exists")
    else:
        print(f"{path} exists")


def cleanup(path):
    """
    Method to remove -shm and -wal files after reading gpkg layer
    :param path:
    :return:
    """
    try:
        for filename in os.listdir(path):
            if filename.endswith('.gpkg-shm') or filename.endswith('.gpkg-wal'):
                os.remove(path + '/' + filename)
    except:
        pass
