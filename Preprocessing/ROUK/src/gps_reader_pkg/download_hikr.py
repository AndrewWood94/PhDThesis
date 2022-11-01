import json
import requests
from pathlib import Path

def getHikr(destination_folder, json_file_path):

    with open(json_file_path) as json_file:
        data = json.load(json_file)
        for path in data:
            file_path = path['filepath']
            file = requests.get(file_path)
            name = Path(file_path).name
            with open(destination_folder + '/' + name, 'wb') as f:
                f.write(file.content)
