import multiprocessing as mp
import os
import subprocess
import zipfile

import tqdm
from path import Path

from fiadb import STATES

SQLITE_URL_FMT = (
    "https://apps.fs.usda.gov/fia/datamart/Databases/SQLite_FIADB_{STATE}.zip"
)

# database basename within zip file
DB_BASENAME = "SQLite_FIADB_{STATE}.db"


def download(url, dest_dir: str, method="wget"):
    """Download zipped SQLite file for a state from FIA to local file"""
    dest_dir = Path(dest_dir)
    dest_zip = dest_dir / Path(url).basename()
    if method == "wget":
        subprocess.run(["wget", "-O", dest_zip, url])
    elif method == "curl":
        subprocess.run(["curl", url, "-o", dest_zip])

    with zipfile.ZipFile(dest_zip, "r") as zip_ref:
        files = zip_ref.namelist()
        db_file = [f for f in files if f.endswith(".db")][0]
        zip_ref.extractall(dest_dir)

    # delete the downloaded zip
    os.remove(dest_zip)

    return dest_dir / db_file


def clone_fia(dest_dir, states=STATES, url_fmt=SQLITE_URL_FMT, method="wget"):
    """Download zipped SQLite files from FIA into local directory"""
    dest_dir = Path(dest_dir).mkdir_p()
    with mp.Pool(processes=mp.cpu_count()) as pool:
        jobs = [
            pool.starmap_async(
                download,
                [(url_fmt.format(STATE=state), dest_dir, method)],
            )
            for state in states
        ]
        db_files = []
        for job in tqdm.tqdm(jobs):
            db_files.append(job.get()[0])

    return {state: db_filename for state, db_filename in zip(states, db_files)}
