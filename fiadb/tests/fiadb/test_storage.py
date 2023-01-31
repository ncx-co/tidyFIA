from fiadb import storage
from path import Path
from tempfile import TemporaryDirectory

import os


TEST_DATA_DIR = Path(__file__).parent.parent.parent.parent / "test_data/fiadb"
TEST_STATES = ["HI", "WY"]
TEST_DATE = "test"
LOCAL_URL_PREFIX = f"file://{TEST_DATA_DIR}"
TEST_URL_FMT = LOCAL_URL_PREFIX + "/SQLite_FIADB_{STATE}.zip"


def test_clone_fia():
    with TemporaryDirectory() as tempdir:
        tempdir = Path(tempdir)
        db_files = storage.clone_fia(
            dest_dir=tempdir,
            states=TEST_STATES,
            url_fmt=TEST_URL_FMT,
            method="curl",
        )

        assert all(os.path.exists(db_files[state]) for state in TEST_STATES)


def test_download():
    with TemporaryDirectory() as tempdir:
        db_filename = storage.download(
            url=TEST_URL_FMT.format(STATE=TEST_STATES[0]),
            dest_dir=tempdir,
            method="curl",
        )
        assert os.path.exists(db_filename)
