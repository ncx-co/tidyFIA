from tempfile import TemporaryDirectory
from fiadb import database, storage
from path import Path

import psycopg2
import pytest
import sqlite3

TEST_DATA_DIR = Path(__file__).parent.parent.parent.parent / "test_data/fiadb"
TEST_STATES = ["HI", "WY"]
TEST_CONFIG = {
    "host": "localhost",
    "port": "5432",
    "dbname": "testfiadb",
    "user": "postgres",
    "password": "password",
}

# expected number of tables added by PostGIS extension
POSTGIS_TABLES = 3


def check_tables_and_plots():
    with psycopg2.connect(dsn=database.format_connection_string(TEST_CONFIG)) as con:
        cur = con.cursor()
        cur.execute(
            """SELECT table_name FROM information_schema.tables WHERE table_schema = 'public'"""
        )
        tables = len(cur.fetchall())

        cur.execute("""SELECT count(*) FROM plot""")
        plots = cur.fetchone()[0]
        cur.close()

    return tables, plots


def check_duplicates(table):
    with psycopg2.connect(dsn=database.format_connection_string(TEST_CONFIG)) as con:
        cur = con.cursor()
        cur.execute(
            f"""SELECT value, COUNT(*) FROM {table} GROUP BY value HAVING COUNT(*) > 1"""
        )
        duplicates = cur.fetchall()
        assert len(duplicates) == 0


def test_database():
    with TemporaryDirectory() as tempdir:
        tempdir = Path(tempdir)
        db_files = storage.clone_fia(
            dest_dir=tempdir,
            states=TEST_STATES,
            url_fmt=f"file://{TEST_DATA_DIR}" "/SQLite_FIADB_{STATE}.zip",
            method="curl",
        )

        # download raw db file, check contents
        n_plots = 0
        tables = []
        for db_filename in db_files.values():
            with sqlite3.connect(db_filename) as con:
                cur = con.cursor()
                cur.execute("""SELECT name FROM sqlite_master  WHERE type='table';""")
                tables.extend(table[0] for table in cur.fetchall())
                cur.execute("""SELECT COUNT(*) FROM plot""")
                n_plots += cur.fetchone()[0]
                cur.close()
        n_tables = len(list(set(tables)))

        # build up the test database
        database.populate_db(
            db_files=db_files,
            config=TEST_CONFIG,
            seed_state=TEST_STATES[1],
        )
    assert check_tables_and_plots() == (n_tables + POSTGIS_TABLES, n_plots)

    # look for duplicates in ref_forest_type_group table
    check_duplicates(table="ref_forest_type_group")
