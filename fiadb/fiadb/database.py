import logging
import os
import sqlite3

import psycopg2

# path to pgloader built from source
PGLOADER = "pgloader"

# example database connection info for a local clone
LOCAL_CONFIG = {
    "host": "localhost",
    "port": "5432",
    "dbname": "fiadb",
    "user": "postgres",
    "password": "password",
}

POSTGRES_CONN_STRING = "postgresql://{USER}:{PASSWORD}@{HOST}/{DATABASE_NAME}"


def format_connection_string(config):
    """Format PostgreSQL connection string from config dictionary"""
    return POSTGRES_CONN_STRING.format(
        DATABASE_NAME=config["dbname"],
        USER=config["user"],
        PASSWORD=config["password"],
        HOST=config["host"],
        PORT=config["port"],
    )


def initialize_db(
    connection_string,
    db_filename,
):
    """Initialize database with SQLite file"""
    logging.info(f"initiating the fiadb database with {db_filename}")
    cmd = (
        f'{PGLOADER} --with "prefetch rows = 10000" '
        '--with "on error stop" '
        f"{db_filename} {connection_string}"
    )

    os.system(cmd)


def add_state_to_db(connection_string, db_filename):
    """Load contents of sqlite database into existing database"""
    copy_cmd = (
        f'{PGLOADER} --client-min-messages --with "prefetch rows = 10000" '
        '--with "include no drop" --with "create no tables" '
        f"{db_filename} {connection_string}"
    )
    logging.info(f"running:\n{copy_cmd}")

    os.system(copy_cmd)


def update_db(
    connection_string: str,
    db_files: dict,
):
    """Update database with contents of many files

    Args:
        connection_string: PostgreSQL connection string
        db_files: dictionary with {state: /path/to/file.db}
    """
    for state, db_filename in db_files.items():
        logging.info(f"processing {state}")
        drop_ref_tables(db_filename)
        add_state_to_db(connection_string=connection_string, db_filename=db_filename)


def populate_db(
    config,
    db_files,
    seed_state="WY",
):
    """Populate the FIA database from scratch

    Starting with a single "seed" file to define the schema for the database,
    load all of the .db files in the `db_files` dictionary using pgloader.

    Args:
        config: dictionary with database connection parameters to be passed to
            `format_connection_string`
        db_files: dictionary with {state: /path/to/file.db}, output of
            `storage.clone_fia`
        seed_state: state to use for defining the schema of the new database,
            useful if one of the states has a new schema and the rest don't!
    """
    connection_string = format_connection_string(config=config)

    # initiate db with one state's worth of data
    logging.info(f"initiating database with data from {seed_state}")
    seed_db_filename = db_files.pop(seed_state)
    initialize_db(
        connection_string=connection_string,
        db_filename=seed_db_filename,
    )

    # load rest of states into the database
    update_db(
        connection_string=connection_string,
        db_files=db_files,
    )

    # update the geom column
    update_db_geom(connection_string=connection_string)


def update_db_geom(connection_string):
    """Update database geometry for PostGIS queries"""
    # add geom column, lat long coordinates
    with psycopg2.connect(dsn=connection_string) as conn:
        cur = conn.cursor()
        cur.execute(
            "ALTER TABLE public.plot ADD COLUMN IF NOT EXISTS geom geometry(Point, 4326);"
        )
        cur.execute(
            "CREATE INDEX IF NOT EXISTS idx_plot_geom ON public.plot USING GIST(geom);"
        )
        cur.execute(
            "UPDATE public.plot SET geom=st_SetSrid(st_MakePoint(lon, lat), 4326);"
        )
        cur.close()


def drop_ref_tables(db_filename):
    """Drop all REF tables from a sqlite FIADB file"""
    with sqlite3.connect(db_filename) as conn:
        cursor = conn.cursor()
        cursor.execute("SELECT name FROM sqlite_master WHERE type='table';")
        ref_tables = [
            table[0] for table in cursor.fetchall() if "ref" in table[0].lower()
        ]
        for table in ref_tables:
            cursor.execute(f"DROP TABLE {table}")
            conn.commit()
