# `fiadb`

`fiadb` is a python package for building a PostgreSQL/PostGIS clone of the
public Forest Inventory and Analysis (FIA) database.
It pulls state-level SQLite files from
[FIA's Datamart](https://apps.fs.usda.gov/fia/datamart/datamart.html) and uses
[`pgloader`](https://pgloader.readthedocs.io/en/latest/) to dump the data into
a PostgreSQL database.
**It has only been tested on Ubuntu 20.04! It will definitely not work on a Windows machine**

This is the system that [NCX](https://ncx.com) uses to set up the PostGIS clone
of the FIA database that is accessible via the
[`tidyFIA`](https://github.com/ncx-co/tidyFIA) R package.

## Installation
You must install postgresql, postgis, and pgloader before running `fiadb`
```bash
# optional, add postgresql repository to get the latest version of postgresql
curl https://www.postgresql.org/media/keys/ACCC4CF8.asc | gpg --dearmor | sudo tee /etc/apt/trusted.gpg.d/apt.postgresql.org.gpg >/dev/null
sh -c 'echo "deb http://apt.postgresql.org/pub/repos/apt $(lsb_release -cs)-pgdg main" > /etc/apt/sources.list.d/pgdg.list'

sudo apt update && sudo apt install postgresql pgloader postgis
```

To install the python package, you can clone this repo and install with `pip`
```bash
git clone git@github.com:ncx-co/fiadb.git && cd fiadb && pip install -e .
```

## Building a database
You could populate a database to run on your local machine, but we recommend
setting up a database instance on a service like AWS RDS.

Before you populate the database, you will need to do these operations on the
PostgreSQL instance where your database will live.

1. Connect to database via `psql`:
```bash
psql --host <host> -p 5432 -U postgres -W
# provide password when prompted
```

2. Set up the `fiadb` database with `postgis` extensions:
```sql
-- First, revoke access for the role 'public'.
-- We do this to ensure that the role 'public', that all other users inherit
-- can't be used to mess with our tables.
-- Superusers (like postgres) will still be able to perform operations.
REVOKE ALL ON SCHEMA public FROM public;

DROP  DATABASE fiadb;
CREATE  DATABASE fiadb;
\c fiadb
CREATE EXTENSION postgis;
CREATE EXTENSION postgis_topology;
\q
```

### Download the FIA SQLite files
The database can be populated and updated from the state-level SQLite files
provided by FIA.
Downloading data for all 50 states will take several hours.
The `fiadb` function `storage.clone_fia` will download all 50 state-level
SQLite files.
```python
from fiadb import storage

db_files = storage.clone_fia(dest_dir="/tmp/fiadb_20230125")
```

### Database credentials
`fiadb` will connect to the database that you specify via a python dictionary.
The example below shows credentials for a PostgreSQL database running locally,
though you can provide credentials for a remotely hosted database
(e.g. AWS RDS).

```python
config = {
    "host": "localhost",
    "port": "5432",
    "dbname": "fiadb",
    "user": "postgres",
    "password": "password",
}
```

### Populate the database
The function `database.populate_db` will load the dated SQLite files from each
state into the database.
It takes a few hours to load all of the state-level SQLite files into the
database.

**Warning** This function will build the database from scratch and will delete existing tables when it starts!
```python
from fiadb import database

database.populate_db(
    config=config,
    db_files=db_files,
)
```

## Testing
To execute the unit tests, you must spin up a local postgresql database:
```bash
# spin up local postgresql database
sudo service postgresql start
sudo -u postgres psql -c "DROP DATABASE testfiadb;"
sudo -u postgres createdb testfiadb
sudo -u postgres psql -c "ALTER USER postgres PASSWORD 'password';"
sudo -u postgres psql -d testfiadb -c "CREATE EXTENSION postgis; CREATE EXTENSION postgis_topology;"
```

To run the unit tests:
```bash
# install the test requirements
pip install .[test]
pytest
```
The unit tests will modify the local testfiadb database that you created!
