import setuptools


setuptools.setup(
    name="fiadb",
    version="0.0.1",
    author="Henry Rodman",
    author_email="henry@ncx.com",
    description="Maintain a clone of the FIA database",
    packages=setuptools.find_packages(),
    install_requires=[
        "path",
        "psycopg2",
        "tqdm",
        "us",
    ],
    extras_require={
        "test": [
            "pytest",
        ],
    },
)
