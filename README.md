# What is it?

Morph is a simple tool to migrate databases on PostgreSQL. It supports
rollbacking from other versions without requiring the migration files from the
previous version. Hence, to migrate to a new version, you only need the
migration files from this particular version.

# Migration files

Each migration file should be prefixed with a numerical identifier. These
identifiers determine the order in which the migrations are going to be ran.
Thus, a good way to avoid conflict with other commiters is to prefix it with a
timestamp formatted as `YYYYMMDDHHMM`.

Each migration has two SQL files: one ending with `.up.sql` and the other by
`.down.sql`, respectively the request to migrate and the one to rollback.
Failing to have a `.down.sql` file will result in an error when attempting to
rollback but will not prevent the initial migration.

For example, you could have the following files:

**migrations/201603291434_create_users.up.sql**

    CREATE TABLE users (
           id       serial  PRIMARY KEY,
           username varchar NOT NULL UNIQUE CHECK (username <> ''),
           password varchar NOT NULL CHECK (password <> '')
    );

**migrations/201603291434_create_users.down.sql**

    DROP TABLE users;

**migrations/201604011203_add_email_to_user.up.sql**

    ALTER TABLE users ADD COLUMN email varchar;

**migrations/201604011203_add_email_to_user.down.sql**

    ALTER TABLE users DROP COLUMN email;

# Config file

Morph reads the database connection information from a config file. This is
intended to be the same file you use for your application. It only supports JSON
at the moment. An object (potentially nested, see options) needs to be available
with the following structure:

    {
      "user": "_postgresql",
      "password": "",
      "host": "127.0.0.1",
      "port": 5432,
      "name": "my_database"
    }

# Options

See `morph --help` for details about command-line options.
