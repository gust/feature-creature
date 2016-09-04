# Users Service

Provides a user data service for a web product.

# Building the project

Compatible OS's: Linux, OSX

### System dependencies

__Stack build tool__

This project uses Stack as a build tool. Follow the [instructions](http://docs.haskellstack.org/en/stable/README/) to install Stack on your system.


__Postgresql database__

The application is backed by a Postgres database. See the [instructions](https://wiki.postgresql.org/wiki/Detailed_installation_guides) for installing on your
machine.


### Quick definition

_project root_ = directory containing the `stack.yaml` file.

All commands, unless otherwise specified, will expect to be run from the _project root_


### Initializing the Stack environment

Initialize Stack and GHC by running:

```bash
stack setup
```

The above step is a one time only setup instruction. You should not have to
run it every time you build the project.


### Building the source

To pull all of the application dependencies, build from source, and link the 
required libraries, run the following:

```bash
stack build
```

It will take a while for the application to initally pull all of its 
dependencies. Don't worry, though. The following builds will be quick.


### Running the tests

First, let's ensure our databases and roles aresetup correctly. Run the following:

```bash
./users-service/scripts/setup-database.sh
```

To run the application test suite:

```bash
stack test
```

During development, I find it useful to run the file watcher so that the tests
(or build) will run as I save my source files. To enable this, run:

```bash
stack test --file-watch
```


### running the web server

There is a script to manage
- copying env vars to the right place (linux only and currently terrible)
- running the web server on port 8080

```bash
./server-local.sh
```


### contributing

- Always be linting (hlint)
- Treat warning as errors (both build warning and linter warnings)
