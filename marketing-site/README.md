# Example Basic Web Site

The intention of this project is to provide an example landing site for users
first arriving at a web product.

There are public and private routes to explore.
- "/" a public home page
- "/private" a private page protected by an authentication token

Authentication is provided by a separate service.



# Building the project

Compatible OS's: Linux, OSX

### System dependencies

__Stack build tool__

This project uses Stack as a build tool. Follow the [instructions](http://docs.haskellstack.org/en/stable/README/) to install Stack on your system.


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

To run the application test suite:

```bash
stack test
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

During development, I find it useful to run the file watcher so that the tests
(or build) will run as I save my source files. To enable this, run:

```bash
stack test --file-watch
```

