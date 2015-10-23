# feature-creature
A live editor for Cucumber features

![](http://www.homecinemachoice.com/sites/18/images/article_images_month/2012-07/universal%20monsters%20news%2001.jpg)

---

## Dependencies
Postgresql for data persistence. Refer to the

[Postgres Installation Guide](http://www.postgresql.org/download/) for system
specific instructions.

Git for Cucumber feature file retrieval and synchronization.

[Git Installation Guide](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git) for system
specific instructions.

---

## Building feature-creature
feature-creature uses [Cabal](http://www.haskell.org/cabal/) as a build tool.

* clone the repository
* change to the newly created `feature-creature` directory (or the directory you specified)
* run `cabal sandbox init` (optional, but recommended to isolate dependencies)
* run `cabal install`

After a successful build, Cabal will output the location of the feature-creature binary.

---

## Example Usage
The application depends on a database and certain environment variables to exist in order to 
funcation properly. 
#### Quick Environment Setup
Prerequisites: The quick setup script assumes Postgresql is already installed.

The quick setup script will add required environment variables, create a database user,
and create a database for feature-creature to use. Please view the source file for all 
default values.

If there already exists a database or database user, an error message will be echoed and 
can be safely ignored.
```
# from the project root
source ./scripts/development/bootstrap-env.sh
```
#### Custom Environment Setup
The environment variables can be added by executing the follwing commands
within the terminal session in which you plan to run feature-creature.
```
export FC_DB_NAME="[db-name-here]"        # required
export FC_DB_HOST="[db-hostname here]"    # optional. defaults to 'localhost'
export FC_DB_PORT="[db-port-number-here]" # optional. defaults to '5432'
export FC_DB_USER="[db-username-here]"    # optional.
export FC_DB_PASS="[db-password-here]"    # optional.

# required. must be a writable location
# defaults to the "$HOME/feature-creature/data"
export FC_DATA_FILES_PATH="[path-to-application-data-files]"
```
#### Running the application
Execute the binary and follow the CLI output help pages to discover existing functionality.

---

## Contributing
1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create a new Pull Request
