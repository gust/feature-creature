# feature-creature
A live editor for Cucumber features

---

## Dependencies
feature-creature uses Postgresql for data persistence. Refer to the
[Postgres Installation Guide](http://www.postgresql.org/download/) for system
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
#### Environment setup
The application depends on certain environment variables to exist in order to funcation
properly. The environment variables can be added by executing the follwing commands
within the terminal session in which you plan to run feature-creature.
```
export DB_NAME="[db-name-here]"        # required
export DB_HOST="[db-hostname here]"    # optional. defaults to 'localhost'
export DB_PORT="[db-port-number-here]" # optional. defaults to '5432'
export DB_USER="[db-username-here]"    # optional.
export DB_PASS="[db-password-here]"    # optional.
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
