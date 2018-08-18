We will use this *README* file for general comments and instructions on the project.

> Note that this repository has been extracted from a larger parent project.
> All commits past the `legacy_history` tag, stem from before it was turned into a stand alone repository.
> With the addition (merge into master) of this *README.md* file, the transition to a stand alone project is considered complete.

This project provides docker image sources for a number of containers that can be used to run pre configured FLEXPART simulations (see https://www.flexpart.eu/).
These pre configured FLEXPART simulations are designed to meet the needs of the AlpEnDAC project (see https://www.alpendac.eu).
Actually running simulations requires appropriate input data (not provided by this repository).

# Conventions

* All regular project file and folder names, should use all lower underscore case.
* We prefer to keep the use of abreviation to a minimum in most cases.
* We prefer one line per sentence for text based source files.
  (One line is the smallest unit for which Git tracks changes).
* Work is done on the `active` and other branches, which can be rebased before merging into `master`.


# Project Folder Structure

* Each docker image build context provided, is placed within its own folder in the repository root.

# TODO

* Extend the README.md file to provide a proper documentation.
* Add more extensive "sanity checks" to the quicklook container.
* Implement the missing command line options for the quicklook container.
