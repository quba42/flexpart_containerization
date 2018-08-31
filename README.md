We will use this *README* file for general comments and instructions on the project.

> Note that this repository has been extracted from a larger parent project.
> All commits past the `legacy_history` tag, stem from before it was turned into a stand alone repository.
> With the addition (merge into master) of this *README.md* file, the transition to a stand alone project is considered complete.

This project provides docker image sources for a number of containers that can be used to run pre configured FLEXPART simulations (see https://www.flexpart.eu/).
These pre configured FLEXPART simulations are designed to meet the needs of the AlpEnDAC project (see https://www.alpendac.eu).
Actually running simulations requires appropriate input data (not provided by this repository).


# Conventions

* All regular project file and folder names, should use all lower underscore case.
* We prefer to keep the use of abbreviation to a minimum in most cases.
* We prefer one line per sentence for text based source files.
  (One line is the smallest unit for which Git tracks changes).
* Work is done on the `active` and other branches, which can be rebased before merging into `master`.


# Project Folder Structure

* Each docker image build context provided, is placed within its own folder in the repository root.


# Basic Usage Instructions

The following describes a quick guide how to run a basic Flexpart simulation.
Currently it requires access to a sshfs based data repository belonging to AlpEnDAC.

> Note that these instructions presume a Linux system with a working Docker installation.
> For more on obtaining Docker, see https://docs.docker.com/install/.
> Ultimately, Docker is likely to be available via your distributions package manager.

> Note also that this repository was developed and tested with Docker version 18.03.1.
> It may not work with much older versions of Docker.

1. Clone this repository to any Linux host with a working Docker installation.
2. Change to the repositories base directory: `cd <path_to_git_repo>/flexpart_containerization/`
3. Build the main container image: `./build_alpendac_prototype.sh`
4. Obtain access to Flexpart input data. (Currently this involves contacting someone at AlpEnDAC about getting access).
5. Install `vieux/sshfs` (https://github.com/vieux/docker-volume-sshfs) in case you want to use the sshfs based data repository for input data: `docker plugin install vieux/sshfs`
6. Create the input data volume: `docker volume create -d vieux/sshfs -o sshcmd=weatherdata@<alpendac_sshfs_repo_ip_addr>:/weatherdata/GFS -o password=<password> -o port=<port> flexpart_input_sshfs`
7. Create a local output data volume: `docker volume create flexpart_output_local`
8. Run your first example simulation using default parameters: `./run_alpendac_prototype.sh`

> Note that the creation of any volume containing relevant Flexpart input data will work instead of steps 4-6 above.


# A Note on Licensing

This repository is released under version 3 of the GNU General Public License.
See the LICENSE file in the repository root for details.

Please note that this repository includes QuickLook sources as a submodule.
(Stored in the folder `/quicklook/sources/`).
These sources are not covered by the license of this repository, but rather by any licenses contained within the submodule.

Please note also that this repository contains Flexpart source code.
(Stored in the folder `/alpendac_prototype/flexpart_code/`).
Flexpart is itself released under version 3 of the GNU General Public License.
The version of the Flexpart source code included in this repository may have been modified from the original upstream sources.
It counts as a "modified version" as laid out in version 3 of the GNU GPL.
This modified version is released under version 3 of the GNU GPL without any additional conditions.


# TODO

* Extend the README.md file to provide a proper documentation.
* Add more extensive "sanity checks" to the quicklook container.
* Implement the missing command line options for the quicklook container.
