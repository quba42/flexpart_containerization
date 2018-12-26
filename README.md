This `README.md` file contains the main project documentation.
The `documentation` folder also includes the final version of the bachelors thesis that originally motivated this project. It serves as a very detailed background documentation.

This project provides docker image sources for a number of containers that can be used to run pre configured FLEXPART simulations (see https://www.flexpart.eu/).
These pre configured FLEXPART simulations were originally designed to meet the needs of the AlpEnDAC project (see https://www.alpendac.eu).

The project now aims to transition these AlpEnDAC specific FLEXPART containers for fully general FLEXPART use.
The hope is that this will provide some benefit to the FLEXPART community.

As of this writing, actually running simulations requires appropriate input data not provided by this repository.


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

# Usage
# Contribution Guidelines
## General Conventions

* All regular project file and folder names, should use all lower underscore case.
* We prefer to keep the use of abbreviation to a minimum in most cases.
* We prefer one line per sentence for text based source files.
  (One line is the smallest unit for which Git tracks changes).
* Work is done on the `active` and other branches, which can be rebased before merging into `master`.


# Repository Structure
# Tags and Version History

This repository currently includes two tags in its history.

The `alpendac_portotype` tag marks the final point in this repositories history that is referenced by the bachelors thesis that originally motivated this project.
The final version of this bachelors thesis can be found under `documentation/bachelors_thesis.pdf`.

The `legacy_history` tag marks the point in the history at which this repository was extracted from its non public parent repository to form a stand alone project.

Any future tags should mark the point at which a specific container image is released in a specific version.
Version tags for containers should use the following format: `<container_name>_<contained_software_version>-<container_version>`
For example, the point in the history that first provides a container image containing FLEXPART version `9.3.2e` would be tagged with `flexpart_9.3.2e-1`.
Any flexpart images built from this point in the history would then be tagged (using the Docker tag mechanism) with `flexpart/flexpart:9.3.2e-1`.
Any subsequently modified releases of the same container image containing the same version of FLEXPART could then be tagged `flexpart_9.3.2e-2`, `flexpart_9.3.2e-3` and so on.


# A Note on Licensing

This repository is released under version 3 of the GNU General Public License.
See the LICENSE file in the repository root for details.

Note that this repository contains seperate projects as Git submodules.
Any so included sources are not covered by the license of this repository, but rather by any license contained within the submodules themselves.
You can use the command `git submodule` to obtain a list of the currently included submodules.
