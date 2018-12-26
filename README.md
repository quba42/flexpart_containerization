This `README.md` file contains the main project documentation.
The `documentation` folder also includes the final version of the bachelors thesis that originally motivated this project. It serves as a very detailed background documentation.


# Introduction

This project provides Docker image sources for a number of containers that can be used to run pre configured FLEXPART simulations (see https://www.flexpart.eu/).
These pre configured FLEXPART simulations were originally designed to meet the needs of the AlpEnDAC project (see https://www.alpendac.eu).

The project now aims to transition these AlpEnDAC specific FLEXPART containers for fully general FLEXPART use.
The hope is that this will provide some benefit to the FLEXPART community.

As of this writing, actually running simulations requires appropriate input data not provided by this repository.


# Usage

This project aims to provide out of the box FLEXPART simulations on any linux host with a working Docker installation.

> See https://docs.docker.com/install/ for more on obtaining Docker.
> It should generally be possible to install Docker via the package manager of any major linux distribution.

As of this writing there is one additional major caveat:
The Docker images provided by this repository require access to FLEXPART input data belonging to the AlpEnDAC project.
Using any other input data likely requires extensive reconfiguration of the FLEXPART model configuration used.
Providing alternatives is a priority for this project.

> Note also, that this repository was developed and tested with Docker version 18.03.1.
> It may not work with much older versions of Docker.


## Quick Start Guide

The following describes how to run a basic FLEXPART simulation.
Currently it requires access to FLEXPART input data belonging to AlpEnDAC.

1. Clone this repository to any Linux host with a working Docker installation.
2. Change working directory to the repository base: `cd flexpart_containerization`
3. Run `git submodule init` and `git submodule update` to ensure the submodules are downloaded.
4. Build the main container image: `./scripts/build_flexpart.sh`
5. Create the needed data volumes: `docker volume create flexpart_input_local` and `docker volume create flexpart_output_local`
6. Obtain access to FLEXPART input data. (Currently this requires contacting someone at AlpEnDAC).
7. Ensure that your FLEXPART input data is placed within the `flexpart_input_local` volume you just created.
8. Run your first example simulation using default parameters: `./scripts/run_flexpart.sh`

The output data of your FLEXPART run will be placed inside the `flexpart_output_local` volume.

> Note that there are many possible ways to achieve steps 5 to 7 above.
> The official Docker documentation (https://docs.docker.com) is a good next point of call.


## Advanced Usage

Once you have achieved a successfull containerized FLEXPART run, you can start experimenting with the run configuration found under `env_files/flexpart.env`.
If the provided configuration parameters are insufficient you will need to manually modify the FLEXPART model configuration used under `image_sources/flexpart/model/` and rebuild the Docker image with your modifications.
It is the aim of this project to keep extending the provided configuration interface until such manual model adjustments become unnecessary.

This repository also provides a QuickLook container image that can be used to run post processing visualizations on FLEXPART output data.
The image can be built by running `./scripts/build_quicklook.sh` from the repository root.


# Contribution Guide
## General Conventions

* All regular project file and folder names, should use all lower underscore case.
* We prefer to keep the use of abbreviation to a minimum in most cases.
* We prefer one line per sentence for text based source files.
  (One line is the smallest unit for which Git tracks changes).
* Work is done on the `active` and other branches, which can be rebased before merging into `master`.


## Repository Structure
## Tags and Version History

This repository currently includes two tags in its history.

The `alpendac_portotype` tag marks the final point in this repositories history that is referenced by the bachelors thesis that originally motivated this project.
The final version of this bachelors thesis can be found under `documentation/bachelors_thesis.pdf`.

The `legacy_history` tag marks the point in the history at which this repository was extracted from its non public parent repository to form a stand alone project.

Any future tags should mark the point at which a specific container image is released in a specific version.
Version tags for containers should use the following format: `<container_name>_<contained_software_version>-<container_version>`
For example, the point in the history that first provides a container image containing FLEXPART version `9.3.2e` would be tagged with `flexpart_9.3.2e-1`.
Any FLEXPART images built from this point in the history would then be tagged (using the Docker tag mechanism) with `flexpart/flexpart:9.3.2e-1`.
Any subsequently modified releases of the same container image containing the same version of FLEXPART could then be tagged `flexpart_9.3.2e-2`, `flexpart_9.3.2e-3` and so on.


# A Note on Licensing

This repository is released under version 3 of the GNU General Public License.
See the LICENSE file in the repository root for details.

Note that this repository contains seperate projects as Git submodules.
Any so included sources are not covered by the license of this repository, but rather by any license contained within the submodules themselves.
You can use the command `git submodule` to obtain a list of the currently included submodules.
