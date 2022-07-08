# Discrete-Event Simulation using R-simmer

This repository contains the two basic example simulations covered in the tutorial held during WueWoWas'22.

The scripts all exhibit the following dependencies.

* [tidyverse](https://www.tidyverse.org/) for plotting and data manipulation
* [r-simmer](https://r-simmer.org/) for the actual simulation framework
* [futil.logger](https://cran.r-project.org/web/packages/futile.logger/index.html) for progress visualization

Note that `tidyverse` requires the following non-R dependencies, which need to be installed using the package manager of your system, if you are running Linux.

`sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev`

This should not be required if running Windows or MacOS.

## Simulating Common Queuing Systems

The files located in `R/ex1` provide a basis for the simulation of well known queuing systems, such as `Gi/Gi/1-\infty` or `Gi/Gi/1-L`. They can easily be adapted to simulate `M/M/n-L` waiting or loss systems, by adapting the corresponding arrival or service distributions.

* `ex_1.R` contains only the basic structure and leaves the critical tasks for the participants to implement
* `ex_1_solution.R` contains the same structure but includes an example implementation of the TODOs
* `ex_1_dta.py` contains python code that evaluates the waiting time of the system implemented in `ex_1_solution.R`. This file can be sourced using the `reticulate` package, to compare results between the analytical model and the simulation.

## More Complex Example: Simulation of a Video Streaming System

The files located in `R/video_streaming` provide an example simulation of a more complex system. Specifically, the files start with a rough abstraction of a video streaming system, which is then developed into a more detailed system model.

* `001_basic.R` contains the most basic form of the simulation. Here, clients request a video that exhibits two basic parameters, duration and bitrate. The server is modeled as a single entity that has the maximum bitrate it can stream concurrently, as an approximation of the available bandwidth.
* In `002_infrastructure.R` the system is refined by adding additional infrastructure components. Instead of only the streaming server, the system now consists of a frontend, backend and server component. Each client requires resources from all components in order to be served effectively.
* In `003_clients.R` the simulation is extended by adding a simple model for client behavior. Instead of simply arriving and requesting resources, clients now exhibit a basic form of "real" behavior. The browse the platform, thereby requesting resources from the frontend and backend. Once a video has been selected based on a stochastic process, clients start streaming the video. During video streaming, clients exhibit a basic form of the DASH mechanism, meaning they request videos in chunks that are stored in a local buffer. Once the buffer reaches a certain fill level, the next chunk is requested.

Note that this simulation contains a multitude of very rough approximations and should not be used for anything serious. However, with a little work it would probably be possible to introduce a realistic level of detail to model a real-world system.