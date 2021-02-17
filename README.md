## *D2Q9 BGK Lattice Boltzmann code written in OpenCL and Modern Fortran.*

An OpenCL demonstration code making use of the [Focal](https://github.com/LKedward/focal) OpenCL abstraction library for Fortran.

![D2Q9 Lattice Boltzmann](https://github.com/LKedward/lbm2d_opencl/blob/master/cylinder_flow.gif "D2Q9 Lattice Boltzmann")

## Getting started

### Prerequisites

- A modern fortran compiler supporting the 2008 standard (tested with gfortran 9.1.0)
- An openCL development library (One of:
[Intel OpenCL SDK](https://software.intel.com/en-us/opencl-sdk), 
[NVIDIA CUDA Toolkit](https://developer.nvidia.com/cuda-downloads), 
[AMD OpenCL SDK](https://github.com/GPUOpen-LibrariesAndSDKs/OCL-SDK/releases) )


### Download

This project makes use of [git submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules) to bundle a library dependency.

Therefore to clone the repository:

```shell
$> git clone git@github.com:LKedward/lbm2d_opencl.git
$> git submodule init
$> git submodule update
```

### Build

To compile, navigate to the repository directory and run `make`:

```shell
$> make -j
```

Parallel build is fully supported so use of the `-j` flag is recommended.
This will place executables in the `./bin/` directory.

To build the `debug` version:

```shell
$> make -j BUILD=debug
```

If the linker is unable to locate `-lOpenCL` you may have to specify the location of the OpenCL library (libOpenCL.so):

```shell
$> make -j OPENCL_DIR=/path/to/OpenCL/lib/
```
*This should not be necessary on Linux platforms if the OpenCL development library is installed correctly*


### Run

To run the compiled program:

```shell
$> ./bin/lbmocl
```

## Configuration file

When called with no arguments, the `lbmocl` program will run with a default configuration.
To specify alternative configuration options, specify a configuration file as the first argument, *e.g.*:

```shell
$> ./bin/lbmocl lbm.cfg
```

The following optional parameters may be specified in the configuration file:

`CASE`: (*string*) which built-in test case to run. Must be one of: `CAVITY`, `CYLINDER`, `STEP`. Default: `CAVITY`.

`NI`: (*integer*) number of lattice points in vertical direction. Default: `400`.

`N_ITER`: (*integer*) number of unsteady timestep iterations to perform. Default: `5000`.

`SAVE_FREQ`: (*integer*) how often to calculate macroscopic variables and save to disk. Default: `100` (every 100 timesteps).

`SAVE_FILE`: (*string*) filename of tecplot output. Default: `lbm.plt`.

`SAVE_TYPE`: (*string*) tecplot save mode. Must be one of: `BIN`, `TXT` for binary (fast,small) and ascii (portable) modes respectively. Default: `BIN` (recommended).

`CL_VENDOR`: (*string*) which hardware vendor to select device. Default: `NVIDIA`.



The following options override parameters defined by the built-in test cases:

`DX`: (*float*) overrides test case lattice grid spacing.

`DT`: (*float*) overrides test case time step.

`TAU`: (*float*) overrides test case relaxation parameter.


## About the code

### Files and directories

- `./src/lbmocl.f90`: main program code
- `./src/kernels.cl`: OpenCL C kernel code implementing the lattice Boltzmann method
- `./src/UserInput.f90`: module for processing user inputs from configuration file
- `./src/TecplotOutput.f90`: module for writing solutions to tecplot files
- `./external/`: directory containing code for external libraries
- `./makefile`: makefile
- `./make.compiler`: defines compiler flags, 'included' in `makefile`
- `./lbm.cfg`: example configuration file containing parameter defaults

## Acknowledgement

_This work was funded by the MENtOR project, a UKVLN project supported by the Engineering and Physical Sciences Research Council (EPSRC) of the UK. Grant reference number EP/S010378/1_
