# Ncdfutil: The netCDF utilities
 
This README file describes what you need to do to build the NcdfUtilities executable and library files.

## Directory Structure:

The root-level directory is GEOS_5/, which contains these subdirectories:

  - `Code/`: Folder containing Fortran source code files
  - `bin/`: Folder where the TestNcdfUtilities.x executable will be sent
  - `lib/`: Folder where the library file `libNcUtils.a` will be built
  - `mod/`: Folder where the Fortran module files (`.mod`) will be placed.


## System Requirements:

In order to use NcdfUtilities, you will first have to make sure that the netCDF library is installed on your system.  Consult with your local sysadmin as to where these libraries are found. 

If the netCDF libraries have not been installed, you may [install them with Spack](https://geos-chem.readthedocs.io/en/latest/geos-chem-shared-docs/supplemental-guides/spack.html)


## Setting environment variables:

The NcdfUtilities library requires that you set the following environment variables in your system startup file (e.g. `.bashrc`):

  1. `NETCDF_BIN`: The `bin/` folder of the netCDF installation, where utilities such as `nc-config` are stored 
           
  2. `NETCDF_INCLUDE`: The `include/`" folder of the netCDF installation, where the `netcdf.inc` and `netcdf_mod.F90` are found.
        
  3. `NETCDF_LIB`: The `lib/` or `lib64/` folder of the netCDF installation, where the netCDF library files (ending in `*.a`) 
	 files are found.

NOTE: In netCDF-4.2 and higher versions, the netCDF Fortran libraries are built from a separate distribution.  If on your system, the netCDF-Fortran libraries have been installed into a different folder than the rest of the netCDF libaries, you will also need to set these environment variables
in your system startup file:

  4. `NETCDF_FORTRAN_BIN`: The `bin/` folder of the netCDF-Fortran installation, where "nf-config" is found. 

  5. `NETCDF_FORTRAN_INCLUDE`: The `include/` folder of the  netCDF-Fortran installation, where `netcdf.inc` is found.

  6. `NETCDF_FORTRAN_LIB` The `lib/` or `lib64/` folder of the netCDF-Fortran installation, where the library files (ending in
      `*.a`) are found. 


## Building the ncdfutil library

The `Code` folder the Fortran source code modules as well as two Makefiles (named `Makefile` and `Makefile_header.mk).

The file `Makefile_header.mk` is a sub-makefile which is used to define the compilation options for different compilers.  At present, the `ifort`, `gfortran`, and `pgfortran` compilers are supported.

Once you have set the proper environment variables for your system (as described above), you are ready to build the executable.  Make sure you are in the `Code/` folder and type:

```console
$ make lib
```

This should start building the source code and create a library file named `libNcUtils.a` in the `lib/` folder.


## Testing the ncdfutil library
 
Once the `libNcUtils.a` file has been created in the `lib` folder, you can test to see if the library was created (and can link to) the netCDF library correctly.  Type:

```console
$ make check
```

This will create an executable file named "TestNcdfUtilities.x" in the bin subdirectory, and will also execute the file.  If the `libNcUtils.a` library was installed correctly you should see the following output:

```console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  Testing libNcdfUtilities.a  %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
=== Begin netCDF file creation test ===
Writing XDim (# lons)   to netCDF file
Writing YDim (# lats)   to netCDF file
Writing ZDim (# alts)   to netCDF file
Writing LON  (1D array) to netCDF file
Writing LAT  (1D array) to netCDF file
Writing PLEV (1D array) to netCDF file
Writing PS   (2D array) to netCDF file
Writing T    (3D array) to netCDF file
=== End netCDF file creation test ===
=== Begin netCDF file reading test ===
Reading XDim back from netCDF file...........PASSED
Reading YDim back read from netCDF...........PASSED
Reading ZDim back from netCDF file...........PASSED
Reading LON  back from netCDF file...........PASSED
Reading LAT  back from netCDF file...........PASSED
Reading PLEV back from netCDF file...........PASSED
Reading PS   back from netCDF file...........PASSED
Reading T    back from netCDF file...........PASSED
=== End of netCDF file read test! ===
```

If all of the tests return with `PASSED` then the `libNcUtils.a` file was created correctly.

## Cleaning up

To remove all of the `*.o`, `*.mod` and executable file (`*.x`) in the `Code/` folder only, type:

```console
$ make clean
```

However, if you wish to also remove the contents of the `bin/` and `lib/` subdirectories then type:

```console
$ make distclean
```


    
