FC=ifort
NCDF=/opt/software/occigen/libraries/netcdf/4.4.0_fortran-4.4.2/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1
HDF=/opt/software/occigen/libraries/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1

FFLAGS= -O -I$(NCDF)/include -L$(NCDF)/lib -lnetcdf -lnetcdff -L$(HDF)/lib -lhdf5_hl -lhdf5 -ldl -lm -lz

check_nan: check_nan.f90
	$(FC) $(FFLAGS) check_nan.f90 -o check_nan -lnetcdf

check_low: check_low.f90
	$(FC) $(FFLAGS) check_low.f90 -o check_low -lnetcdf

check_nan_and_replace: check_nan_and_replace.f90
	$(FC) $(FFLAGS) check_nan_and_replace.f90 -o check_nan_and_replace -lnetcdf
