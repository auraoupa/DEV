FC=ifort
NCDF=/opt/software/occigen/libraries/netcdf/4.4.0_fortran-4.4.2/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1
HDF=/opt/software/occigen/libraries/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1

FFLAGS= -O -I$(NCDF)/include -L$(NCDF)/lib -lnetcdf -lnetcdff -L$(HDF)/lib -lhdf5_hl -lhdf5 -ldl -lm -lz

mkmaskinit: mkmaskinit.f90
	$(FC) $(FFLAGS) mkmaskinit.f90 -o mkmaskinit -lnetcdf
diagmask: diagmask.f90
	$(FC) $(FFLAGS) diagmask.f90 -o diagmask -lnetcdf

