FC=ifort
NCDF=/opt/software/occigen/libraries/netcdf/4.4.0_fortran-4.4.2/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1
HDF=/opt/software/occigen/libraries/hdf5/1.8.17/intel/17.0/openmpi/intel/2.0.1

FFLAGS= -O -I$(NCDF)/include -L$(NCDF)/lib -lnetcdf -lnetcdff -L$(HDF)/lib -lhdf5_hl -lhdf5 -ldl -lm -lz

modif_bathy: modif_bathy.f90
	$(FC) $(FFLAGS) modif_bathy.f90 -o modif_bathy -lnetcdf

