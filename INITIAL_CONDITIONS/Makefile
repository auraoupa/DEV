include make.macro

EXE = ic_field_vertical_extent ic_changezgrlev ic_correct_low ic_correct_zero

all : $(EXE)

ic_field_vertical_extent:  ic_field_vertical_extent.f90
	$(F90) ic_field_vertical_extent.f90 -o ic_field_vertical_extent $(FFLAGS) $(NCDF)

ic_changezgrlev:  ic_changezgrlev.f90
	$(F90) ic_changezgrlev.f90 -o ic_changezgrlev $(FFLAGS) $(NCDF)

ic_correct_zero: ic_correct_zero.f90
	$(F90)  ic_correct_zero.f90 -o ic_correct_zero $(FFLAGS)  $(NCDF)


ic_correct_low: ic_correct_low.f90
	$(F90)  ic_correct_low.f90 -o ic_correct_low $(FFLAGS)

clean:
	\rm -f $(EXE) *~

install:
	cp $(EXE) $(INSTALL)
