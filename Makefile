FFLAGS := -O3 -Wall -pedantic
FORTRAN := gfortran
TARGETS := proj2d

.PHONY: all clean

all: $(TARGETS)

clean:
	rm -rf $(TARGETS) *.o

proj2d: proj2d.o
	$(FORTRAN) $(FFLAGS) -o $@ $<

%.o: %.f03
	$(FORTRAN) $(FFLAGS) -c -o $@ $<
