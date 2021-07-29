#
#
#
FC=gfortran
NFDIR=/usr/local

INCLUDES = -I${NFDIR}/include
LFLAGS = -L${NFDIR}/lib
LIBS = -lnetcdff
FCLAGS =  -fcheck=bounds

SRCS = macs_4D_rd_hourly.f90

MAIN = cams.x

.PHONY: clean

OBJS = $(SRCS:.f90=.o)

all: $(MAIN)
	@echo  Simple compiler named mycc has been compiled

$(MAIN): $(OBJS)
	$(FC) $(INCLUDES) -o $(MAIN) $(OBJS) $(LFLAGS) $(FCLAGS) $(LIBS)

clean:
	$(RM) *.o *~ $(MAIN)

$(OBJS): macs_4D_rd_hourly.f90
	$(FC) $(INCLUDES) -c macs_4D_rd_hourly.f90 -o macs_4D_rd_hourly.o

#.f90.o:
#	$(FC) $(INCLUDES) -c $< -o $@
