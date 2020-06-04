# --- Source directories ---
vpath %.f90 external/ConfigLoader
vpath %.f90 src
vpath %.cl src

# --- Source names ---
# Headers - built first, contain subroutine interface definitions
# Modules - built second, contain subroutine collections
# Programs - built and linked last, contain main program(s)
# Kernels - contain OpenCL kernel code, linked into executable as binary resource
HEADERMODULES = ConfigLoader_h
MODULES = TecplotOutput UserInput TestCases ConfigLoader
PROGS = lbmocl
KERNELS = kernels.cl

# --- Output Directories ---
FOCAL_DIR ?= ./external/focal
OPENCL_DIR ?= /usr/lib/
BINDIR = ./bin/
OBJDIR = ./obj/
MODDIR = ./mod/

# --- Targets ---
PROGOBJS =$(addprefix $(OBJDIR), $(addsuffix .o, $(PROGS)))
MODOBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(MODULES)))
HEADEROBJS = $(addprefix $(OBJDIR), $(addsuffix .o, $(HEADERMODULES)))
EXEC = $(addprefix $(BINDIR), $(PROGS))
DIRS = $(MODDIR) $(BINDIR) $(OBJDIR)

# --- Compiler Flags ---
include make.compiler


# --- Link Flags ---
ifeq ($(BUILD), release)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lFocal
else ifeq ($(BUILD), debug)
    FOCAL_LFLAGS ?= -L$(FOCAL_DIR)/lib -lFocaldbg
else
    $(error unrecognized build.)
endif
OPENCL_LFLAGS ?= -g -L$(OPENCL_DIR) -lOpenCL
LFLAGS = $(FOCAL_LFLAGS) $(OPENCL_LFLAGS)

# --- Main build target ---
all: $(DIRS) $(EXEC)

include $(FOCAL_DIR)/make.include
FFLAGS+=-I$(FOCAL_MODDIR)

# --- Cleanup (reset) ---
clean: 
	rm -f $(OBJDIR)*.o
	rm -f $(MODDIR)*.mod
	rm -f $(MODDIR)*.smod
	rm -f $(BINDIR)*

# Programs depend on modules
$(PROGOBJS): $(MODOBJS) $(FOCAL_LIB_OBJS)

$(EXEC): $(FOCAL_LIB_OBJS)

# Modules depend on any header modules
$(MODOBJS): $(HEADEROBJS)

# Recipe to link executables
$(BINDIR)%: $(addprefix $(OBJDIR), %.o fclKernels.o) $(MODOBJS) $(HEADEROBJS) $(FOCAL_LIB_OBJS)
	$(FC) $(addprefix $(OBJDIR), $*.o fclKernels.o) $(MODOBJS) $(HEADEROBJS) $(LFLAGS) -o $@

# Recipe to compile fortran objects
$(OBJDIR)%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@

# Recipe to 'compile' kernel source into a binary object
$(OBJDIR)%.o: %.cl
	ld -r -b binary fclKernels.cl -o $@
	nm $@

# Recipe to concatenate kernel files
fclKernels.cl: $(KERNELS)
	cat $^ > fclKernels.cl

# Recipe to create output directories
$(DIRS):
	mkdir $@

.SECONDARY: $(addprefix $(OBJDIR), fclKernels.o)
