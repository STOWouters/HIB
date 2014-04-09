# List all targets implemented
.PHONY:	all build install uninstall clean

# List all variables
EXE			:= Battleship
HCOMPILER	:= ghc -outputdir build -o bin/$(EXE)
MKDIR		:= mkdir -v -p
RM			:= rm -f -v -r
CP			:= cp -u -r -v

# Where to put the executables
BIN	:= /usr/local/bin

all: build

# Just build everything you need before installing (but do not perform the
# installation itself).
build: Main.hs
	@$(MKDIR) bin/ build/
	@$(HCOMPILER) Main.hs

# Perform the installation
install: build bin/$(EXE)
	@$(CP) bin/$(EXE) $(BIN)/

# Undo the commands executed at the `install` target.
uninstall:
	@$(RM) $(BIN)/$(EXE)

# Remove all files created on `build` target
clean:
	@$(RM) bin/ build/
