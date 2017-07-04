TARGET = conway

export FILENAME = $(shell echo $(TARGET) | tr a-z A-Z)
TYPE = B

ASM = dasm
AC = java -jar ${APPLE_COMMANDER}

SDIR = ./src
_SRC = $(TARGET).asm
SRC = $(patsubst %,$(SDIR)/%,$(_SRC))

IDIR = ./include
_DEPS = apple2.asm macros.asm
DEPS = $(patsubst %,$(IDIR)/%,$(_DEPS))

BDIR = ./build
_OBJ = $(TARGET)
OBJ = $(patsubst %,$(BDIR)/%,$(_OBJ))
_LST = $(TARGET).lst
LST = $(patsubst %,$(BDIR)/%,$(_LST))
_IMAGE = $(TARGET).dsk
export IMAGE = $(patsubst %,$(BDIR)/%,$(_IMAGE))

RDIR = ./res
_BOOTIMAGE = dos3.3bootable.dsk
BOOTIMAGE = $(patsubst %,$(RDIR)/%,$(_BOOTIMAGE))
_RUNNER = runner.scpt
RUNNER = $(patsubst %,$(RDIR)/%,$(_RUNNER))

all: $(OBJ)

$(OBJ): $(SRC) $(DEPS)
	@mkdir -p $(BDIR)
	$(ASM) $< -o$@ -l$(LST) -v4 -f2

$(IMAGE): $(OBJ)
	cp $(BOOTIMAGE) $(IMAGE)
	$(AC) -cc65 $(IMAGE) $(FILENAME) $(TYPE) < $(OBJ)

run: $(IMAGE)
	osascript $(RUNNER)
	@echo Running...

clean:
	@rm -rf $(BDIR)
	@echo Cleaned!