# Override these ones with your own
INSTALL_CMD:=sudo cp -f
INSTALL_BIN:=/usr/local/bin


OBUILD_DIR:=../../_obuild
TARGET:=ocp-split-git-patch

all:
	ocp-build $(TARGET)

clean:
	rm -rf _obuild

install:
	$(INSTALL_CMD) $(OBUILD_DIR)/$(TARGET)/$(TARGET).asm \
           $(INSTALL_BIN)/$(TARGET)a
