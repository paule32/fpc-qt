# ---------------------------------------------------------------------------
# File:   Makefile
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

WIN_PATH = 

GPP_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-g++.exe
GCC_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-gcc.exe

GPP_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-g++.exe
GCC_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-gcc.exe

QT_632_INC =\
    -I./cpp \
    -I$(WIN_PATH)/mingw64/include

GCC_32_REL_FLAGS  = -m32 -O2 -wno-write-strings -fPIC -shared
GCC_64_REL_FLAGS  = -m64 -O2 -wno-write-strings -fPIC -shared

GCC_32_DEB_FLAGS += $(GCC_32_REL_FLAGS) -g gdb
GCC_64_DEB_FLAGS += $(GCC_32_REL_FLAGS) -g gdb

GPP_XX_REL_FLAGS = -O2 -Wno-write-strings -fPIC -shared -std=c++20
GPP_32_REL_FLAGS = $(GPP_XX_REL_FLAGS) -m32 $(INCLUDES)
GPP_64_REL_FLAGS = $(GPP_XX_REL_FLAGS) -m64 $(INCLUDES) -DDEBUG

GPP_32_DEB_FLAGS = $(GPP_32_REL_FLAGS) -g gdb
GPP_64_DEB_FLAGS = $(GPP_64_REL_FLAGS) -g gdb

BIN_REL_32 = ./win32/Release
BIN_REL_64 = ./win64/Release

BIN_DEB_32 = ./win32/Debug
BIN_DEB_64 = ./win64/Debug

TARGET = fpc-qt.dll

TARGET_REL_32 = $(BIN_REL_32)/$(TARGET)
TARGET_REL_64 = $(BIN_REL_64)/$(TARGET)

TARGET_DEB_32 = $(BIN_DEB_32)/$(TARGET)
TARGET_DEB_64 = $(BIN_DEB_64)/$(TARGET)

SRC_DIR = ./cpp
OBJ_DIR = ./obj

SRCS = $(wildcard $(SRC_DIR)/*.cc)
OBJS = $(patsubst $(SRC_DIR)/%.cc,$(OBJ_DIR)/%.o,$(SRCS))

# ---------------------------------------------------------------------------
# start file
# ---------------------------------------------------------------------------
all: clean presteps $(TARGET_REL_64)

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_64): $(OBJS)
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $@ $^  -lQt5Core

$(OBJ_DIR)/%.o: $(SRC_DIR)/%.cc
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $@ -c $<

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_32): $(OBJS)
	$(GPP_32) $(GPP_32_REL_FLAGS) -o $@ $^

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

rebuild: clean all

presteps:
	mkdir -p ./obj
    
.PHONY: all clean rebuild
