# ---------------------------------------------------------------------------
# File:   Makefile
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

WIN_PATH = 
FPC_PATH = E:\FPCdeluxe

GPP_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-g++.exe
GCC_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-gcc.exe

FPC_32 = $(FPC_PATH)/fpc/bin/i386-win32/ppcross386.exe
FPC_64 = $(FPC_PATH)/fpc/bin/i386-win32/ppcrossx64.exe

GPP_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-g++.exe
GCC_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-gcc.exe

FPC_CROSS = $(FPC_PATH)/bin/i386-win32/gcc.exe
FPC_LINUX = fpc
FPC_REL_FLAGS_CROSS_LINUX =\
    -Tlinux -dRELEASE -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ \
    -FE./obj \
    -vn- -Mdelphi
FPC_REL_FLAGS_LINUX =\
    -Fu./pas -Fu./ \
    -gl \
    -FE./obj \
    -vn- -Mdelphi
#
FPC_DEB_FLAGS_CROSS_LINUX =\
    -Tlinux -dDEBUG   -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ \
    -gl \
    -FE./obj \
    -vn- -Mdelphi
FPC_DEB_FLAGS_LINUX =\
    -Fu./pas -Fu./ \
    -gl \
    -FE./obj \
    -vn- -Mdelphi

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

WINDOWS_BIN_REL_32 = ./win32/Release
WINDOWS_BIN_REL_64 = ./win64/Release

WINDOWS_BIN_DEB_32 = ./win32/Debug
WINDOWS_BIN_DEB_64 = ./win64/Debug

TARGET = fpc-qt.dll

TARGET_REL_32 = $(WINDOWS_BIN_REL_32)/$(TARGET)
TARGET_REL_64 = $(WINDPWS_BIN_REL_64)/$(TARGET)

TARGET_DEB_32 = $(WINDOWS_BIN_DEB_32)/$(TARGET)
TARGET_DEB_64 = $(WINDOWS_BIN_DEB_64)/$(TARGET)

CPP_DIR = ./cpp
PAS_DIR = ./pas
OBJ_DIR = ./obj

SRCS = $(wildcard $(CPP_DIR)/*.cc)
OBJS = $(patsubst $(CPP_DIR)/%.cc,$(OBJ_DIR)/%.o,$(SRCS))

# ---------------------------------------------------------------------------
# start file
# ---------------------------------------------------------------------------
all: linux
#clean presteps $(TARGET_REL_64)

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_64): $(OBJS)
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $@ $^ -lQt5Core -lQt5Widgets

$(OBJ_DIR)/%.o: $(CPP_DIR)/%.cc
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $@ -c $<

# ---------------------------------------------------------------------------
# 32-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_32): $(OBJS)
	$(GPP_32) $(GPP_32_REL_FLAGS) -o $@ $^

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_LINUX_64): fpcqt.dpr
	$(FPC_64) $(FPC_REL_FLAGS_LINUX) fpcqt.dpr

linux: fpcqt.dpr
	echo "compile native under Linux:"
	$(FPC_LINUX) $(FPC_REL_FLAGS_LINUX) fpcqt.dpr

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

rebuild: clean all

presteps:
	mkdir -p ./obj
    
.PHONY: all clean rebuild linux
