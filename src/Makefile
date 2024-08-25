# ---------------------------------------------------------------------------
# File:   Makefile
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# get operating system, and architecture ...
# ---------------------------------------------------------------------------
OS   := $(shell uname -s)
ARCH := $(shell uname -m)

CPP_DIR = ./cpp
PAS_DIR = ./pas
OBJ_DIR = ./obj

ifeq ($(OS),Windows_NT)
WIN_PATH = 
FPC_PATH = E:\FPCdeluxe

GPP_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-g++.exe
GCC_32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-gcc.exe

FPC_32 = $(FPC_PATH)/fpc/bin/i386-win32/ppcross386.exe
FPC_64 = $(FPC_PATH)/fpc/bin/i386-win32/ppcrossx64.exe

GPP_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-g++.exe
GCC_64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-gcc.exe

QT_632_INC =\
    -I./cpp \
    -I$(WIN_PATH)/mingw64/include

FPC_CROSS = $(FPC_PATH)/bin/i386-win32/gcc.exe

FPC_REL_FLAGS_LINUX =\
    -Tlinux -dRELEASE -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ \
    -FE./obj \
    -vn- -Mdelphi
#
FPC_DEB_FLAGS_LINUX =\
    -Tlinux -dDEBUG   -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ -Fl./obj \
    -gl \
    -FE./obj \
    -vn- -Mdelphi

else ifeq ($(OS),Linux)
GPP_64    = g++
INCLUDES +=\
    -I /usr/include/x86_64-linux-gnu/qt5/

FPC_LINUX = fpc
FPC_REL_FLAGS_LINUX =\
    -Fu./pas -Fu./ \
    -gl -Cn  \
    -FE./obj \
    -vn- -Mdelphi
FPC_DEB_FLAGS_LINUX =\
    -Fu./pas -Fu./ \
    -gl -Cn  \
    -FE./obj \
    -vn- -Mdelphi
endif

CC_FLAGS = -O2 -Wno-write-strings

GCC_32_REL_FLAGS += -m32 $(CC_FLAGS)
GPP_32_REL_FLAGS += -m32 $(CC_FLAGS) -fPIC -shared -std=c++2a
#
GCC_32_DEB_FLAGS += -m32 $(CC_FLAGS) -g gdb
GPP_32_DEB_FLAGS += -m32 $(CC_FLAGS) -fPIC -shared -std=c++2a
###
GCC_64_REL_FLAGS += -m64 $(CC_FLAGS)
GPP_64_REL_FLAGS += -m64 $(CC_FLAGS) -fPIC -shared -std=c++2a
#
GCC_64_DEB_FLAGS += -m64 $(CC_FLAGS) -g gdb
GPP_64_DEB_FLAGS += -m64 $(CC_FLAGS) -fPIC -shared -std=c++2a

GCC_32_REL_FLAGS += $(INCLUDES) -DRELEASE
GPP_32_REL_FLAGS += $(INCLUDES) -DRELEASE
GCC_64_REL_FLAGS += $(INCLUDES) -DRELEASE
GPP_64_REL_FLAGS += $(INCLUDES) -DRELEASE
#
GCC_32_DRB_FLAGS += $(INCLUDES) -DDEBUG -g gdb
GPP_32_DRB_FLAGS += $(INCLUDES) -DDEBUG -g gdb
GCC_64_DEB_FLAGS += $(INCLUDES) -DDEBUG -g gdb
GPP_64_DRB_FLAGS += $(INCLUDES) -DDEBUG -g gdb

ifeq ($(OS),Windows_NT)
WINDOWS_BIN_REL_32 = ./win32/Release
WINDOWS_BIN_REL_64 = ./win64/Release

WINDOWS_BIN_DEB_32 = ./win32/Debug
WINDOWS_BIN_DEB_64 = ./win64/Debug

TARGET = fpc-qt.dll

TARGET_REL_32 = $(WINDOWS_BIN_REL_32)/$(TARGET)
TARGET_REL_64 = $(WINDPWS_BIN_REL_64)/$(TARGET)

TARGET_DEB_32 = $(WINDOWS_BIN_DEB_32)/$(TARGET)
TARGET_DEB_64 = $(WINDOWS_BIN_DEB_64)/$(TARGET)

GCC_32_REL_FLAGS += -D WINDOWS -D WIN32
GCC_64_REL_FLAGS += -D WINDOWS -D WIN64

GPP_32_DEB_FLAGS += -D WINDOWS -D WIN32
GPP_64_DEB_FLAGS += -D WINDOWS -D WIN64

else ifeq ($(OS),Linux)
TARGET = fpc-qt.so

GCC_32_REL_FLAGS += -D UNIX
GCC_64_REL_FLAGS += -D UNIX

GPP_32_DEB_FLAGS += -D UNIX
GPP_64_DEB_FLAGS += -D UNIX

TARGET_REL_32 = $(OBJ_DIR)/$(TARGET)
TARGET_REL_64 = $(OBJ_DIR)/$(TARGET)

TARGET_DEB_32 = $(OBJ_DIR)/$(TARGET)
TARGET_DEB_64 = $(OBJ_DIR)/$(TARGET)
endif

SRCS = $(wildcard $(CPP_DIR)/*.cc)
OBJS = $(patsubst $(CPP_DIR)/%.cc,$(OBJ_DIR)/%.o,$(SRCS))

# ---------------------------------------------------------------------------
# start file
# ---------------------------------------------------------------------------
ifeq ($(OS),Linux)
all: clean presteps linux
else ifeq ($(OS),Windows_NT)
all: clean presteps windows
endif

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
linux: $(OBJS)
	echo "compile fpc-qt.so native under Linux:"
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $(OBJ_DIR)/$(TARGET) $^
	echo "compile test.app native under Linux:"
	$(FPC_LINUX) $(FPC_REL_FLAGS_LINUX) fpcqt.dpr

clean:
	rm -rf $(OBJ_DIR) $(BIN_DIR)

rebuild: clean all

presteps:
	mkdir -p ./obj
    
.PHONY: all clean rebuild linux
