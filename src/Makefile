# ---------------------------------------------------------------------------
# File:   Makefile
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# get operating system, and architecture ...
# ---------------------------------------------------------------------------
OS   := $(shell uname -o)
ARCH := $(shell uname -m)

CPP_DIR = ./cpp
PAS_DIR = ./pas

PAS_OBJ_DIR = ./obj_pas
CPP_OBJ_DIR = ./obj_cpp

Qt5LIBS = -lQt5Core -lQt5Widgets

ifeq ($(OS),Msys)
WIN_PATH = 
FPC_PATH = E:\FPCdeluxe

WINDPWS_BIN_REL_32 = ./win32/Release
WINDPWS_BIN_REL_64 = ./win64/Release
#
WINDPWS_BIN_DEB_32 = ./win32/Debug
WINDPWS_BIN_DEB_64 = ./win64/Debug

GPP_WIN32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-g++.exe
GCC_WIN32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-gcc.exe

FPC_WIN32 = $(FPC_PATH)/fpc/bin/i386-win32/ppcross386.exe
FPC_WIN64 = $(FPC_PATH)/fpc/bin/i386-win32/ppcrossx64.exe

GPP_WIN64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-g++.exe
GCC_WIN64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-gcc.exe

QT_632_INC =\
    -I./cpp \
    -I$(WIN_PATH)/mingw64/include

FPC_REL_FLAGS =\
    -dRELEASE -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ -Fl./obj_cpp\
    -FE./obj_pas \
    -vn- -Mdelphi
#
FPC_DEB_FLAGS =\
    -dDEBUG   -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ -Fl./obj_cpp \
    -gl \
    -FE./obj_pas \
    -vn- -Mdelphi

FPC_REL_FLAGS_CROSS = -Tlinux $(FPC_REL_FLAGS)
FPC_DEB_FLAGS_CROSS = -Tlinux $(FPC_DEB_FLAGS)

else ifeq ($(OS),Linux)
GPP_64    = g++
INCLUDES +=\
    -I /usr/include/x86_64-linux-gnu/qt5/

FPC_LINUX = fpc
FPC_REL_FLAGS_LINUX =\
    -Fu./pas -Fu./ -Fl./obj_cpp \
    -gl \
    -FE./obj_pas \
    -vn- -Mdelphi
FPC_DEB_FLAGS_LINUX =\
    -Fu./pas -Fu./ -Fl./obj_cpp \
    -gl \
    -FE./obj_pas \
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

ifeq ($(OS),Msys)
WINDOWS_BIN_REL_32 = ./win32/Release
WINDOWS_BIN_REL_64 = ./win64/Release

WINDOWS_BIN_DEB_32 = ./win32/Debug
WINDOWS_BIN_DEB_64 = ./win64/Debug

TARGET = fpc-qt.dll

TARGET_REL_32 = $(WINDOWS_BIN_REL_32)/$(TARGET)
TARGET_REL_64 = $(WINDOWS_BIN_REL_64)/$(TARGET)

TARGET_DEB_32 = $(WINDOWS_BIN_DEB_32)/$(TARGET)
TARGET_DEB_64 = $(WINDOWS_BIN_DEB_64)/$(TARGET)

GCC_32_REL_FLAGS += -D WINDOWS -D WIN32
GCC_64_REL_FLAGS += -D WINDOWS -D WIN64

GPP_32_DEB_FLAGS += -D WINDOWS -D WIN32
GPP_64_DEB_FLAGS += -D WINDOWS -D WIN64

else ifeq ($(OS),Linux)
TARGET = $(CPP_OBJ_DIR)/fpc-qt.so

GCC_32_REL_FLAGS += -D UNIX
GCC_64_REL_FLAGS += -D UNIX

GPP_32_DEB_FLAGS += -D UNIX
GPP_64_DEB_FLAGS += -D UNIX

TARGET_REL_32 = $(TARGET)
TARGET_REL_64 = $(TARGET)

TARGET_DEB_32 = $(TARGET)
TARGET_DEB_64 = $(TARGET)
endif

SRCS = $(wildcard $(CPP_DIR)/*.cc)
OBJS = $(patsubst $(CPP_DIR)/%.cc,$(CPP_OBJ_DIR)/%.o,$(SRCS))

# ---------------------------------------------------------------------------
# start file
# ---------------------------------------------------------------------------
ifeq ($(OS),Linux)
all: common linux
else ifeq ($(OS),Msys)
all: common windows_dll windows_app linux_dll linux_app
endif

common: welcome clean presteps

ifeq ($(OS),Msys)
GCC_32 = $(GCC_WIN32)
GCC_64 = $(GCC_WIN64)
#
FPC_32 = $(FPC_WIN32)
FPC_64 = $(FPC_WIN64)
#
GPP_32 = $(GPP_WIN32)
GPP_64 = $(GPP_WIN64)
endif

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_64): $(OBJS)
	$(GPP_64) $(GPP_64_REL_FLAGS) aaa -o ./$@ $^ $(Qt5LIBS)

$(CPP_OBJ_DIR)/%.o: $(CPP_DIR)/%.cc
	@echo "compile: $< ..."
	@$(GPP_64) $(GPP_64_REL_FLAGS) -o ./$@ -c $<

# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Linux ...
# ---------------------------------------------------------------------------
ifeq ($(OS),Linux)
welcome:
	@echo "compile fpc-qt.so native under Linux:"

linux: $(OBJS)
	@echo "compile fpc-qt.so native under Linux:"
	@echo ""
	@$(GPP_64) $(GPP_64_REL_FLAGS) -o $(TARGET_REL_64) $^ $(Qt5LIBS)
	@strip $(TARGET_REL_64)
	@echo "compile test.app native under Linux:"
	@echo $(FPC_REL_FLAGS_LINUX)
	$(FPC_64) $(FPC_REL_FLAGS_LINUX) fpcqt.dpr

else ifeq ($(OS),Msys)
# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Windows ...
# ---------------------------------------------------------------------------
welcome:
	@echo "compile native files under Windows:"

windows_dll: $(OBJS)
	@echo "create 'fpc-qt.dll':"
	$(GPP_64) $(GPP_64_REL_FLAGS) -o $(TARGET_REL_64) $^ $(Qt5LIBS)
	strip $(TARGET_REL_64)
	upx   $(TARGET_REL_64)

windows_app: $(TARGET_REL_64)
	$(FPC_64) $(FPC_REL_FLAGS) -o$(WINDOWS_BIN_REL_64)/fpcqt.exe fpcqt.dpr
	upx $(WINDOWS_BIN_REL_64)/fpcqt.exe

# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Linux ...
# ---------------------------------------------------------------------------
linux_dll:
	$(FPC_64) $(FPC_REL_FLAGS_CROSS) -o$(WINDOWS_BIN_REL_64)/fpcso.so pas/fpcso.pas

linux_app:
	$(FPC_64) $(FPC_REL_FLAGS_CROSS) -o$(WINDOWS_BIN_REL_64)/fpcqt fpcqt.dpr

endif

clean:
	@echo "delete old temporary stuff..."
	@rm -rf $(PAS_OBJ_DIR)
	@rm -rf $(CPP_OBJ_DIR)
	@rm -rf link.res
	@rm -rf Linux32
	@rm -rf Linux64
	@rm -rf Win32
	@rm -rf Win64

rebuild: clean all

presteps:
	@echo "create new tempory directories..."
	@mkdir -p $(PAS_OBJ_DIR)
	@mkdir -p $(CPP_OBJ_DIR)
	@mkdir -p ./Win32/Debug
	@mkdir -p ./Win64/Debug
	@mkdir -p ./Win32/Release
	@mkdir -p ./Win64/Release

.PHONY: all clean rebuild linux
