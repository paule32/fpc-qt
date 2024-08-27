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

# ---------------------------------------------------------------------------
# user/developer common section, for set individual path's and setting's ...
# ---------------------------------------------------------------------------
FPC_PATH = E:\FPCdeluxe
WIN_PATH = 
#
TARGET   = fpcso

# ---------------------------------------------------------------------------
# adjust to your needs ...
# ---------------------------------------------------------------------------
WINDOWS_BIN_REL_32 = ./win32/Release
WINDOWS_BIN_REL_64 = ./win64/Release
#
WINDOWS_BIN_DEB_32 = ./win32/Debug
WINDOWS_BIN_DEB_64 = ./win64/Debug

LINUX_BIN_REL_64 = ./Linux64/Release
LINUX_BIN_DEB_64 = ./Linux64/Debug

LINUX_BIN_REL_32 = ./Linux32/Release
LINUX_BIN_DEB_32 = ./Linux32/Debug

TARGET_REL_WIN32 = $(WINDOWS_BIN_REL_32)/$(TARGET).dll
TARGET_REL_WIN64 = $(WINDOWS_BIN_REL_64)/$(TARGET).dll

TARGET_DEB_WIN32 = $(WINDOWS_BIN_DEB_32)/$(TARGET).dll
TARGET_DEB_WIN64 = $(WINDOWS_BIN_DEB_64)/$(TARGET).dll

GPP_WIN32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-g++.exe
GCC_WIN32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-gcc.exe

GPP_WIN64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-g++.exe
GCC_WIN64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-gcc.exe

GCC_DEL32 = $(WIN_PATH)/mingw32/bin/i686-w64-mingw32-strip.exe
GCC_DEL64 = $(WIN_PATH)/mingw64/bin/x86_64-w64-mingw32-strip.exe

PATH_CROSS = $(FPC_PATH)/cross/bin
PATH_WIN   = $(FPC_PATH)/fpc/bin/i386-win32
#
PATH_LINUX_32 = $(PATH_CROSS)/i386-linux
PATH_LINUX_64 = $(PATH_CROSS)/x86_64-linux

FPC_WIN32 = $(FPC_PATH)/fpc/bin/i386-win32/ppcross386.exe
FPC_WIN64 = $(FPC_PATH)/fpc/bin/i386-win32/ppcrossx64.exe

FPC_DEL32 = $(PATH_CROSS)/bin/i386-linux/linux/i386-linux-gnu-strip.exe
FPC_DEL64 = $(PATH_CROSS)/bin/x86_64-linux/x86_64-w64-mingw32-strip.exe

LD_UNIX32 = $(PATH_CROSS)/i386-linux/i386-linux-gnu-ld.exe
LD_UNIX64 = $(PATH_CROSS)/x86_64-linux\x86_64-linux-gnu-ld.exe

EXT_DLL = dll
EXT_EXE = exe
EXT_SO  = so

TARGET_EXE_32 = $(WINDOWS_BIN_REL_32)/$(TARGET).$(EXT_EXE)
TARGET_EXE_64 = $(WINDOWS_BIN_REL_64)/$(TARGET).$(EXT_EXE)
#
TARGET_DLL_32 = $(TARGET)_32.$(EXT_DLL)
TARGET_DLL_64 = $(TARGET)_64.$(EXT_DLL)
#
TARGET_SO_32  = $(TARGET).$(EXT_SO)
TARGET_SO_64  = $(LINUX_BIN_REL_64)/$(TARGET).$(EXT_SO)

TARGET_DLL_REL_32 = $(WINDOWS_BIN_REL_32)/$(TARGET_DLL)
TARGET_DLL_REL_64 = $(WINDOWS_BIN_REL_64)/$(TARGET_DLL)

TARGET_DLL_DEB_32 = $(WINDOWS_BIN_DEB_32)/$(TARGET_DLL)
TARGET_DLL_DEB_64 = $(WINDOWS_BIN_DEB_64)/$(TARGET_DLL)

CPP_DIR = ./cpp
PAS_DIR = ./pas

# ---------------------------------------------------------------------------
# temporary output directories ...
# ---------------------------------------------------------------------------
PAS_OBJ_DIR = ./obj_pas
CPP_OBJ_DIR = ./obj_cpp

Qt5LIBS = -lQt5Core -lQt5Widgets

# ---------------------------------------------------------------------------
# Msys = msys2 MinGW (Windows) ...
# ---------------------------------------------------------------------------
ifeq ($(OS),Msys)
GCC_32_REL_FLAGS += -D WINDOWS -D WIN32
GCC_64_REL_FLAGS += -D WINDOWS -D WIN64

GPP_32_DEB_FLAGS += -D WINDOWS -D WIN32
GPP_64_DEB_FLAGS += -D WINDOWS -D WIN64

QT_632_INC =\
    -I./cpp \
    -I$(WIN_PATH)/mingw64/include

FPC_FLAGS_WINDOWS =\
    -Twin64 -FD$(FPC_PATH)/fpc/bin/i386-win32 \
    -Fu$(FPC_PATH)/fpc/units/x86_64-win64 \
    -Fu./pas -Fu./ -Fl./obj_cpp \
    -FE./obj_pas \
    -vn- -Mdelphi

FPC_REL_FLAGS_WINDOWS = $(FPC_FLAGS_WINDOWS) -dRELEASE
FPC_DEB_FLAGS_WINDOWS = $(FPC_FLAGS_WINDOWS) -dDEBUG

FPC_FLAGS_LINUX =\
    -Tlinux -FD$(FPC_PATH)/cross/bin/x86_64-linux \
    -Fu$(FPC_PATH)/fpc/units/x86_64-linux \
    -Fl$(FPC_PATH)/cross/lib/x86_64-linux \
    -Fu./pas -Fu./ -Fl./Linux64/Release \
    -FE./obj_pas \
    -vn- -Mdelphi

FPC_REL_FLAGS_LINUX = $(FPC_FLAGS_LINUX) -dRELEASE
FPC_DEB_FLAGS_LINUX = $(FPC_FLAGS_LINUX) -dDEBUG

LD_UNIX64_FLAGS =\
    $(LD_UNIX64) -b elf64-x86-64 -m elf_x86_64 \
    -init FPC_SHARED_LIB_START   \
    -fini FPC_LIB_EXIT           \
    -soname $(TARGET_SO) -shared \
    -L. -o  $(TARGET_SO_64) \
    -T link.win64.ld \
    $(PAS_OBJ_DIR)/fpcso.o
endif
ifeq ($(OS),Linux)
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

CC_FLAGS  = -O2 -Wno-write-strings
CC_FLAGS += $(INCLUDES)
#
XX_FLAGS  = $(CC_FLAGS) -fPIC -shared -std=c++2a
XX_FLAGS += $(INCLUDES)

GCC_32_REL_FLAGS += -m32 $(CC_FLAGS) -DRELEASE
GCC_64_REL_FLAGS += -m64 $(CC_FLAGS) -DRELEASE

GCC_32_DEB_FLAGS += -m32 $(CC_FLAGS) -DDEBUG -g gdb
GCC_64_DEB_FLAGS += -m64 $(CC_FLAGS) -DDEBUG -g gdb
#
GPP_32_REL_FLAGS += -m32 $(XX_FLAGS) -DRELEASE
GPP_64_REL_FLAGS += -m64 $(XX_FLAGS) -DRELEASE

GPP_32_DEB_FLAGS += -m32 $(XX_FLAGS) -DDEBUG -g gdb
GPP_64_DEB_FLAGS += -m64 $(XX_FLAGS) -DDEBUG -g gdb
#

ifeq ($(OS),Linux)
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
endif
ifeq ($(OS),Msys)
all: common windows_dll windows_app linux_so
endif

common: welcome clean presteps

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(TARGET_REL_64): $(OBJS)
	$(GPP_WIN64) $(GPP_64_REL_FLAGS) -o ./$@ $^ $(Qt5LIBS)

$(CPP_OBJ_DIR)/%.o: $(CPP_DIR)/%.cc
	@echo "compile: $< ..."
	@$(GPP_WIN64) $(GPP_64_REL_FLAGS) -o ./$@ -c $<

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
endif
ifeq ($(OS),Msys)
# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Windows ...
# ---------------------------------------------------------------------------
welcome:
	@echo "compile native files under Windows:"

windows_dll: $(OBJS)
	@echo "create 'fpcso.dll':"
	$(GPP_WIN64) $(GPP_64_REL_FLAGS) -o$(TARGET_REL_WIN64) $^ $(Qt5LIBS)
	strip $(TARGET_REL_WIN64)
	upx   $(TARGET_REL_WIN64)

windows_app: $(TARGET_REL_64)
	$(FPC_WIN64) $(FPC_REL_FLAGS_WINDOWS) -o$(TARGET_EXE_64) fpcqt.dpr
	upx $(TARGET_EXE_64)

# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Linux ...
# ---------------------------------------------------------------------------
linux_so:
	$(FPC_WIN64) -sh $(FPC_REL_FLAGS_LINUX) pas/fpcso.pas
	$(LD_UNIX64_FLAGS)
	$(PATH_LINUX_64)/x86_64-linux-gnu-strip.exe --discard-all --strip-debug \
	$(TARGET_SO_64)

linux_app:
	$(FPC_WIN64) -sh $(FPC_REL_FLAGS_LINUX) -o$(LINUX_BIN_REL_64)/fpcqt fpcqt.dpr
	$(PATH_LINUX_64)/x86_64-linux-gnu-ld.exe -b elf64-x86-64 -m elf_x86_64 \
	--dynamic-linker=./lib64/ld-linux-x86-64.so.2 -s \
	-L. -o ./Linux64/Release/fpcqt    \
	-T ./link.linux64.ld -e _start    \
	$(LINUX_BIN_REL_64)/fpcqt.o       \
	$(LINUX_BIN_REL_64)/appsettings.o \
	$(LINUX_BIN_REL_64)/misc.o        \
	$(LINUX_BIN_REL_64)/QCharClass.o

endif

# ---------------------------------------------------------------------------
# delete old stuff ...
# ---------------------------------------------------------------------------
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

# ---------------------------------------------------------------------------
# create temporary directories ...
# ---------------------------------------------------------------------------
presteps:
	@echo "create new tempory directories..."
	@mkdir -p $(PAS_OBJ_DIR)
	@mkdir -p $(CPP_OBJ_DIR)
	@mkdir -p $(WINDOWS_BIN_DEB_32)
	@mkdir -p $(WINDOWS_BIN_REL_32)
	@mkdir -p $(WINDOWS_BIN_REL_64)
	@mkdir -p $(WINDOWS_BIN_DEB_64)
	@mkdir -p $(LINUX_BIN_DEB_32)
	@mkdir -p $(LINUX_BIN_REL_64)

.PHONY: all clean rebuild linux
