# ---------------------------------------------------------------------------
# File:   Makefile
# Author: (c) 2024 Jens Kallup - paule32
# All rights reserved
# ---------------------------------------------------------------------------
TARGET = fpcqt

# ---------------------------------------------------------------------------
# get operating system, and architecture ...
# ---------------------------------------------------------------------------
OS    := $(shell uname -o)
ARCH  := $(shell uname -m)

# ---------------------------------------------------------------------------
# the following variables/environment values must be set, before make is run:
# user/developer common section, for set individual path's and setting's ...
# ---------------------------------------------------------------------------
PascalCompiler := $(shell echo $$PascalCompiler)
RadConfig      := $(shell echo $$RadConfig)
FpcPath        := $(shell echo $$FPC_PATH)

# ---------------------------------------------------------------------------
# check, when these variables are "not set", then we don't compile from the
# RAD Studio (EMB Delphi Community Edition 12).
# If so, try to compile with Free Pascal Compiler (FPC) ...
# todo: FPC_PATH must be set !
# ---------------------------------------------------------------------------
ifeq ($strip $(PascalCompiler)),)
    PascalCompiler = FPC
endif
ifeq ($strip $(RadConfig)),)
    RadConfig = Release
endif
ifeq ($strip $(FpcPath)),)
    FPC_PATH = E:\FPCdeluxe
endif

ifeq ($(PascalCompiler),)
    PascalCompiler = FPC
endif
ifeq ($(RadConfig),)
    RadConfig = Release
endif
ifeq ($(FpcPath),)
    FPC_PATH = E:\FPCdeluxe
endif

# ---------------------------------------------------------------------------
# cross compiler make it possible to compile for Linux under Windows.
# The path is depend on the "FPCdeluxe" application.
# ---------------------------------------------------------------------------
FPC_PATH_Linux32 = $(FPC_PATH)/cross/bin/i386-linux
FPC_PATH_Linux64 = $(FPC_PATH)/cross/bin/x86_64-linux
#
FPC_PATH_WIN = $(FPC_PATH)/fpc/bin/i386-win32
#
FPC_Win_32   = $(FPC_PATH_WIN)/fpc
FPC_Win_64   = $(FPC_PATH_WIN)/ppcrossx64
#
FPC_LINUX_32 = $(FPC_PATH_WIN)/ppcross386
FPC_LINUX_64 = $(FPC_PATH_WIN)/ppcrossx64

ifeq ($(OS),Msys)
    ToolChain_Win32 = i686-w64-mingw32
    ToolChain_Win64 = x86_64-w64-mingw32
    #
    ToolChain_Linux32 = i386-linux-gnu
    ToolChain_Linux64 = x86_64-linux-gnu
endif

# ---------------------------------------------------------------------------
# supported target extension's ...
# ---------------------------------------------------------------------------
EXT_DLL = dll
EXT_EXE = exe
EXT_DSO = so

# ---------------------------------------------------------------------------
# temporary output directories ...
# ---------------------------------------------------------------------------
PAS_OBJ_DIR = ./obj_pas
CPP_OBJ_DIR = ./obj_cpp

CPP_DIR = ./cpp
PAS_DIR = ./pas

# ---------------------------------------------------------------------------
# adjust to your needs ...
# ---------------------------------------------------------------------------
ifeq ($(OS),Msys)
    ifeq ($(ARCH),x86_64)
        ifeq ($(RadConfig),Release)
            OUTPUT_PATH = ./win64/Release
            GCC_FLAGS  = -D WINDOWS -D RELEASE
            GCC_FLAGS += -D WIN32
            #
            GXX_FLAGS  = -D WINDOWS -D RELEASE
            GXX_FLAGS += -D WIN64
            #
        endif
        ifeq ($(RadConfig),Debug)
            OUTPUT_PATH = ./win64/Debug
            GCC_FLAGS  = -D WINDOWS -D DEBUG
            GCC_FLAGS += -D WIN32
            #
            GXX_FLAGS  = -D WINDOWS -D DEBUG
            GXX_FLAGS += -D WIN64
        endif
    endif
    ifneq ($(ARCH),x86_64)
        ifeq ($(RadConfig),Release)
            OUTPUT_PATH = ./win32/Release
            GCC_FLAGS  = -D WINDOWS -D RELEASE
            GCC_FLAGS += -D WIN32
            #
            GXX_FLAGS  = -D WINDOWS -D RELEASE
            GXX_FLAGS #= -D WIN64
            #
        endif
        ifeq ($(RadConfig),Debug)
            OUTPUT_PATH = ./win32/Debug
            GCC_FLAGS  = -D WINDOWS -D DEBUG
            GCC_FLAGS += -D WIN32
            #
            GXX_FLAGS  = -D WINDOWS -D DEBUG
            GXX_FLAGS #= -D WIN64
        endif
    endif
    DLL_TARGET = $(OUTPUT_PATH)/$(TARGET).$(EXT_DLL)
    EXE_TARGET = $(OUTPUT_PATH)/$(TARGET).$(EXT_EXE)
    DSO_TARGET = $(OUTPUT_PATH)/$(TARGET).$(EXT_DSO)
    #
    ToolChain_Linux32 = $(FPC_LINUX_32)
    ToolChain_Linux64 = $(FPC_LINUX_64)
    #
    ToolChain_Win32_Path = /mingw32/bin/$(ToolChain_Win32)
    ToolChain_Win64_Path = /mingw64/bin/$(ToolChain_Win64)
    #
    GCC_32 = $(ToolChain_Win32_Path)-gcc
    GXX_32 = $(ToolChain_Win32_Path)-g++
    #
    GCC_64 = $(ToolChain_Win64_Path)-gcc
    GXX_64 = $(ToolChain_Win64_Path)-g++
    #
    STRIP_Win32 = $(ToolChain_Win32_Path)-strip
    STRIP_Win64 = $(ToolChain_Win64_Path)-strip
    #
    STRIP_Linux32 = $(ToolChain_Linux32)-strip
    STRIP_Linux64 = $(ToolChain_Linux64)-strip
    #
    LD_LINUX_32 = $(FPC_PATH_Linux32)/i386-linux-gnu-ld.exe
    LD_LINUX_64 = $(FPC_PATH_Linux64)/x86_64-linux-gnu-ld.exe
    #
endif
ifeq ($(OS),Linux)
    ifeq ($(ARCH),x86_64)
        ifeq ($(RadConfig),Release)
            OUTPUT_PATH = ./Linux64/Release
        endif
        ifeq ($(RadConfig),Debug)
            OUTPUT_PATH = ./Linux64/Debug
        endif
    else
        ifeq ($(RadConfig),Release)
            OUTPUT_PATH = ./Linux32/Release
        endif
        ifeq ($(RadConfig),Debug)
            OUTPUT_PATH = ./Linux32/Debug
        endif
    endif
    TARGET = $(OUTPUT_PATH)/lib$(TARGET).so
endif

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

GCC_FLAGS += -O2 -Wno-write-strings
GCC_FLAGS += $(INCLUDES)
#
GXX_FLAGS += $(GCC_FLAGS) -fPIC -shared -std=c++2a
GXX_FLAGS += $(INCLUDES)

GCC_32_REL_FLAGS += -m32 $(GCC_FLAGS)
GCC_64_REL_FLAGS += -m64 $(GCC_FLAGS)

GCC_32_DEB_FLAGS += -m32 $(GCC_FLAGS) -g gdb
GCC_64_DEB_FLAGS += -m64 $(GCC_FLAGS) -g gdb
#
GPP_32_REL_FLAGS += -m32 $(GXX_FLAGS) -DFPC
GPP_64_REL_FLAGS += -m64 $(GXX_FLAGS) -DFPC

GPP_32_DEB_FLAGS += -m32 $(GXX_FLAGS) -g gdb
GPP_64_DEB_FLAGS += -m64 $(GXX_FLAGS) -g gdb
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
all: common windows_dll windows_app
#commit_win64 linux_so
endif

common: welcome clean presteps

# ---------------------------------------------------------------------------
# 64-Bit release DLL target ...
# ---------------------------------------------------------------------------
$(DLL_TARGET): $(OBJS)
	g++ $(GPP_64_REL_FLAGS) -o ./$@ $^ $(Qt5LIBS)

$(CPP_OBJ_DIR)/%.o: $(CPP_DIR)/%.cc
	@echo "compile: $< ..."
	g++ $(GPP_64_REL_FLAGS) -o ./$@ -c $<

# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Linux ...
# ---------------------------------------------------------------------------
ifeq ($(OS),Linux)
welcome:
	@echo "verwende: $(isFPC)"
	@echo "compile fpc-qt.so native under Linux:"

linux: $(OBJS)
	@echo "compile fpc-qt.so native under Linux:"
	@echo ""
	@$(GPP_64) $(GPP_64_REL_FLAGS) -o $(TARGET_REL_64) $^ $(Qt5LIBS)
	@strip $(TARGET_REL_64)
	@echo "compile test.app native under Linux:"
	@echo $(FPC_REL_FLAGS_LINUX)
	$(FPC_Win_64) $(FPC_REL_FLAGS_LINUX) fpcqt.dpr
endif
ifeq ($(OS),Msys)
# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Windows ...
# ---------------------------------------------------------------------------
welcome:
	@echo "verwende: $(isFPC)"
	@echo "Config:   $(RadConfig)"
	@echo "Compiler: $(PascalCompiler)"
	@echo "compile native files under Windows:"

windows_dll: $(OBJS)
	@echo "create 'fpcso.dll':"
	g++   $(GPP_64_REL_FLAGS) -o$(DLL_TARGET) $^ $(Qt5LIBS)
	strip $(DLL_TARGET)
	cp    $(DLL_TARGET) $(DLL_TARGET).bak
	upx   $(DLL_TARGET)

windows_app: $(TARGET_REL_64)
	$(FPC_Win_64) $(FPC_REL_FLAGS_WINDOWS) -o$(EXE_TARGET) fpcqt.dpr
	cp  $(EXE_TARGET) $(EXE_TARGET).bak
	upx $(EXE_TARGET)

# ---------------------------------------------------------------------------
# commit + push source code to github.com
# ---------------------------------------------------------------------------
commit_win64:

# ---------------------------------------------------------------------------
# 64-Bit release DLL target for Linux ...
# ---------------------------------------------------------------------------
linux_so:
	$(FPC_Win_64) -sh $(FPC_REL_FLAGS_LINUX) pas/fpcso.pas
	$(LD_UNIX64_FLAGS)
	$(STRIP) --discard-all --strip-debug $(TARGET_SO_64)

linux_app:
	$(FPC_Win_64) -sh $(FPC_REL_FLAGS_LINUX) -o$(LINUX_BIN_REL_64)/fpcqt fpcqt.dpr
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
	@rm -rf $(OUTPUT_PATH)
	@rm -rf link.res
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
	@mkdir -p $(OUTPUT_PATH)

.PHONY: all clean rebuild linux
