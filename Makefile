PROJECT_NAME=cobol-data-app
COB=cobc
SRC=src/main.cob
BIN=bin/$(PROJECT_NAME)

# Detect GMP via pkg-config if available
GMP_CFLAGS:=$(shell pkg-config --cflags gmp 2>/dev/null)
GMP_LIBS:=$(shell pkg-config --libs gmp 2>/dev/null)

.PHONY: all build run clean

all: build

build:
	@mkdir -p bin
	$(COB) $(GMP_CFLAGS) -x -free -Wall -O2 -o $(BIN) $(SRC) $(GMP_LIBS)

run: build
	./$(BIN)

clean:
	rm -rf bin
