.RECIPEPREFIX = >


all: src/main.c
> $(shell mkdir build)
> $(CC) src/main.c -o build/rv64asm -g -O0

