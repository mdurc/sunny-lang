CC = gcc
CFLAGS = -Wall -Wextra -g
PROGRAM = compile.out

SRC_DIR = ./src
BUILD_DIR = ./build

SRC_LIST = $(SRC_DIR)/lexer.c $(SRC_DIR)/main.c $(SRC_DIR)/parser.c \
			$(SRC_DIR)/ast.c $(SRC_DIR)/error.c $(SRC_DIR)/symbol_table.c \
			$(SRC_DIR)/type_checker.c $(SRC_DIR)/exporter/json_exporter.c
OBJ_LIST = $(SRC_LIST:$(SRC_DIR)/%.c=$(BUILD_DIR)/%.o)

all: $(PROGRAM)

$(PROGRAM): $(OBJ_LIST)
	@$(CC) $(CFLAGS) $^ -o $@

$(BUILD_DIR)/%.o: $(SRC_DIR)/%.c
	@mkdir -p $(dir $@)
	@$(CC) $(CFLAGS) -c $< -o $@

clean:
	@rm -rf $(BUILD_DIR) $(PROGRAM)

.PHONY: all clean
