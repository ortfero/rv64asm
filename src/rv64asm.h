#ifndef _RV64ASM_H_
#define _RV64ASM_H_

#include <stdbool.h>
#include <stdint.h>


#define RVASM_MAJOR_VERSION 0
#define RVASM_MINOR_VERSION 1
#define MAX_ARITY 3

struct fragment {
  char const* p;
  size_t n;
  uint64_t h;
};

static bool is_fragment_equals(struct fragment, uint64_t, char const*);


enum parameter_kind {
  PR_NONE, PR_IMM, PR_RG
};


struct parameter {
  enum parameter_kind kind;
  union parameter_data {
    int64_t imm;
    int r;
  } data;
};


enum command_kind {
  COMMAND_NONE, COMMAND_DIRECTIVE, COMMAND_INSTRUCTION
};


enum directive_kind {
  DIRECTIVE_NONE, DIRECTIVE_ORG,
  DIRECTIVES_COUNT
};


enum instruction_kind {
  INSTRUCTION_NONE,
  INSTRUCTION_ADD,
  INSTRUCTIONS_COUNT
};


enum command_label_kind {
  COMMAND_LABEL_NO, COMMAND_LABEL_LOCAL, COMMAND_LABEL_GLOBAL
};


struct command_label {
  enum command_label_kind kind;
  union command_label_data {
    uint64_t local;
    struct fragment global;
  } data;
};


struct command {
  size_t line;
  uint64_t offset;
  struct command_label label;
  enum command_kind kind;
  union command_data {
    enum directive_kind directive;
    enum instruction_kind instruction;
  } data;
  struct parameter parameters[MAX_ARITY]; 
};


struct symbol {
  struct fragment name;
  int64_t value;  
};


struct module {
  char* source;
  size_t source_size;
  struct command* commands;
  size_t commands_count;
  struct symbol* symbols;
  size_t symbols_count;
  uint8_t* codes;
  uint64_t org;
  size_t code_size;
};

static void init_module(struct module*);
static void cleanup_module(struct module*);

#endif
