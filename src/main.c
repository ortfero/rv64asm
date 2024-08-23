#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "rv64asm.h"


#define HASH2(s) (s[0] * 31 + s[1])
#define HASH3(s) (s[0] * 961 + s[1] * 31 + s[2])
#define HASH4(s) (s[0] * 29791 + s[1] * 961 + s[2] * 31 + s[3])
#define EQUALS2(x, y) (x[0] == y[0] && x[1] == y[1])
#define EQUALS3(x, y) (x[0] == y[0] && x[1] == y[1] \
                        && x[2] == y[2])
#define EQUALS4(x, y) (x[0] == y[0] && x[1] == y[1] \
                        && x[2] == y[2] && x[3] == y[3]) 

static bool is_fragment_equals(struct fragment f, uint64_t h, char const* s) {
  if(f.h != h)
    return false;
  switch(f.n) {
    case 2:
      return EQUALS2(f.p, s);
    case 3:
      return EQUALS3(f.p, s);
    case 4:
      return EQUALS4(f.p, s);
    default:
      return false;
  }
}


static void init_module(struct module* module) {
  memset(module, 0, sizeof(struct module));
}


static void cleanup_module(struct module* module) {
  if(module->source) {
    free(module->source);
    module->source = NULL;
  }
  if(module->commands) {
    free(module->commands);
    module->commands= NULL;
  }
  if(module->symbols) {
    free(module->symbols);
    module->symbols = NULL;
  }
  if(module->codes) {
    free(module->codes);
    module->codes = NULL;
  }
}


struct directive_spec {
  enum directive_kind kind;
  char const* text;
  uint64_t hash;
  int arity;
  enum parameter_kind parameters[3];
};

static struct directive_spec const directive_specs[DIRECTIVES_COUNT] = {
  { DIRECTIVE_NONE, "", 0,  0, { PR_NONE, PR_NONE, PR_NONE } },
  { DIRECTIVE_ORG, "org", HASH3("org"),  1, { PR_IMM, PR_NONE, PR_NONE } }
};


enum instruction_type {
  INSTRUCTION_TYPE_NONE,
  INSTRUCTION_TYPE_R
};

struct instruction_spec {
  enum instruction_kind kind;
  char const* text;
  uint64_t hash;
  enum instruction_type type;
  uint8_t opcode;
  uint8_t funct3;
  uint8_t funct7;
  int arity;
  enum parameter_kind parameters[3];
};

static struct instruction_spec const instruction_specs[INSTRUCTIONS_COUNT] = {
  { INSTRUCTION_NONE, "", 0, INSTRUCTION_TYPE_NONE,  0x00, 0x0, 0x00, 0, {PR_NONE, PR_NONE, PR_NONE } },
  { INSTRUCTION_ADD, "add", HASH3("add"), INSTRUCTION_TYPE_R, 0x33, 0x0, 0x00, 3, { PR_RG, PR_RG, PR_RG} }
};


enum error_kind {
  ERROR_NONE,
  ERROR_UNKNOWN_OPTION,
  ERROR_NO_INPUT_FILE,
  ERROR_UNABLE_TO_OPEN_INPUT_FILE,
  ERROR_UNABLE_TO_OPEN_OUTPUT_FILE,
  ERROR_UNABLE_TO_READ_FROM_INPUT_FILE,
  ERROR_UNABLE_TO_WRITE_TO_OUTPUT_FILE,
  ERROR_NOT_ENOUGH_MEMORY,
  ERROR_UNEXPECTED_IDENTIFIER,
  ERROR_UNEXPECTED_NUMBER,
  ERROR_INVALID_SYNTAX,
  ERROR_EXPECTED_COMMA,
  ERROR_TOO_MANY_PARAMETERS,
  ERROR_EXPECTED_DIRECTIVE,
  ERROR_UNKNOWN_DIRECTIVE,
  ERROR_UNKNOWN_INSTRUCTION,
  ERROR_EXPECTED_INTEGER_NUMBER,
  ERROR_INVALID_LABEL,
  ERROR_TOO_MANY_SYMBOLS,
  ERROR_DUPLICATED_SYMBOL,
  ERROR_INVALID_HEX_NUMBER,
  ERROR_NO_CODE_TO_OUTPUT,
  ERROR_EXPECTED_REGISTER_NAME,
  ERROR_INTERNAL_INVALID_PR_KIND,
  ERROR_INTERNAL_INVALID_COMMAND,
  ERROR_INTERNAL_INVALID_DIRECTIVE,
  ERROR_INTERNAL_INVALID_INSTRUCTION_TYPE,
  ERROR_INTERNAL_MISMATCHED_CODE_SIZE,
  ERROR_COUNT
};

struct error {
  enum error_kind kind;
  size_t line;
};

inline struct error result(enum error_kind kind, int line) {
  struct error r = {kind, line};
  return r;
}

bool with_error(struct error* e, enum error_kind kind, int line) {
  e->kind = kind;
  e->line = line;
  return false;
}

static char const* error_messages[ERROR_COUNT] = {
  "That`s fine",
  "Unknown option",
  "No input file",
  "Unable to open input file",
  "Unable to open output file",
  "Unable to read from input file",
  "Unable to write to output file",
  "Not enough memory",
  "Unexpected identifier",
  "Unexpected number",
  "Invalid syntax",
  "Expected comma",
  "Too many parameters",
  "Expected directive",
  "Unknown directive",
  "Unknown instruction",
  "Expected integer number",
  "Invalid label",
  "Too many symbols",
  "Duplicated symbol",
  "Invalid hex number",
  "No code to output",
  "Expected register name",
  "Internal (invalid parameter kind)",
  "Internal (invalid command)",
  "Internal (invalid directive)",
  "Internal (invalid instruction type)",
  "Internal (mismatched code size)"
};


void print_error(struct error e) {
  if(e.line == 0) {
    fprintf(stderr, "Error: %s\n",
            error_messages[e.kind]);
  } else {
    fprintf(stderr, "Error [%u]: %s\n",
          (unsigned)e.line, error_messages[e.kind]);
  }  
}


void print_version() {
  fprintf(stdout, "rv64asm %d.%d\n",
          RVASM_MAJOR_VERSION, RVASM_MINOR_VERSION);
}


void print_help() {
  fprintf(stdout, "rv64asm -o <bin-file> <asm-file>\n");
}


enum token_kind {
  TOKEN_NONE, TOKEN_END, TOKEN_LINE, 
  TOKEN_NAME, TOKEN_INTEGER,
  TOKEN_POINT
};


struct token {
  enum token_kind kind;
  union token_data {
    struct fragment name;
    int64_t integer;
  } data;
};


struct scanner {
  char const* c;
  size_t line;
  struct token token;
};

void init_scanner(struct scanner* scanner, char const* source) {
  scanner->c = source;
  scanner->line = 1;
  memset(&scanner->token, 0, sizeof(struct token));
}


char peek(struct scanner* scanner) {
  char const* c = scanner->c;
  while(*c == ' ' || *c == '\t' || *c == '\r')
    ++c;
  scanner->c = c;
  return *c;
}


bool scan_name(struct scanner* scanner, struct error* e) {
  char const* c = scanner->c;
  uint64_t h = *c;
  ++c;
  for(;;)
    switch(*c) {
      case 'A': case 'B': case 'C': case 'D': case 'E':
      case 'F': case 'G': case 'H': case 'I': case 'J':
      case 'K': case 'L': case 'M': case 'N': case 'O':
      case 'P': case 'Q': case 'R': case 'S': case 'T':
      case 'U': case 'V': case 'W': case 'X': case 'Y':
      case 'Z': case 'a': case 'b': case 'c': case 'd':
      case 'e': case 'f': case 'g': case 'h': case 'i':
      case 'j': case 'k': case 'l': case 'm': case 'n':
      case 'o': case 'p': case 'q': case 'r': case 's':
      case 't': case 'u': case 'v': case 'w': case 'x':
      case 'y': case 'z': case '_': case '0': case '1':
      case '2': case '3': case '4': case '5': case '6':
      case '7': case '8': case '9':
        h *= 31;
        h += *c;
        ++c;
        continue;
      default:
        scanner->token.kind = TOKEN_NAME;
        scanner->token.data.name.p = scanner->c;
        scanner->token.data.name.n = (size_t)(c - scanner->c);
        scanner->token.data.name.h = h;        
        scanner->c = c;
        return true;
    }
}


bool scan_hex(struct scanner* scanner, struct error* e) {
  scanner->c += 2;
  char const* c = scanner->c;
  int64_t n = 0;
  for(;;)
    switch(*c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        n *= 16;
        n += *c - '0';
        ++c;
        continue;
      case 'a': case 'b': case 'c':
      case 'd': case 'e': case 'f':
        n *= 16;
        n += *c - 'a' + 10;
        ++c;
        continue;
      case 'A': case 'B': case 'C':
      case 'D': case 'E': case 'F':
        n *= 16;
        n += *c - 'A' + 10;
        ++c;
        continue;
      default:
        if(scanner->c == c)
          return with_error(e, ERROR_INVALID_HEX_NUMBER, scanner->line);
        scanner->c = c;
        scanner->token.kind = TOKEN_INTEGER;
        scanner->token.data.integer = n;
        return true;
    }
}


bool scan_integer(struct scanner* scanner, struct error* e) {
  char const* c = scanner->c;
  if(c[0] == '0' && c[1] == 'x')
    return scan_hex(scanner, e);
  int64_t n = *c - '0';
  ++c;
  for(;;) 
    switch(*c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        n *= 10;
        n += *c - '0';
        ++c;
        continue;
      default:
        scanner->c = c;
        scanner->token.kind = TOKEN_INTEGER;
        scanner->token.data.integer = n;
        return true;
    }
}


bool scan(struct scanner* scanner, struct error* e) {
  switch(peek(scanner)) {
    case 'A': case 'B': case 'C': case 'D': case 'E':
    case 'F': case 'G': case 'H': case 'I': case 'J':
    case 'K': case 'L': case 'M': case 'N': case 'O':
    case 'P': case 'Q': case 'R': case 'S': case 'T':
    case 'U': case 'V': case 'W': case 'X': case 'Y':
    case 'Z': case 'a': case 'b': case 'c': case 'd':
    case 'e': case 'f': case 'g': case 'h': case 'i':
    case 'j': case 'k': case 'l': case 'm': case 'n':
    case 'o': case 'p': case 'q': case 'r': case 's':
    case 't': case 'u': case 'v': case 'w': case 'x':
    case 'y': case 'z': case '_':
      return scan_name(scanner, e);
    case '0': case '1': case '2': case '3': case '4':
    case '5': case '6': case '7': case '8': case '9':
      return scan_integer(scanner, e);
    case '\n':
      ++scanner->c;
      scanner->token.kind = TOKEN_LINE;
      return true;
    case '.':
      scanner->token.kind = TOKEN_POINT;
      ++scanner->c;
      return true;
    case '\0':
      scanner->token.kind = TOKEN_END;
      return true;
    default:
      return with_error(e, ERROR_INVALID_SYNTAX, scanner->line);
  }
}


bool parse_label(struct scanner* scanner, struct command_label* label, struct error* e) {
  switch(scanner->token.kind) {
    case TOKEN_NAME:
      label->kind = COMMAND_LABEL_GLOBAL;
      label->data.global = scanner->token.data.name;
      return true;
    case TOKEN_INTEGER:
      label->kind = COMMAND_LABEL_LOCAL;
      label->data.local = scanner->token.data.integer;
      return true;
    default:
      return with_error(e, ERROR_INVALID_LABEL, scanner->line);
  }
}


bool parse_register(struct scanner* scanner, int* r, struct error* e) {
  if(!scan(scanner, e))
    return false;
  if(scanner->token.kind != TOKEN_NAME)
    return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
  char p0 = scanner->token.data.name.p[0];
  char p1, p2, p3;
  switch(scanner->token.data.name.n) {
    case 2:
      p1 = scanner->token.data.name.p[1];
      switch(p0) {
        case 's':
          if(p1 >= '2' && p1 <= '9') return (*r = p1 - '2' + 18);
          if(p1 == '0' || p1 == '1') return (*r = p1 - '0' + 8), true;
          if(p1 == 'p') return (*r = 2), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        case 't':
          if(p1 >= '3' && p1 <= '6') return (*r = p1 - '3' + 28), true;
          if(p1 >= '0' && p1 <= '2') return (*r = p1 - '0' + 5), true;
          if(p1 == 'p') return (*r = 4), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        case 'a':
          if(p1 >= '0' && p1 <= '7') return (*r = p1 - '0' + 10), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        case 'r':
          if(p1 == 'a') return (*r = 1), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        case 'f':
          if(p1 == 'p') return (*r = 8), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        case 'g':
          if(p1 == 'p') return (*r = 3), true;
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
        default:
          return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
      }
    case 3:
      p1 = scanner->token.data.name.p[1];
      p2 = scanner->token.data.name.p[2];
      if(p0 == 's' && p1 == '1' && (p2 == '0' || p2 == '1'))
        return (*r = p2 - '0' + 26), true; 
      return with_error(e,ERROR_EXPECTED_REGISTER_NAME, scanner->line);
    case 4:
      p1 = scanner->token.data.name.p[1];
      p2 = scanner->token.data.name.p[2];
      p3 = scanner->token.data.name.p[3];
      if(p0 == 'z' && p1 == 'e' && p2 == 'r' && p3 == 'o')
        return (*r = 0), true;
      return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
    default:
      return with_error(e, ERROR_EXPECTED_REGISTER_NAME, scanner->line);
  }
}


bool parse_parameter(
  struct scanner* scanner,
  enum parameter_kind kind,
  struct parameter* parameter,
  struct error* e
) {
  parameter->kind = kind;
  switch(kind) {
    case PR_IMM:
      if(!scan(scanner, e))
        return false;
      if(scanner->token.kind != TOKEN_INTEGER)
        return with_error(e, ERROR_EXPECTED_INTEGER_NUMBER, scanner->line);
      parameter->data.imm = scanner->token.data.integer;
      return true;
    case PR_RG:
      return parse_register(scanner, &parameter->data.r, e);
    default:
      return with_error(e, ERROR_INTERNAL_INVALID_PR_KIND, scanner->line);
  }
}


bool parse_parameters(
  struct scanner* scanner,
  int arity,
  enum parameter_kind const* pp_kinds,
  struct command* command,
  struct error* e
) {
  if(arity > 0) {
    bool parsed = parse_parameter(scanner, pp_kinds[0], &command->parameters[0], e);
    if(!parsed)
      return false;
    for(int i = 1; i != arity; i++) {
      if(peek(scanner) != ',')
        return with_error(e, ERROR_EXPECTED_COMMA, scanner->line);
      ++scanner->c;
      parsed = parse_parameter(scanner, pp_kinds[i], &command->parameters[i], e);
      if(!parsed)
        return false;
    }
  }
  if(peek(scanner) != '\n')
    return with_error(e, ERROR_TOO_MANY_PARAMETERS, scanner->line);
  ++scanner->c;
  ++scanner->line;
  return true;
}


bool parse_directive(struct scanner* scanner, struct command* command, struct error* e) {
  ++scanner->c;
  if(!scan(scanner, e))
    return false;
  if(scanner->token.kind != TOKEN_NAME)
    return with_error(e, ERROR_EXPECTED_DIRECTIVE, scanner->line);
  for(size_t i = 0; i != DIRECTIVES_COUNT; i++) {
    struct directive_spec const* directive = directive_specs + i;
    if(!is_fragment_equals(scanner->token.data.name, directive->hash, directive->text))
      continue;
    command->kind = COMMAND_DIRECTIVE;
    command->data.directive = i;
    if(!parse_parameters(scanner, directive->arity, directive->parameters, command, e))
      return false;
    return true;
  }
  return with_error(e, ERROR_UNKNOWN_DIRECTIVE, scanner->line);
}


bool parse_instruction(struct scanner* scanner, struct command* command, struct error* e) {
  for(size_t i = 0; i != INSTRUCTIONS_COUNT; i++) {
    struct instruction_spec const* instruction = instruction_specs + i;
    if(!is_fragment_equals(scanner->token.data.name, instruction->hash, instruction->text))
      continue;
    command->kind = COMMAND_INSTRUCTION;
    command->data.instruction = i;
    if(!parse_parameters(scanner, instruction->arity, instruction->parameters, command, e))
      return false;
    return true; 
  }
  return with_error(e, ERROR_UNKNOWN_INSTRUCTION, scanner->line);
}


bool parse(struct module* module, struct error* e) {
  int lines_capacity = module->source_size / 4 + 4;
  module->commands= malloc(sizeof(struct command) * lines_capacity);
  if(!module->commands)
    return with_error(e, ERROR_NOT_ENOUGH_MEMORY, 0);
  memset(module->commands, 0, sizeof(struct command) * lines_capacity);
  struct scanner scanner;
  init_scanner(&scanner, module->source);
  while(scanner.token.kind != TOKEN_END) {
    struct command* command = module->commands + scanner.line - 1;
    command->line = scanner.line;
    if(!scan(&scanner, e))
      return false;
    if(peek(&scanner) == ':') {
      if(!parse_label(&scanner, &command->label, e))
        return false;
      if(!scan(&scanner, e))
        return false;
    }
    switch(scanner.token.kind) {
      case TOKEN_END:
        command->kind = COMMAND_NONE;
        continue;
      case TOKEN_LINE:
        command->kind = COMMAND_NONE;
        ++scanner.line;
        continue;
      case TOKEN_POINT:
        if(!parse_directive(&scanner, command, e))
          return false;
        continue;
      case TOKEN_NAME:
        if(!parse_instruction(&scanner, command, e))
          return false;
        continue;
      case TOKEN_INTEGER:
        return with_error(e, ERROR_UNEXPECTED_NUMBER, scanner.line);
      default:
        return with_error(e, ERROR_INVALID_SYNTAX, scanner.line);
    }
  }
  module->commands_count = scanner.line;
  return true;
}


struct symbol* find_symbol(
  struct module* module,
  struct fragment name
) {
  for(struct symbol* it = module->symbols;
      it != module->symbols + module->symbols_count;
      it++) {
    if(is_fragment_equals(it->name, name.h, name.p))
      return it;      
  }
  return NULL;
}


bool add_symbol(
  struct module* module,
  struct fragment name,
  int64_t value,
  size_t line,
  struct error* e
) {
  if(module->symbols_count == module->commands_count)
    return with_error(e, ERROR_TOO_MANY_SYMBOLS, line);
  if(find_symbol(module, name))
    return with_error(e, ERROR_DUPLICATED_SYMBOL, line);
  struct symbol* new_symbol = module->symbols + module->symbols_count;
  new_symbol->name = name;
  new_symbol->value = value;
  module->symbols_count++;
  return true;
}


bool process_symbols(struct module* module, struct error* e) {
  module->symbols = malloc(sizeof(struct symbol) * module->commands_count);
  if(!module->symbols)
    return with_error(e, ERROR_NOT_ENOUGH_MEMORY, 0);
  uint64_t offset = 0;
  for(struct command* c = module->commands;
      c != module->commands + module->commands_count;
      c++) {
    c->offset = offset;
    if(c->label.kind == COMMAND_LABEL_GLOBAL) {
      if(!add_symbol(module, c->label.data.global, offset, c->line, e))
        return false;
    }
    switch(c->kind) {
      case COMMAND_NONE:
        continue;
      case COMMAND_INSTRUCTION:
        offset += 4;
        continue;
      case COMMAND_DIRECTIVE:
        switch(c->data.directive) {
          case DIRECTIVE_ORG:
            offset += c->parameters[0].data.imm;
            module->org = c->parameters[0].data.imm;
            continue;
          default:
            return with_error(e, ERROR_INTERNAL_INVALID_DIRECTIVE, c->line);
        }
      default:
        return with_error(e, ERROR_INTERNAL_INVALID_COMMAND, c->line);
    }
  }
  module->code_size = offset - module->org;
  if(module->code_size == 0)
    return with_error(e, ERROR_NO_CODE_TO_OUTPUT, 0);
  return true;
}


bool encode_instruction(
  enum instruction_kind instruction,
  struct parameter const* pp,
  size_t line,
  uint8_t* codes,
  size_t* code_size,
  struct error* e
) {
  struct instruction_spec const* spec = instruction_specs + instruction;
  uint32_t code;
  switch(spec->type) {
    case INSTRUCTION_TYPE_R:
      code = (uint32_t)spec->opcode
        | ((uint32_t)pp[0].data.r) << 7
        | ((uint32_t)spec->funct3) << 12
        | ((uint32_t)pp[1].data.r) << 15
        | ((uint32_t)pp[2].data.r) << 20
        | ((uint32_t)spec->funct7) << 25;
      break;
    default:
      return with_error(e, ERROR_INTERNAL_INVALID_INSTRUCTION_TYPE, line); 
  }
  codes[0] = code & 0xFF;
  codes[1] = (code & 0xFF00) >> 8;
  codes[2] = (code & 0xFF0000) >> 16;
  codes[3] = (code & 0xFF000000) >> 24;
  *code_size += 4;
  return true;
}


bool encode(struct module* module, struct error* e) {
  module->codes = malloc(sizeof(uint8_t) * module->code_size);
  if(!module->codes)
    return with_error(e, ERROR_NOT_ENOUGH_MEMORY, 0);
  size_t code_size = 0;
  for(struct command* c = module->commands;
      c != module->commands + module->commands_count;
      ++c) {
    switch(c->kind) {
      case COMMAND_NONE:
        continue;
      case COMMAND_DIRECTIVE:
        continue;
      case COMMAND_INSTRUCTION:
        if(!encode_instruction(c->data.instruction, c->parameters, c->line, module->codes + code_size, &code_size, e))
          return false;
        continue;
      default:
        return with_error(e, ERROR_INTERNAL_INVALID_COMMAND, c->line);
    }      
  }
  if(code_size != module->code_size)
    return with_error(e, ERROR_INTERNAL_MISMATCHED_CODE_SIZE, 0);
  return true;
}



bool assemble(struct module* module, struct error* e) {
  if(!parse(module, e))
    return false;
  if(!process_symbols(module, e))
    return false;
  if(!encode(module, e))
    return false;
  return true;
}


struct command_line {
  char const* input_path;
  char const* output_path;
};


bool parse_command_line(
  int argc,
  char** argv,
  struct command_line* options,
  struct error* e
) {
  if(strcmp(argv[1], "--version") == 0)
    return print_version(), true;
  if(strcmp(argv[1], "--help") == 0)
    return print_help(), true;
  if(strcmp(argv[1], "-o") != 0)
    return with_error(e, ERROR_UNKNOWN_OPTION, 0);
  if(argc != 4)
    return with_error(e, ERROR_NO_INPUT_FILE, 0);
  options->input_path = argv[3];
  options->output_path = argv[2];
  return true;
}


bool read_input(
  char const* input_path,
  char** source,
  size_t* source_size,
  struct error* e
) {
  FILE* input = fopen(input_path, "rb");
  if(!input)
    goto unable_to_open_input;
  fseeko64(input, 0, SEEK_END);
  int64_t input_size = ftello64(input);
  if(input_size < 0)
    goto unable_to_read_input;
  *source_size = (size_t)(input_size);
  *source = malloc(*source_size + 1);
  if(!*source)
    goto not_enough_memory;
  fseeko64(input, 0, SEEK_SET);
  size_t input_read = fread(*source, sizeof(char), *source_size, input);
  if(input_read != *source_size)
    goto unable_to_read_input;
  (*source)[*source_size] = '\0';
  fclose(input);
  return true;
unable_to_open_input:
  e->kind = ERROR_UNABLE_TO_OPEN_INPUT_FILE;
  goto failed;
unable_to_read_input:
  e->kind = ERROR_UNABLE_TO_READ_FROM_INPUT_FILE;
  goto failed;
not_enough_memory:
  e->kind = ERROR_NOT_ENOUGH_MEMORY;
  goto failed;
failed:
  if(input) fclose(input);
  return false;
}


bool write_output(
  char const* output_path,
  uint8_t const* codes,
  size_t code_size,
  struct error* e
) {
  FILE* output = fopen(output_path, "wb+");
  if(!output) goto unable_to_open_output;
  size_t written = fwrite(codes, sizeof(uint8_t), code_size, output);
  if(written != code_size)
    goto unable_to_write_output;
  fclose(output);
  return true;
unable_to_open_output:
  e->kind = ERROR_UNABLE_TO_OPEN_OUTPUT_FILE;
  goto failed;
unable_to_write_output:
  e->kind = ERROR_UNABLE_TO_WRITE_TO_OUTPUT_FILE;
  goto failed;
failed:
  if(output) fclose(output);
  return false;
}


int main(int argc, char** argv) {
  struct module module;
  init_module(&module);
  struct command_line command_line;
  struct error error = {ERROR_NONE, 0};
  if(argc < 2)
    return 0;
  if(!parse_command_line(argc, argv, &command_line, &error))
    goto failed;
  if(!command_line.input_path)
    return 255;
  if(!read_input(command_line.input_path, &module.source, &module.source_size, &error))
    goto failed;
  if(!assemble(&module, &error))
    goto failed;
  if(!write_output(command_line.output_path, module.codes, module.code_size, &error))
    goto failed;
  cleanup_module(&module);
  return 0;
failed:
  print_error(error);
  cleanup_module(&module);
  return 255;
}

void test() {
  
}
