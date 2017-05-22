/*
 * Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include "gbldefs.h"
#include "error.h"
#include "ll_structure.h"
#include "ll_write.h"
#include "global.h"
#include "x86.h"
#include "dwarf2.h"
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#define SPACES "    "

void write_gblvar_defs(FILE *);

static struct LL_Function_ *called = NULL;
static int debug_calls = 0;
static int text_calls = 0;

static const char *
ll_get_linkage_string(enum LL_LinkageType linkage)
{
  switch (linkage) {
  case LL_INTERNAL_LINKAGE:
    return "internal";
  case LL_COMMON_LINKAGE:
    return "common";
  case LL_WEAK_LINKAGE:
    return "weak";
  case LL_EXTERNAL_LINKAGE:
    return "external";
  case LL_NO_LINKAGE:
    break;
  default:
    break;
  }
  return "";
}

/**
   \brief Write out the header of a module.

   The header is: Module ID, target triple, and datalayout
 */
void
ll_write_module_header(FILE *out, LLVMModuleRef module)
{
  if (module->module_name[0])
    fprintf(out, "; ModuleID = '%s'\n", module->module_name);
  if (module->datalayout_string[0])
    fprintf(out, "target datalayout = \"%s\"\n", module->datalayout_string);
  if (module->target_triple[0])
    fprintf(out, "target triple = \"%s\"\n", module->target_triple);
}

/**
   \brief Write out definitions for named struct types in module.
  
   If this function is called more than once, only the new types added since
   the last call are written.
 */
void
ll_write_user_structs(FILE *out, LLVMModuleRef module)
{
  unsigned i, j;

  for (i = module->written_user_structs; i < module->num_user_structs; i++) {
    LL_Value *val = module->user_structs.values[i];
    LL_Type *type = val->type_struct;
    int packed = type->flags & LL_TYPE_IS_PACKED_STRUCT;

    /* TODO: Should we support opaque struct types too? */
    if (type->sub_elements == 0) {
      fprintf(out, "%s = type %s\n", type->str, packed ? "<{}>" : "{}");
      continue;
    }

    fprintf(out, "%s = type %s{%s", type->str, packed ? "<" : "",
            type->sub_types[0]->str);
    for (j = 1; j < type->sub_elements; j++)
      fprintf(out, ", %s", type->sub_types[j]->str);
    fprintf(out, "}%s\n", packed ? ">" : "");
  }

  /* Avoid rewriting these structs if called again. */
  module->written_user_structs = i;
}

static const char *
get_op_name(enum LL_Op op)
{
  switch (op) {
  case LL_FPTRUNC:
    return "fptrunc";
  case LL_FPEXT:
    return "fpext";
  case LL_SEXT:
    return "sext";
  case LL_ZEXT:
    return "zext";
  case LL_TRUNC:
    return "trunc";
  case LL_BITCAST:
    return "bitcast";
  case LL_SITOFP:
    return "sitofp";
  case LL_UITOFP:
    return "uitofp";
  case LL_FPTOSI:
    return "fptosi";
  case LL_FPTOUI:
    return "fptoui";
  case LL_ADD:
    return "add";
  case LL_FADD:
    return "fadd";
  case LL_SUB:
    return "sub";
  case LL_FSUB:
    return "fsub";
  case LL_MUL:
    return "mul";
  case LL_FMUL:
    return "fmul";
  case LL_UDIV:
    return "udiv";
  case LL_SDIV:
    return "sdiv";
  case LL_SREM:
    return "srem";
  case LL_UREM:
    return "urem";
  case LL_FDIV:
    return "fdiv";
  case LL_OR:
    return "or";
  case LL_ASHR:
    return "ashr";
  case LL_LSHR:
    return "lshr";
  case LL_AND:
    return "and";
  case LL_XOR:
    return "xor";
  case LL_SHL:
    return "shl";
  case LL_INTTOPTR:
    return "inttoptr";
  case LL_PTRTOINT:
    return "ptrtoint";
  case LL_ICMP:
    return "icmp";
  case LL_FCMP:
    return "fcmp";
  default:
    return "thisisnotacceptable";
  }
}

static void
add_prototype(struct LL_Instruction_ *instruction)
{
  struct LL_Function_ *scan_function = called;
  struct LL_Function_ *new_function;
  LL_Value *function = instruction->operands[1];
  int i;

  if (function->data == NULL) {
    fprintf(stderr, "Attempting to add prototype for function with no name.\n");
    return;
  }

  while (scan_function != NULL) {
    if (strcmp(scan_function->name, function->data) == 0) {
      /* We've already prototyped this function.  Exit. */
      return;
    }
    scan_function = scan_function->next;
  }
  new_function = malloc(sizeof(struct LL_Function_));
  ll_set_function_num_arguments(new_function, instruction->num_operands - 2);
  new_function->next = called;
  new_function->name = function->data;
  new_function->num_args = instruction->num_operands - 2;

  new_function->return_type = function->type_struct;
  called = new_function;

  for (i = 2; i < instruction->num_operands; i++) {
    new_function->arguments[i - 2] = instruction->operands[i];
  }
}

static bool
defined_in_module(struct LL_Function_ *function, LLVMModuleRef module)
{
  struct LL_Function_ *scan_function;
  scan_function = module->first;
  while (scan_function != NULL) {
    if (strcmp(scan_function->name, function->name) == 0)
      return true;
    scan_function = scan_function->next;
  }
  return false;
}

static void
write_prototypes(FILE *out, LLVMModuleRef module)
{
  struct LL_Function_ *cur_function = called;
  int i;

  while (cur_function != NULL) {
    if (!defined_in_module(cur_function, module)) {
      fprintf(out, "\ndeclare %s @%s(", cur_function->return_type->str,
              cur_function->name);
      for (i = 0; i < cur_function->num_args; i++) {
        fprintf(out, "%s", cur_function->arguments[i]->type_struct->str);

        if (i + 1 < cur_function->num_args) {
          fprintf(out, ", ");
        }
      }
      fprintf(out, ") nounwind");
    }
    cur_function = cur_function->next;
  }
  fprintf(out, "\n");
  if (debug_calls) {
    fprintf(out, "declare void @llvm.dbg.declare(metadata, metadata) nounwind "
                 "readnone\n");
    fprintf(out, "declare void @llvm.dbg.value(metadata, i64, metadata) "
                 "nounwind readnone\n\n");
  }

  if (text_calls) {
    fprintf(out, "declare i64 @llvm.nvvm.texsurf.handle.p1i64(metadata, i64 "
                 "addrspace(1)*) nounwind readnone\n");
  }

  for (i = 0; i < module->num_refs; i++) {
    if (module->extern_func_refs[i] != NULL)
      fprintf(out, "declare void %s()\n", module->extern_func_refs[i]->data);
  }
}

static void
clear_prototypes(void)
{
  struct LL_Function_ *scan_function = called;
  struct LL_Function_ *next_function;

  while (scan_function != NULL) {
    free(scan_function->arguments);
    next_function = scan_function->next;
    free(scan_function);
    scan_function = next_function;
  }
  called = NULL;
}

static void
render_bitcast(FILE *out, struct LL_Instruction_ *inst)
{
  const char *cast_operand = inst->operands[1]->data;

  if (inst->operands[1]->type_struct->data_type == LL_PTR &&
      strcmp(inst->operands[1]->data, "0") == 0) {
    /* Replace "0" with "null" */
    cast_operand = "null";
  }
  fprintf(out, "%s%s = bitcast %s %s to %s", SPACES, inst->operands[0]->data,
          inst->operands[1]->type_struct->str, cast_operand,
          inst->operands[0]->type_struct->str);
}

static void
render_store(FILE *out, struct LL_Instruction_ *inst)
{
  const char *store_operand = inst->operands[0]->data;

  if (inst->operands[0]->type_struct->data_type == LL_PTR &&
      strcmp(inst->operands[0]->data, "0") == 0) {
    /* Replace "0" with "null" */
    store_operand = "null";
  } else if (inst->operands[0]->type_struct->data_type == LL_FLOAT) {
    if (strcmp(inst->operands[0]->data, "inf") == 0) {
      store_operand = "0x7FF0000000000000";
    } else if (strcmp(inst->operands[0]->data, "-inf") == 0) {
      store_operand = "0xFFF0000000000000";
    } else if (strcmp(inst->operands[0]->data, "nan") == 0) {
      store_operand = "0x7FF8000000000000";
    }
  }
  fprintf(out, "%sstore%s %s %s, %s %s", SPACES,
          (inst->flags & INST_VOLATILE) ? " volatile" : "",
          inst->operands[0]->type_struct->str, store_operand,
          inst->operands[1]->type_struct->str, inst->operands[1]->data);
  if (inst->num_operands >= 3)
    fprintf(out, ", align %s", inst->operands[2]->data);
}

void
ll_write_instruction(FILE *out, struct LL_Instruction_ *inst)
{
  const char *opname;
  int i;
  int print_branch_target;

  if (inst->flags & INST_CANCELED)
    return;
  print_branch_target = 0;
  opname = get_op_name(inst->op);
  switch (inst->op) {
  case LL_ADD:
  case LL_FADD:
  case LL_SUB:
  case LL_FSUB:
  case LL_MUL:
  case LL_FMUL:
  case LL_UDIV:
  case LL_SDIV:
  case LL_FDIV:
  case LL_UREM:
  case LL_SREM:
  case LL_ASHR:
  case LL_OR:
  case LL_AND:
  case LL_XOR:
  case LL_LSHR:
  case LL_SHL:
    /* Group all binary operations */
    fprintf(out, "%s%s = %s %s %s, %s", SPACES, inst->operands[0]->data, opname,
            inst->operands[1]->type_struct->str, inst->operands[1]->data,
            inst->operands[2]->data);
    break;
  case LL_STORE:
    render_store(out, inst);
    break;
  case LL_LOAD:
    fprintf(out, "%s%s = load%s %s %s", SPACES, inst->operands[0]->data,
            (inst->flags & INST_VOLATILE) ? " volatile" : "",
            inst->operands[1]->type_struct->str, inst->operands[1]->data);
    if (inst->num_operands >= 3)
      fprintf(out, ", align %s", inst->operands[2]->data);
    break;
  case LL_SEXT:
  case LL_ZEXT:
  case LL_TRUNC:
  case LL_FPTRUNC:
  case LL_FPEXT:
  case LL_SITOFP:
  case LL_UITOFP:
  case LL_PTRTOINT:
  case LL_INTTOPTR:
  case LL_FPTOSI:
  case LL_FPTOUI:
    /* Group all conversion operations */
    fprintf(out, "%s%s = %s %s %s to %s", SPACES, inst->operands[0]->data,
            opname, inst->operands[1]->type_struct->str,
            inst->operands[1]->data, inst->operands[0]->type_struct->str);
    break;
  case LL_BITCAST:
    render_bitcast(out, inst);
    break;
  case LL_RET:
    fprintf(out, "%sret %s %s", SPACES, inst->operands[0]->type_struct->str,
            inst->operands[0]->data);
    break;
  case LL_ICMP:
  case LL_FCMP:
    fprintf(out, "%s%s = %s %s %s %s, %s", SPACES, inst->operands[0]->data,
            opname, inst->operands[1]->data,
            inst->operands[2]->type_struct->str, inst->operands[2]->data,
            inst->operands[3]->data);
    break;
  case LL_SELECT:
    fprintf(out, "%s%s = select i1 %s, %s %s, %s %s", SPACES,
            inst->operands[0]->data, inst->operands[1]->data,
            inst->operands[2]->type_struct->str, inst->operands[2]->data,
            inst->operands[3]->type_struct->str, inst->operands[3]->data);
    break;
  case LL_BR:
    fprintf(out, "%sbr i1 %s, label %%%s, label %%%s", SPACES,
            inst->operands[0]->data, inst->operands[1]->data,
            inst->operands[2]->data);
    print_branch_target = 1;
    break;
  case LL_UBR:
    fprintf(out, "%sbr label %%%s", SPACES, inst->operands[0]->data);
    break;
  case LL_CALL:
    /* TODO: support fancier calls */
    if (inst->operands[0]->type_struct->data_type != LL_VOID) {
      fprintf(out, "%s%s = call %s @%s(", SPACES, inst->operands[0]->data,
              inst->operands[1]->type_struct->str, inst->operands[1]->data);
    } else {
      fprintf(out, "%scall %s @%s(", SPACES,
              inst->operands[1]->type_struct->str, inst->operands[1]->data);
    }
    for (i = 2; i < inst->num_operands; i++) {
      fprintf(out, "%s %s", inst->operands[i]->type_struct->str,
              inst->operands[i]->data);
      if (i + 1 < inst->num_operands) {
        fprintf(out, ", ");
      }
    }
    fprintf(out, ")");
    if (!inst->flags & IN_MODULE_CALL) {
      add_prototype(inst);
    }
    break;
  case LL_TEXTCALL:
    if (inst->operands[0]->type_struct->data_type != LL_VOID) {
      fprintf(out, "%s%s = call %s @%s(", SPACES, inst->operands[0]->data,
              inst->operands[1]->type_struct->str, inst->operands[1]->data);
    } else {
      fprintf(out, "%scall %s @%s(", SPACES,
              inst->operands[1]->type_struct->str, inst->operands[1]->data);
    }
    fprintf(out, "metadata !{%s %s}, ", inst->operands[2]->type_struct->str,
            inst->operands[2]->data);
    fprintf(out, "%s %s", inst->operands[2]->type_struct->str,
            inst->operands[2]->data);
    fprintf(out, ")");
    text_calls = 1;
    break;
  case LL_GEP:
    fprintf(out, "%s%s = getelementptr %s %s", SPACES, inst->operands[0]->data,
            inst->operands[1]->type_struct->str, inst->operands[1]->data);
    for (i = 2; i < inst->num_operands; i++) {
      fprintf(out, ", %s %s", inst->operands[i]->type_struct->str,
              inst->operands[i]->data);
    }
    break;
  case LL_ALLOCA:
    fprintf(out, "%s%s = alloca %s", SPACES, inst->operands[0]->data,
            inst->operands[1]->type_struct->str);
    if (inst->num_operands >= 3)
      fprintf(out, ", align %s", inst->operands[2]->data);
    break;
  case LL_UNREACHABLE:
    fprintf(out, "%sunreachable", SPACES);
    break;
  case LL_SWITCH:
    fprintf(out, "%sswitch %s %s, label %%%s [\n", SPACES,
            inst->operands[0]->type_struct->str, inst->operands[0]->data,
            inst->operands[1]->data);
    for (i = 2; i < inst->num_operands; i += 2) {
      fprintf(out, "%s  %s %s, label %%%s\n", SPACES,
              inst->operands[i + 0]->type_struct->str,
              inst->operands[i + 0]->data, inst->operands[i + 1]->data);
    }
    fprintf(out, "%s]", SPACES);
    break;
  case LL_NONE:
    break;
  default:
    fprintf(stderr, "Error: unrendered instruction %d\n", inst->op);
    break;
  }
  if (!LL_MDREF_IS_NULL(inst->dbg_line_op)) {
    fprintf(out, ", !dbg !%u", LL_MDREF_value(inst->dbg_line_op));
  }
#if DEBUG
  if (inst->comment)
    fprintf(out, " ; %s", inst->comment);
#endif

  fputc('\n', out);
  if (print_branch_target)
    fprintf(out, "%s:\n", inst->operands[2]->data);
  fflush(out);
}

/**
   \brief Emit a list of \c !dbg \e n annotations
   \param ods  the object to \c !dbg list
   
   In LLVM 4.0, we can generate a list of comma separated \c !dbg metadata to
   link the object to a number of debug metadata descriptions.
 */
void
ll_write_object_dbg_references(FILE *out, LL_Module *m, LL_ObjToDbgList *ods)
{
  LL_ObjToDbgListIter i;
  if (!ll_feature_from_global_to_md(&m->ir))
    return;
  for (llObjtodbgFirst(ods, &i); !llObjtodbgAtEnd(&i); llObjtodbgNext(&i)) {
    LL_MDRef mdnode = llObjtodbgGet(&i);
    fprintf(out, ", !dbg !%u", LL_MDREF_value(mdnode));    
  }
  llObjtodbgFree(ods);
}

void
ll_write_basicblock(FILE *out, LL_Function *function, LL_BasicBlock *block)
{
  LL_Instruction *inst = block->first;

  if (block->name)
    fprintf(out, "%s:\n", block->name);

  if (block == function->first)
    ll_write_local_objects(out, function);

  while (inst) {
    ll_write_instruction(out, inst);
    inst = inst->next;
  }
}

/**
   \brief Write out definitions of local objects in function as a series of
   alloca instructions.
 
   Unlike ll_write_global_objects(), this function only expects to be called
   once per function.
 */
void
ll_write_local_objects(FILE *out, LL_Function *function)
{
  LL_Object *object;

  for (object = function->first_local; object; object = object->next) {
    fprintf(out, "%s%s = alloca %s", SPACES, object->address.data,
            object->type->str);
    if (object->align_bytes)
      fprintf(out, ", align %u", object->align_bytes);
    fprintf(out, "\n");
  }
}

void
ll_write_function(FILE *out, LL_Function *function)
{
  int i;
  char attribute[256];
  LL_BasicBlock *block = function->first;

  fprintf(out, "define %s %s %s @%s(", ll_get_linkage_string(function->linkage),
          function->calling_convention, function->return_type->str,
          function->name);
  for (i = 0; i < function->num_args; i++) {
    fprintf(out, "%s", function->arguments[i]->type_struct->str);

    if (function->arguments[i]->flags & VAL_IS_NOALIAS_PARAM) {
      fprintf(out, " noalias");
    }

    fprintf(out, " %s", function->arguments[i]->data);
    if (i + 1 < function->num_args)
      fprintf(out, ", ");
  }
  fprintf(out, ") nounwind {\n");

  while (block) {
    ll_write_basicblock(out, function, block);
    block = block->next;
  }
  fprintf(out, "}\n\n");
}

void
ll_write_function_signature(FILE *out, struct LL_Function_ *function)
{
  int j;
  fprintf(out, "%s (", function->return_type->str);
  for (j = 0; j < function->num_args; j++) {
    fprintf(out, "%s", function->arguments[j]->type_struct->str);
    if (j + 1 < function->num_args)
      fprintf(out, ", ");
  }
  fprintf(out, ")* @%s", function->name);
}

/*
 * Metadata
 */

enum FieldType {
  UnsignedField,
  SignedField,
  BoolField,
  NodeField,
  StringField,
  ValueField,
  DWTagField,
  DWLangField,
  DWVirtualityField,
  DWEncodingField,
  DWEmissionField
};

enum FieldFlags {
  /** Field must be present, even with a default value. */
  FlgMandatory = 0x1,
  /** Field does not have to be present in the MDNode. */
  FlgOptional = 0x2,
  /** Field is never printed. */
  FlgHidden = 0x4
};

/**
 * \brief Templates for printing specialized metadata nodes.
 *
 * A template is an array of MDTemplate structs with nf+1 elements where nf is
 * the number of fields.
 *
 * The first entry of the array is the class name without the leading bang
 * (e.g., "MDLocation"), and the "flags" field contains nf.
 *
 * The remaining entries correspond to the node elements, the names are field
 * names without the trailing colon.
 */
typedef struct MDTemplate {
  const char *name;
  enum FieldType type;
  unsigned flags;
} MDTemplate;

/* !DILocation(line: 2900, column: 42, scope: !1, inlinedAt: !2) */
static const MDTemplate Tmpl_DILocation[] = {
  {"DILocation", 0, 4},      {"line", UnsignedField},
  {"column", UnsignedField}, {"scope", NodeField, FlgMandatory},
  {"inlinedAt", NodeField}
};

/* !MDLocation(line: 2900, column: 42, scope: !1, inlinedAt: !2) */
static const MDTemplate Tmpl_MDLocation[] = {
  {"MDLocation", 0, 4},      {"line", UnsignedField},
  {"column", UnsignedField}, {"scope", NodeField, FlgMandatory},
  {"inlinedAt", NodeField}
};

/* An DIFile(filename: "...", directory: "...") pair */
static const MDTemplate Tmpl_DIFile_pair[] = {
  {"DIFile", 0, 2},          {"filename", StringField},
  {"directory", StringField}
};

/* A tagged MDFile node. Not used by LLVM. */
static const MDTemplate Tmpl_DIFile_tagged[] = {
  {"DIFile", 0, 2},   {"tag", DWTagField},
  {"pair", NodeField}
};

/* MDFile before 3.4 */
static const MDTemplate Tmpl_DIFile_pre34[] = {
  {"DIFile", 0, 4},          {"tag", DWTagField},
  {"filename", StringField}, {"directory", StringField},
  {"context", NodeField}
};

static const MDTemplate Tmpl_DICompileUnit[] = {
  {"DICompileUnit", 0, 13},   {"tag", DWTagField, FlgHidden},
  {"file", NodeField},        {"language", DWLangField},
  {"producer", StringField},  {"isOptimized", BoolField},
  {"flags", StringField},     {"runtimeVersion", UnsignedField},
  {"enums", NodeField},       {"retainedTypes", NodeField},
  {"subprograms", NodeField}, {"globals", NodeField},
  {"imports", NodeField},     {"splitDebugFilename", StringField}
};

/* "subprograms" removed from DICompileUnit in LLVM 3.9 */
static const MDTemplate Tmpl_DICompileUnit_ver39[] = {
  {"DICompileUnit", 0, 13},  {"tag", DWTagField, FlgHidden},
  {"file", NodeField},       {"language", DWLangField},
  {"producer", StringField}, {"isOptimized", BoolField},
  {"flags", StringField},    {"runtimeVersion", UnsignedField},
  {"enums", NodeField},      {"retainedTypes", NodeField},
  {"globals", NodeField},    {"emissionKind", DWEmissionField},
  {"imports", NodeField},    {"splitDebugFilename", StringField}
};

static const MDTemplate Tmpl_DICompileUnit_pre34[] = {
  {"DICompileUnit", 0, 14},    {"tag", DWTagField, FlgHidden},
  {"unused", NodeField, FlgHidden},
  {"language", DWLangField},   {"filename", StringField},
  {"directory", StringField},  {"producer", StringField},
  {"isMain", BoolField},       {"isOptimized", BoolField},
  {"flags", StringField},      {"runtimeVersion", UnsignedField},
  {"enums", NodeField},        {"retainedTypes", NodeField},
  {"subprograms", NodeField},  {"globals", NodeField}
};

static const MDTemplate Tmpl_DINamespace_pre34[] = {
  {"DINamespace", 0, 5}, {"tag", DWTagField},
  {"scope", NodeField},  {"name", StringField},
  {"file", NodeField},   {"line", UnsignedField}
};

static const MDTemplate Tmpl_DINamespace_post34[] = {
  {"DINamespace", 0, 5},    {"tag", DWTagField},
  {"file", NodeField},      {"scope", NodeField},
  {"name", StringField},    {"line", UnsignedField}
};

static const MDTemplate Tmpl_DISubprogram[] = {
  {"DISubprogram", 0, 20},         {"tag", DWTagField, FlgHidden},
  {"file", NodeField},             {"scope", NodeField},
  {"name", StringField},           {"displayName", StringField, FlgHidden},
  {"linkageName", StringField},    {"line", UnsignedField},
  {"type", NodeField},             {"isLocal", BoolField},
  {"isDefinition", BoolField},     {"virtuality", DWVirtualityField},
  {"virtualIndex", UnsignedField}, {"containingType", NodeField},
  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"isOptimized", BoolField},      {"function", ValueField},
  {"templateParams", NodeField},   {"declaration", NodeField},
  {"variables", NodeField},        {"scopeLine", UnsignedField}
};

static const MDTemplate Tmpl_DISubprogram_38[] = {
  {"DISubprogram", 0, 20},         {"tag", DWTagField, FlgHidden},
  {"file", NodeField},             {"scope", NodeField},
  {"name", StringField},           {"displayName", StringField, FlgHidden},
  {"linkageName", StringField},    {"line", UnsignedField},
  {"type", NodeField},             {"isLocal", BoolField},
  {"isDefinition", BoolField},     {"virtuality", DWVirtualityField},
  {"virtualIndex", UnsignedField}, {"containingType", NodeField},
  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"isOptimized", BoolField},      {"function", ValueField, FlgHidden},
  {"templateParams", NodeField},   {"declaration", NodeField},
  {"variables", NodeField},        {"scopeLine", UnsignedField}
};

/** "unit" was added in LLVM 3.9 for DISubprogram */
static const MDTemplate Tmpl_DISubprogram_39[] = {
  {"DISubprogram", 0, 21},         {"tag", DWTagField, FlgHidden},
  {"file", NodeField},             {"scope", NodeField},
  {"name", StringField},           {"displayName", StringField, FlgHidden},
  {"linkageName", StringField},    {"line", UnsignedField},
  {"type", NodeField},             {"isLocal", BoolField},
  {"isDefinition", BoolField},     {"virtuality", DWVirtualityField},
  {"virtualIndex", UnsignedField}, {"containingType", NodeField},
  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"isOptimized", BoolField},      {"function", ValueField, FlgHidden},
  {"templateParams", NodeField},   {"declaration", NodeField},
  {"unit", NodeField},             {"variables", NodeField},
  {"scopeLine", UnsignedField}
};

static const MDTemplate Tmpl_DILexicalBlock[] = {
  {"DILexicalBlock", 0, 6},  {"tag", DWTagField, FlgHidden},
  {"file", NodeField},       {"scope", NodeField},
  {"line", UnsignedField},   {"column", UnsignedField},
  {"discriminator", UnsignedField, FlgHidden | FlgOptional}
};

static const MDTemplate Tmpl_DILexicalBlock_pre34[] = {
  {"DILexicalBlock", 0, 6},  {"tag", DWTagField, FlgHidden},
  {"scope", NodeField},      {"line", UnsignedField},
  {"column", UnsignedField}, {"file", NodeField},
  {"discriminator", UnsignedField, FlgOptional}
};

static const MDTemplate Tmpl_DILexicalBlockFile[] = {
  {"DILexicalBlock", 0, 4}, {"tag", DWTagField, FlgHidden},
  {"file", NodeField},      {"scope", NodeField},
  {"discriminator", UnsignedField}
};

static const MDTemplate Tmpl_DIExpression[] = {
  {"DIExpression", 0, 0}
};

static const MDTemplate Tmpl_DILocalVariable[] = {
  {"DILocalVariable", 0, 9},   {"tag", DWTagField},
  {"scope", NodeField},        {"name", StringField},
  {"arg", UnsignedField},      {"file", NodeField},
  {"line", UnsignedField},     {"type", NodeField},
  {"flags", UnsignedField},    /* TBD: DIFlag... */
  {"inlinedAt", UnsignedField} /* TBD: NodeField */
};

static const MDTemplate Tmpl_DILocalVariable_38[] = {
  {"DILocalVariable", 0, 8},   {"scope", NodeField},
  {"name", StringField},       {"arg", UnsignedField},
  {"file", NodeField},         {"line", UnsignedField},
  {"type", NodeField},
  {"flags", UnsignedField},    /* TBD: DIFlag... */
  {"inlinedAt", UnsignedField} /* TBD: NodeField */
};

static const MDTemplate Tmpl_DILocalVariable_embedded_argnum[] = {
  {"DILocalVariable", 0, 8},   {"tag", DWTagField},
  {"scope", NodeField},        {"name", StringField},
  {"file", NodeField},         {"line_and_arg", UnsignedField},
  {"type", NodeField},
  {"flags", UnsignedField},    /* TBD: DIFlag... */
  {"inlinedAt", UnsignedField} /* TBD: NodeField */
};

static const MDTemplate Tmpl_DIGlobalVariable[] = {
  {"DIGlobalVariable", 0, 13}, {"tag", DWTagField, FlgHidden},
  {"unused", NodeField, FlgHidden},
  {"scope", NodeField},        {"name", StringField},
  {"displayName", StringField, FlgHidden}, {"linkageName", StringField},
  {"file", NodeField},         {"line", UnsignedField},
  {"type", NodeField},         {"isLocal", BoolField},
  {"isDefinition", BoolField}, {"variable", ValueField},
  {"addrspace", UnsignedField, FlgOptional} /* nvvm extension */
                                            /* Missing: declaration */
};

static const MDTemplate Tmpl_DIGlobalVariable4[] = {
  {"DIGlobalVariable", 0, 12}, {"tag", DWTagField, FlgHidden},
  {"unused", NodeField, FlgHidden},
  {"scope", NodeField},        {"name", StringField},
  {"displayName", StringField, FlgHidden}, {"linkageName", StringField},
  {"file", NodeField},         {"line", UnsignedField},
  {"type", NodeField},         {"isLocal", BoolField},
  {"isDefinition", BoolField},
  {"addrspace", UnsignedField, FlgOptional} /* nvvm extension */
};

static const MDTemplate Tmpl_DIGlobalVariableExpression[] = {
  { "DIGlobalVariableExpression", 0, 2}, { "var", NodeField },
  { "expr", NodeField }
};

static const MDTemplate Tmpl_DIBasicType_pre34[] = {
  {"DIBasicType", 0, 10},     {"tag", DWTagField},
  {"scope", NodeField},       {"name", StringField},
  {"file", NodeField},        {"line", UnsignedField},
  {"size", UnsignedField},    {"align", UnsignedField},
  {"offset", UnsignedField},  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"encoding", DWEncodingField}
};

static const MDTemplate Tmpl_DIBasicType[] = {
  {"DIBasicType", 0, 10},      {"tag", DWTagField},
  {"unused", NodeField, FlgHidden}, {"unused", NodeField, FlgHidden},
  {"name", StringField},      {"line", UnsignedField},
  {"size", UnsignedField},    {"align", UnsignedField},
  {"offset", UnsignedField},  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"encoding", DWEncodingField}
};

static const MDTemplate Tmpl_DISubroutineType[] = {
  {"DISubroutineType", 0, 15}, {"tag", DWTagField, FlgHidden},
  {"unused", UnsignedField},   {"unused", NodeField},
  {"name", StringField},       {"unused", UnsignedField},
  {"unused", UnsignedField},   {"unused", UnsignedField},
  {"unused", UnsignedField},   {"unused", UnsignedField},
  {"unused", NodeField},       {"types", NodeField},
  {"unused", UnsignedField},   {"unused", NodeField},
  {"unused", NodeField},       {"unused", NodeField},
};

static const MDTemplate Tmpl_DIDerivedType_pre34[] = {
  {"DIDerivedType", 0, 10},  {"tag", DWTagField},
  {"scope", NodeField},      {"name", StringField},
  {"file", NodeField},       {"line", UnsignedField},
  {"size", UnsignedField},   {"align", UnsignedField},
  {"offset", UnsignedField}, {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"baseType", NodeField}
};

static const MDTemplate Tmpl_DIDerivedType[] = {
  {"DIDerivedType", 0, 10},  {"tag", DWTagField},
  {"file", NodeField},       {"scope", NodeField},
  {"name", StringField},     {"line", UnsignedField},
  {"size", UnsignedField},   {"align", UnsignedField},
  {"offset", UnsignedField}, {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"baseType", NodeField}
};

static const MDTemplate Tmpl_DICompositeType_pre34[] = {
  {"DICompositeType", 0, 13},   {"tag", DWTagField},
  {"scope", NodeField},         {"name", StringField},
  {"file", NodeField},          {"line", UnsignedField},
  {"size", UnsignedField},      {"align", UnsignedField},
  {"offset", UnsignedField},    {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"baseType", NodeField},      {"elements", NodeField},
  {"runtimeLang", DWLangField}, {"unused", NodeField, FlgHidden}
};

static const MDTemplate Tmpl_DICompositeType[] = {
  {"DICompositeType", 0, 15}, {"tag", DWTagField},
  {"file", NodeField},        {"scope", NodeField},
  {"name", StringField},      {"line", UnsignedField},
  {"size", UnsignedField},    {"align", UnsignedField},
  {"offset", UnsignedField},  {"flags", UnsignedField}, /* TBD: DIFlag... */
  {"baseType", NodeField},    {"elements", NodeField},
  {"runtimeLang", DWLangField},  {"vtableHolder", NodeField},
  {"templateParams", NodeField}, {"identifier", StringField}
};

static const MDTemplate Tmpl_DISubrange[] = {
  {"DISubrange", 0, 3},        {"tag", DWTagField, FlgHidden},
  {"lowerBound", SignedField}, {"count", SignedField, FlgMandatory}
};

static const MDTemplate Tmpl_DISubrange_pre37[] = {
  {"DISubrange", 0, 3},        {"tag", DWTagField, FlgHidden},
  {"lowerBound", SignedField}, {"upperBound", SignedField}
};

static const MDTemplate Tmpl_DIEnumerator[] = {
  {"DIEnumerator", 0, 3}, {"tag", DWTagField, FlgHidden},
  {"name", StringField},  {"value", SignedField, FlgMandatory}
};

/**
   \brief Write out an \ref LL_MDRef from \p module.
   \param out	  output file
   \param module  the LLVM module
   \param rmdref  the metadata node to be written
   \param omit_metadata_type  if true then omit \c metadata keyword

   This functions writes a metadata reference as it appears in metadata context,
   such as inside a metadata node definition, or following a \c !dbg tag.

   Metadata references may use a different syntax when used as function
   arguments, depending on the LLVM version. That is \e not dealt with by this
   function.
 */
void
write_mdref(FILE *out, LL_Module *module, LL_MDRef rmdref,
            int omit_metadata_type)
{
  const char *tag = "metadata ";
  LL_MDRef mdref = rmdref;

  /* The metadata type tag is omitted in metadata context in LLVM 3.6+, and
   * always in named metadata definitions. */
  if (omit_metadata_type)
    tag = "";

  switch (LL_MDREF_kind(mdref)) {
  case MDRef_Node:
    if (LL_MDREF_value(mdref))
      fprintf(out, "%s!%u", tag, LL_MDREF_value(mdref));
    else
      fprintf(out, "null");
    break;

  case MDRef_String:
    assert(LL_MDREF_value(mdref) < module->mdstrings_count, "Bad string MDRef",
           LL_MDREF_value(mdref), 4);
    fprintf(out, "%s%s", tag, module->mdstrings[LL_MDREF_value(mdref)]);
    break;

  case MDRef_Constant:
    assert(LL_MDREF_value(mdref) < module->constants_count,
           "Bad constant MDRef", LL_MDREF_value(mdref), 4);
    fprintf(out, "%s %s",
            module->constants[LL_MDREF_value(mdref)]->type_struct->str,
            module->constants[LL_MDREF_value(mdref)]->data);
    break;

  case MDRef_SmallInt1:
    fprintf(out, "i1 %u", LL_MDREF_value(mdref));
    break;

  case MDRef_SmallInt32:
    fprintf(out, "i32 %u", LL_MDREF_value(mdref));
    break;

  case MDRef_SmallInt64:
    fprintf(out, "i64 %u", LL_MDREF_value(mdref));
    break;

  default:
    interr("Invalid MDRef kind", LL_MDREF_kind(mdref), 4);
  }
}

/**
   \brief generate full DWARF debug emission mode
 */
static const char *
dwarf_emission_name(int value)
{
  switch (value) {
  case 2:
    return "NoDebug";
  case 3:
    return "LineTablesOnly";
  default:
    return "FullDebug";
  }
}

/**
   \brief Write out an an LL_MDRef as a field in a specialised MDNode class
   \param out	       file to write to
   \param module       module containing the metadata
   \param node	       the metadata node to be written
   \param needs_comma  If true, print a ", " before the field label
   \return true iff the field was actually printed

   Includes priting the "name:" label.  The field is not printed if it has its
   default value.
 
   The formatting is guided by the field type from the MDTemplate, and the
   MDRef types are validated. 
 */
static int
write_mdfield(FILE *out, LL_Module *module, int needs_comma, LL_MDRef mdref,
              const MDTemplate *tmpl)
{
  unsigned value = LL_MDREF_value(mdref);
  const char *prefix = needs_comma ? ", " : "";
  int mandatory = (tmpl->flags & FlgMandatory) != 0;

  if (tmpl->flags & FlgHidden)
    return FALSE;

  switch (LL_MDREF_kind(mdref)) {
  case MDRef_Node:
    if (value) {
      assert(tmpl->type == NodeField, "metadata elem should not be a mdnode",
             tmpl->type, 4);
      fprintf(out, "%s%s: !%u", prefix, tmpl->name, value);
    } else if (mandatory) {
      fprintf(out, "%s%s: null", prefix, tmpl->name);
    } else {
      return FALSE;
    }
    break;

  case MDRef_String:
    assert(tmpl->type == StringField, "metadata elem should not be a string",
           tmpl->type, 4);
    assert(value < module->mdstrings_count, "Bad string MDRef", value, 4);
    if (!mandatory && strcmp(module->mdstrings[value], "!\"\"") == 0)
      return FALSE;
    /* The mdstrings[] entry is formatted as !"...". String the leading !. */
    fprintf(out, "%s%s: %s", prefix, tmpl->name, module->mdstrings[value] + 1);
    break;

  case MDRef_Constant:
    assert(value < module->constants_count, "Bad constant MDRef", value, 4);
    switch (tmpl->type) {
    case ValueField:
      fprintf(out, "%s%s: %s %s", prefix, tmpl->name,
              module->constants[value]->type_struct->str,
              module->constants[value]->data);
      break;

    case UnsignedField:
      if (module->constants[value]->data[0] == '-') {
        /* The value stored is negative.  LLVM expects it to be unsigned, so
           convert it to be positive. */
        long long intval = strtoll(module->constants[value]->data, NULL, 10);
        if ((long long)INT_MIN <= intval && intval < 0) {
          /* It was most likely a 32 bit value originally. */
          fprintf(out, "%s%s: %u", prefix, tmpl->name, (unsigned)(int)intval);
        } else {
          fprintf(out, "%s%s: %llu", prefix, tmpl->name, intval);
        }
      } else {
        fprintf(out, "%s%s: %s", prefix, tmpl->name,
                module->constants[value]->data);
      }
      break;
    case SignedField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name,
              module->constants[value]->data);
      break;

    default:
      interr("metadata elem should not be a value", tmpl->type, 0);
    }
    break;

  case MDRef_SmallInt1:
  case MDRef_SmallInt32:
  case MDRef_SmallInt64:
    if (!value && !mandatory)
      return FALSE;
    switch (tmpl->type) {
    case UnsignedField:
    case SignedField:
      fprintf(out, "%s%s: %u", prefix, tmpl->name, value);
      break;

    case BoolField:
      assert(value <= 1, "boolean value expected", value, 4);
      fprintf(out, "%s%s: %s", prefix, tmpl->name, value ? "true" : "false");
      break;

    case DWTagField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name,
              dwarf_tag_name(value & 0xffff));
      break;

    case DWLangField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name, dwarf_lang_name(value));
      break;

    case DWVirtualityField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name,
              dwarf_virtuality_name(value));
      break;

    case DWEncodingField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name, dwarf_encoding_name(value));
      break;

    case DWEmissionField:
      fprintf(out, "%s%s: %s", prefix, tmpl->name, dwarf_emission_name(value));
      break;

    default:
      interr("metadata elem should not be an int", tmpl->type, 0);
    }
    break;

  default:
    interr("Invalid MDRef kind", LL_MDREF_kind(mdref), 4);
  }

  return TRUE;
}

/*
 * Write out a metadata node definition in the "plain" style: !{ !1, ... }.
 *
 * When omit_metadata_type is set, don't print out the leading "metadata" type
 * tag. This doesn't affect the printing of the internal mdnode contents.
 */
static void
write_mdnode_plain(FILE *out, LL_Module *module, const LL_MDNode *node,
                   int omit_metadata_type)
{
  unsigned i;

  if (!omit_metadata_type)
    fprintf(out, "metadata ");

  if (ll_feature_use_distinct_metadata(&module->ir) && node->is_distinct)
    fprintf(out, "distinct ");

  fprintf(out, "!{ ");
  for (i = 0; i < node->num_elems; i++) {
    LL_MDRef mdref = LL_MDREF_INITIALIZER(0, 0);
    mdref = node->elem[i];
    if (i > 0)
      fprintf(out, ", ");
    write_mdref(out, module, mdref, omit_metadata_type);
  }
  fprintf(out, " }\n");
}

/*
 * Write out a metadata node in the specialized form: !MDLocation(line: 42,
 * ...).
 *
 * Also perform some basic schema validation against the provided template.
 */
static void
write_mdnode_spec(FILE *out, LL_Module *module, const LL_MDNode *node,
                  const MDTemplate *tmpl)
{
  const unsigned num_fields = tmpl->flags;
  unsigned i;
  int needs_comma = FALSE;

  if (ll_feature_use_distinct_metadata(&module->ir) && node->is_distinct)
    fprintf(out, "distinct ");

  assert(node->num_elems <= num_fields, "metadata node has too many fields.",
         node->num_elems, ERR_Fatal);

  fprintf(out, "!%s(", tmpl->name);
  for (i = 0; i < node->num_elems; i++)
    if (write_mdfield(out, module, needs_comma, node->elem[i], &tmpl[i + 1]))
      needs_comma = TRUE;
  fprintf(out, ")\n");
}

/**
   \brief Get the textual name for module-level named metadata.
 */
static const char *
get_metadata_name(enum LL_MDName name)
{
  switch (name) {
  case MD_llvm_module_flags:
    return "!llvm.module.flags";
  case MD_llvm_dbg_cu:
    return "!llvm.dbg.cu";
  case MD_opencl_kernels:
    return "!opencl.kernels";
  case MD_nvvm_annotations:
    return "!nvvm.annotations";
  case MD_nvvmir_version:
    return "!nvvmir.version";
  default:
    interr("Unknown metadata name", name, 4);
  }
  return NULL;
}

typedef const LL_MDNode *MDNodeRef;

static void emitRegular(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDICompileUnit(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIFile(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDiBasicType(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDISubroutineType(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIDerivedType(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDICompositeType(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDISubRange(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIEnumerator(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDINamespace(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIGlobalVariable(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDISubprogram(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDILexicalBlock(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDILexicalBlockFile(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDILocation(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDILocalVariable(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIExpression(FILE*, LLVMModuleRef, MDNodeRef, unsigned);
static void emitDIGlobalVariableExpression(FILE*, LLVMModuleRef, MDNodeRef,
                                           unsigned);

typedef void (*MDDispatchMethod)(FILE *out, LLVMModuleRef mod,
                                 MDNodeRef mdnode, unsigned mdi);

typedef struct MDDispatch {
  MDDispatchMethod method;
} MDDispatch;

static MDDispatch mdDispTable[LL_MDClass_MAX] = {
  {emitRegular},		    // LL_PlainMDNode
  {emitDICompileUnit},		    // LL_DICompileUnit
  {emitDIFile},			    // LL_DIFile
  {emitDiBasicType},		    // LL_DIBasicType
  {emitDISubroutineType},	    // LL_DISubroutineType
  {emitDIDerivedType},		    // LL_DIDerivedType
  {emitDICompositeType},	    // LL_DICompositeType
  {emitDISubRange},		    // LL_DISubRange
  {emitDIEnumerator},		    // LL_DIEnumerator
  {emitRegular},		    // LL_DITemplateTypeParameter
  {emitRegular},		    // LL_DITemplateValueParameter
  {emitDINamespace},		    // LL_DINamespace
  {emitDIGlobalVariable},	    // LL_DIGlobalVariable
  {emitDISubprogram},		    // LL_DISubprogram
  {emitDILexicalBlock},		    // LL_DILexicalBlock
  {emitDILexicalBlockFile},	    // LL_DILexicalBlockFile
  {emitDILocation},		    // LL_DILocation
  {emitDILocalVariable},	    // LL_DILocalVariable
  {emitDIExpression},		    // LL_DIExpression
  {emitRegular},		    // LL_DIObjCProperty
  {emitRegular},		    // LL_DIImportedEntity
  {emitDIGlobalVariableExpression}, // LL_DIGlobalVariableExpression
};

INLINE static void
emitRegularPrefix(FILE *out, unsigned mdi)
{
  fprintf(out, "!%u = ", mdi);
}

/** Simple helper function */
INLINE static bool
useSpecialized(LLVMModuleRef mod)
{
  return ll_feature_use_specialized_mdnodes(&mod->ir);
}

INLINE static void
emitRegular(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
            unsigned mdi)
{
  emitRegularPrefix(out, mdi);
  write_mdnode_plain(out, mod, mdnode, ll_feature_omit_metadata_type(&mod->ir));
}

INLINE static void
emitSpec(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
         unsigned mdi, const MDTemplate *tmpl)
{
  emitRegularPrefix(out, mdi);
  write_mdnode_spec(out, mod, mdnode, tmpl);
}

INLINE static void
emitUnspec(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
           unsigned mdi, const MDTemplate *tmpl)
{
  fputs("; ", out);
  emitSpec(out, mod, mdnode, mdi, tmpl);
  emitRegular(out, mod, mdnode, mdi);
}

static void
emitTmpl(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
         unsigned mdi, const MDTemplate *tmpl)
{
  if (useSpecialized(mod)) {
    emitSpec(out, mod, mdnode, mdi, tmpl);
    return;
  }
  emitUnspec(out, mod, mdnode, mdi, tmpl);
}

static void
emitDICompileUnit(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                  unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DICompileUnit_pre34);
    return;
  }
  if (ll_feature_subprogram_not_in_cu(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DICompileUnit_ver39);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DICompileUnit);
}

static void
emitDIFile(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode, unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DIFile_pre34);
    return;
  }
  if (LL_MDREF_kind(mdnode->elem[0]) == MDRef_String) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DIFile_pair);
    return;
  }
  emitUnspec(out, mod, mdnode, mdi, Tmpl_DIFile_tagged);    
}

static void
emitDiBasicType(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DIBasicType_pre34);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DIBasicType);
}

static void
emitDISubroutineType(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                     unsigned mdi)
{
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubroutineType);
}

static void
emitDIDerivedType(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                  unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DIDerivedType_pre34);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DIDerivedType);
}

static void
emitDICompositeType(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                    unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DICompositeType_pre34);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DICompositeType);
}

static void
emitDISubRange(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
               unsigned mdi)
{
  if (!ll_feature_debug_info_subrange_needs_count(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubrange_pre37);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubrange);
}

static void
emitDIEnumerator(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                 unsigned mdi)
{
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DIEnumerator);
}

static void
emitDINamespace(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DINamespace_pre34);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DINamespace_post34);
}

static void
emitDIGlobalVariable(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                     unsigned mdi)
{
  if (ll_feature_from_global_to_md(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DIGlobalVariable4);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DIGlobalVariable);
}

static void
emitDISubprogram(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                 unsigned mdi)
{
  if (!ll_feature_debug_info_pre34(&mod->ir)) {
    if (ll_feature_subprogram_not_in_cu(&mod->ir)) {
      // 3.9, 'unit:' was added
      emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubprogram_39);
      return;
    }
    if (ll_feature_debug_info_ver38(&mod->ir)) {
      // 3.8, 'function:' was removed
      emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubprogram_38);
      return;
    }
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DISubprogram);
    return;
  }
  emitRegular(out, mod, mdnode, mdi);
}

static void
emitDILexicalBlock(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                   unsigned mdi)
{
  if (ll_feature_debug_info_pre34(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, Tmpl_DILexicalBlock_pre34);
    return;
  }
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DILexicalBlock);
}

static void
emitDILexicalBlockFile(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                       unsigned mdi)
{
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DILexicalBlockFile);
}

static void 
emitDILocation(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
               unsigned mdi)
{
  const MDTemplate *tmpl = ll_feature_debug_info_DI_syntax(&mod->ir) ?
    Tmpl_DILocation : Tmpl_MDLocation;
  if (ll_feature_debug_info_mdlocation(&mod->ir)) {
    emitTmpl(out, mod, mdnode, mdi, tmpl);
    return;
  }
  emitUnspec(out, mod, mdnode, mdi, tmpl);
}

static void
emitDILocalVariable(FILE *out, LLVMModuleRef mod, const LL_MDNode *node,
                    unsigned mdi)
{
  if (ll_feature_dbg_local_variable_embeds_argnum(&mod->ir)) {
    emitTmpl(out, mod, node, mdi, Tmpl_DILocalVariable_embedded_argnum);
    return;
  }
  if (ll_feature_debug_info_ver38(&mod->ir)) {
    // 3.8, 'tag:' was removed
    emitTmpl(out, mod, node, mdi, Tmpl_DILocalVariable_38);
    return;
  }
  emitTmpl(out, mod, node, mdi, Tmpl_DILocation);
}

INLINE static const char *
ll_dw_op_to_name(LL_DW_OP_t op)
{
  switch (op) {
  case LL_DW_OP_deref:
    return "DW_OP_deref";
  case LL_DW_OP_plus:
    return "DW_OP_plus";
  case LL_DW_OP_LLVM_fragment:
    return "DW_OP_LLVM_fragment";
  case LL_DW_OP_swap:
    return "DW_OP_swap";
  case LL_DW_OP_xderef:
    return "DW_OP_xderef";
  case LL_DW_OP_stack_value:
    return "DW_OP_stack_value"; 
  default:
    break;
  }
  DEBUG_ASSERT(false, "unhandled LL_DW_OP_t");
  return "*bug*";
}

INLINE static const char *
decode_expression_op(LLVMModuleRef mod, LL_MDRef md, char *buff)
{
  int value;
  bool isLiteralOp;

  if (LL_MDREF_kind(md) == MDRef_Constant) {
    strcpy(buff, mod->constants[LL_MDREF_value(md)]->data);
    return buff;
  }
  DEBUG_ASSERT(LL_MDREF_kind(md) == MDRef_SmallInt32, "not int");
  value = LL_MDREF_value(md);
  isLiteralOp = value & 1;
  value >>= 1;
  if (isLiteralOp)
    return ll_dw_op_to_name(value);
  sprintf(buff, "%d", value);
  return buff;
}

static void
emitComplexDIExpression(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                        unsigned mdi)
{
  unsigned i;
  unsigned cnt = mdnode->num_elems;
  char buff[32];
  
  emitRegularPrefix(out, mdi);
  fputs("!DIExpression(", out);
  for (i = 0; i < cnt; ++i) {
    if (i > 0)
      fputs(", ", out);
    fputs(decode_expression_op(mod, mdnode->elem[i], buff), out);
  }
  fputs(")\n", out);
}

static void
emitDIExpression(FILE *out, LLVMModuleRef mod, const LL_MDNode *mdnode,
                 unsigned mdi)
{
  if (useSpecialized(mod)) {
    if (mdnode->num_elems > 0) {
      emitComplexDIExpression(out, mod, mdnode, mdi);
      return;
    }
    emitSpec(out, mod, mdnode, mdi, Tmpl_DIExpression);
    return;
  }
  if (mdnode->num_elems > 0) {
    fputs("; ", out);
    emitComplexDIExpression(out, mod, mdnode, mdi);
  }
  emitUnspec(out, mod, mdnode, mdi, Tmpl_DIExpression);
}

static void
emitDIGlobalVariableExpression(FILE *out, LLVMModuleRef mod,
                               const LL_MDNode *mdnode, unsigned mdi)
{
  emitTmpl(out, mod, mdnode, mdi, Tmpl_DIGlobalVariableExpression);
}


static void
write_metadata_node(FILE *out, LLVMModuleRef module, MDNodeRef node,
                    unsigned mdi)
{
  const LL_MDClass mdClass = node->mdclass;

  DEBUG_ASSERT(mdClass < LL_MDClass_MAX, "mdclass out of bounds");
  mdDispTable[mdClass].method(out, module, node, mdi);
}

/**
   \brief Write out all the module metadata

   Write out all the so-called named metadata and then the regular metadata
 */
void
ll_write_metadata(FILE *out, LLVMModuleRef module)
{
  unsigned i;

  fprintf(out, "\n; Named metadata\n");
  for (i = 0; i < MD_NUM_NAMES; i++) {
    const LL_MDNode *node = module->named_mdnodes[i];
    if (node) {
      fprintf(out, "%s = ", get_metadata_name(i));
      write_mdnode_plain(out, module, node, /* omit_metadata_type = */ TRUE);
    }
  }

  fprintf(out, "\n; Metadata\n");
  for (i = 0; i < module->mdnodes_count; i++) {
    write_metadata_node(out, module, module->mdnodes[i], i + 1);
  }
}

void
ll_write_global_var_signature(FILE *out, LL_Value *variable)
{
  if (variable->mvtype == LL_GLOBAL) {
    fprintf(out, "global [0 x double]*");
  } else {
    fprintf(out, "%s*", variable->type_struct->str);
  }
  fprintf(out, " %s", variable->data);
}

/**
   \brief Write definition of the special <code>\@llvm.used</code> global
 */
void
ll_write_llvm_used(FILE *out, LLVMModuleRef module)
{
  unsigned i;

  if (!module->num_llvm_used)
    return;

  fprintf(out, "@llvm.used = appending global [%u x i8*] [\n  ",
          module->num_llvm_used);
  for (i = 0; i < module->num_llvm_used; i++) {
    LL_Value *ptr = module->llvm_used.values[i];
    if (i)
      fprintf(out, ",\n  ");
    fprintf(out, "%s %s", ptr->type_struct->str, ptr->data);
  }
  fprintf(out, "\n], section \"llvm.metadata\"\n");
}

/**
   \brief Write out definitions or declarations of global LL_Objects.
 
   If this function is called more than once, only the new objects added since
   the last call will be written.
 */
void
ll_write_global_objects(FILE *out, LLVMModuleRef module)
{
  LL_Object *object;

  for (object = module->first_global; object; object = object->next) {
    int addrspace = ll_get_pointer_addrspace(object->address.type_struct);

    fprintf(out, "%s =", object->address.data);

    /* TBD: [Linkage] [Visibility] [DLLStorageClass] [ThreadLocal]
     * [unnamed_addr] */

    if (addrspace && object->kind != LLObj_Alias)
      fprintf(out, " addrspace(%d)", addrspace);

    /* Linkage */
    if (object->linkage != LL_EXTERNAL_LINKAGE)
      fprintf(out, " %s", ll_get_linkage_string(object->linkage));

    /* Kind */
    switch (object->kind) {
    case LLObj_Global:
      fprintf(out, " global ");
      break;
    case LLObj_Const:
      fprintf(out, " constant ");
      break;
    case LLObj_Alias:
      fprintf(out, " alias ");
      break;
    default:
      interr("ll_write_global_objects: invalid global kind", object->kind, 4);
    }

    /* Print an initializer following the type. */
    switch (object->init_style) {
    case LLInit_Declaration:
      fprintf(out, "%s", object->type->str);
      break;
    case LLInit_Zero:
      fprintf(out, "%s zeroinitializer", object->type->str);
      break;
    case LLInit_ConstExpr:
      fprintf(out, "%s %s", object->init_data.const_expr->type_struct->str,
              object->init_data.const_expr->data);
      break;
    case LLInit_Function:
      /* Call the provided function pointer which will print out the
       * initializer with the leading type. */
      object->init_data.function(out, object);
      break;
    }

    /* Alignment */
    if (object->align_bytes)
      fprintf(out, ", align %u", object->align_bytes);

    /* TBD: [, section "name"] [, comdat ...] */
    fprintf(out, "\n");
  }

  /* Reset the list of global objects so this function can be called multiple
   * times without creating duplicates. */
  module->first_global = NULL;
  module->last_global = NULL;
}

void
ll_write_module(FILE *out, LL_Module *module)
{
  int i, j, met_idx;
  LL_Function *function = module->first;
  int num_functions;

  clear_prototypes();

  ll_write_module_header(out, module);

  fprintf(out, "; Begin User structs\n");
  ll_write_user_structs(out, module);
  fprintf(out, "; End User structs\n\n");

  fprintf(out, "; Begin module variables\n");
  /* HACKERY */
  for (i = 0; i < module->num_module_vars; i++) {
    const char *linkage_string;
    int addrspace;
    const char *type_str;
    const char *initializer;

    switch (module->module_vars.values[i]->linkage) {
    case LL_EXTERNAL_LINKAGE:
      initializer = "";
      break;
    case LL_COMMON_LINKAGE:
      initializer = "zeroinitializer";
      break;
    case LL_INTERNAL_LINKAGE:
      initializer = "zeroinitializer";
      break;
    case LL_NO_LINKAGE:
      initializer = "zeroinitializer";
      break;
    case LL_WEAK_LINKAGE:
      /* ICE */
      initializer = "";
      break;
    }
    linkage_string =
        ll_get_linkage_string(module->module_vars.values[i]->linkage);

    if (module->module_vars.values[i]->mvtype == LL_GLOBAL) {
      fprintf(out, "%s = external addrspace(%d) global [0 x double]\n",
              module->module_vars.values[i]->data,
              module->module_vars.values[i]->type_struct->addrspace);
    } else if (module->module_vars.values[i]->mvtype == LL_DEVICE) {
      unsigned int align_val ;

      align_val = module->module_vars.values[i]->align_bytes; 
      if (align_val == 0) {
        /* Enforce alignment to 16-bytes, if no alignment specified */
        align_val = 16;
      }
      fprintf(out, "%s = %s addrspace(1) global %s %s, align %u\n",
              module->module_vars.values[i]->data, linkage_string,
              module->module_vars.values[i]->type_struct->str, initializer,
              align_val);
    } else if (module->module_vars.values[i]->mvtype == LL_CONSTANT) {
      fprintf(out, "%s = %s addrspace(4) global %s %s, align 16\n",
              module->module_vars.values[i]->data, linkage_string,
              module->module_vars.values[i]->type_struct->str, initializer);
    } else if (module->module_vars.values[i]->linkage == LL_EXTERNAL_LINKAGE) {
      fprintf(out, "%s = %s addrspace(%d) global %s\n",
              module->module_vars.values[i]->data, linkage_string,
              (module->module_vars.values[i]->storage
                   ? module->module_vars.values[i]
                         ->storage->type_struct->sub_types[0]
                         ->addrspace
                   : 0),
              module->module_vars.values[i]->type_struct->str);
    } else {
      char align_str[80];
      switch (module->module_vars.values[i]->type_struct->data_type) {
      case LL_I1:
      case LL_I8:
      case LL_I16:
      case LL_I32:
      case LL_I64:
      case LL_FLOAT:
      case LL_DOUBLE:
      case LL_PTR:
        if (module->module_vars.values[i]->flags & VAL_IS_TEXTURE)
          linkage_string = "";
        break;
      default:
        break;
      }
      addrspace = 0;
      if (module->module_vars.values[i]->storage) {
        addrspace = module->module_vars.values[i]
                          ->storage->type_struct->sub_types[0]
                          ->addrspace;
      }
      align_str[0] ='\0';
      if (module->module_vars.values[i]->align_bytes)
        sprintf(align_str,", align %d", module->module_vars.values[i]->align_bytes);
      fprintf(out, "%s = %s addrspace(%d) global %s %s%s\n",
              module->module_vars.values[i]->data, linkage_string,
              addrspace, module->module_vars.values[i]->type_struct->str,
              initializer, align_str);
    }
  }
  ll_write_global_objects(out, module);
/* TODO: This needs to be enabled generally */
  ll_write_llvm_used(out, module);
  fprintf(out, "; End module variables\n\n");

  num_functions = 0;
  while (function) {
    ll_write_function(out, function);
    function = function->next;
    num_functions++;
  }
  write_prototypes(out, module);
  ll_write_metadata(out, module);
}
