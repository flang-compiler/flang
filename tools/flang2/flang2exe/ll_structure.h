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

#ifndef LL_STRUCTURE_H_
#define LL_STRUCTURE_H_

/**
   \file
   \brief LLVM bridge representation
 */

#include "universal.h"
#include "flang/ADT/hash.h"
#include <stdio.h>

/* clang-format off */

typedef enum LL_Op {
  LL_ADD,      LL_FADD,        LL_SUB,      LL_FSUB,         LL_MUL,
  LL_FMUL,     LL_UDIV,        LL_SDIV,     LL_UREM,         LL_SREM,
  LL_FDIV,     LL_OR,          LL_XOR,      LL_ASHR,         LL_LSHR,
  LL_SHL,      LL_AND,         LL_STORE,    LL_LOAD,         LL_SEXT,
  LL_ZEXT,     LL_TRUNC,       LL_SITOFP,   LL_UITOFP,       LL_FPTOSI,
  LL_FPTOUI,   LL_FPTRUNC,     LL_FPEXT,    LL_CALL,         LL_RET,
  LL_ICMP,     LL_FCMP,        LL_BR,       LL_UBR,          LL_SELECT,
  LL_GEP,      LL_BITCAST,     LL_INTTOPTR, LL_PTRTOINT,     LL_ALLOCA,
  LL_TEXTCALL, LL_UNREACHABLE, LL_SWITCH,   LL_EXTRACTVALUE, LL_INSERTVALUE,
  LL_ATOMICRMW, LL_CMPXCHG, LL_NONE
} LL_Op;

/* clang-format on */

enum LL_ModuleVarType {
  LL_DEFAULT = 0x1,
  LL_GLOBAL = 0x1 << 1,
  LL_SHARED = 0x1 << 2,
  LL_LOCAL = 0x1 << 3,
  LL_DEVICE = 0x1 << 4,
  LL_CONSTANT = 0x1 << 5,
};

enum LL_LinkageType {
  LL_INTERNAL_LINKAGE = 1,
  LL_COMMON_LINKAGE,
  LL_EXTERNAL_LINKAGE,
  LL_WEAK_LINKAGE,
  LL_NO_LINKAGE
};

enum LL_BaseDataType {
  LL_NOTYPE = 0,
  LL_LABEL,
  LL_METADATA,
  LL_VOID,
  LL_I1,
  LL_I8,
  LL_I16,
  LL_I24,
  LL_I32,
  LL_I40,
  LL_I48,
  LL_I56,
  LL_I64,
  LL_I128,
  LL_I256,
  LL_HALF,      /**< IEEE half precision floating point. */
  LL_FLOAT,     /**< IEEE single precision floating point. */
  LL_DOUBLE,    /**< IEEE double precision floating point. */
  LL_FP128,     /**< IEEE quad precision floating point. */
  LL_X86_FP80,  /**< Intel x87 80-bit floating point. */
  LL_PPC_FP128, /**< PowerPC style double-double. */
  LL_PTR,
  LL_ARRAY,
  LL_VECTOR,
  LL_STRUCT,
  LL_FUNCTION
};

typedef enum LL_AddressSpace { LL_AddrSp_Default = 0 } LL_AddressSpace_t;

/**
   \brief Calling conventions.
   See the LLVM source file include/llvm/IR/CallingConv.h for the complete list.
 */
enum LL_CallConv {
  LL_CallConv_C = 0, /**< This is the default. */
  LL_CallConv_Fast = 8,
  LL_CallConv_Cold = 9,

  /* X86 */
  LL_CallConv_X86_StdCall = 64,
  LL_CallConv_X86_FastCall = 65,
  LL_CallConv_X86_ThisCall = 70,
  LL_CallConv_X86_VectorCall = 80,

  /* ARM */
  LL_CallConv_APCS = 66,
  LL_CallConv_AAPCS = 67,
  LL_CallConv_AAPCS_VFP = 68,

  /* PTX */
  LL_CallConv_PTX_Kernel = 71,
  LL_CallConv_PTX_Device = 72,

  /* SPIR */
  LL_CallConv_SPIR_FUNC = 75,
  LL_CallConv_SPIR_KERNEL = 76
};

/**
   \brief Supported LLVM IR versions.

   We can generate LLVM IR for multiple versions of LLVM. The numeric value of
   these version enumerators must match the x-bits set by the rcfiles.

   The CPU target version is set by -x 249 from rcfiles/llvmrc.
 */
typedef enum LL_IRVersion {
  LL_Version_3_1 = 31,
  LL_Version_3_2 = 32,
  LL_Version_3_4 = 34,
  LL_Version_3_5 = 35,
  LL_Version_3_6 = 36,
  LL_Version_3_7 = 37,
  LL_Version_3_8 = 38,
  LL_Version_3_9 = 39,
  LL_Version_4_0 = 40,
  LL_Version_5_0 = 50,
  LL_Version_trunk = 1023
} LL_IRVersion;

LL_IRVersion get_llvm_version(void);

typedef enum LL_DWARFVersion {
  LL_DWARF_Version_2,
  LL_DWARF_Version_3,
  LL_DWARF_Version_4
} LL_DWARFVersion;

/**
   \brief LLVM IR Feature Vector.

   IR versions are translated to a feature vector that controls the shape of the
   generated IR. Code generation should always check the feature vector instead
   of comparing version numbers directly.
 */
typedef struct LL_IRFeatures_ {
  LL_IRVersion version : 10;
  LL_DWARFVersion dwarf_version : 4; /**< DWARF Version */
  unsigned is_nvvm : 1;              /**< Targeting NVVM IR for CUDA. */
  unsigned is_spir : 1;              /**< Targeting SPIR for OpenCL. */
  /** Version number for debug info metadata. Note that the version number
      sequences are different with/without versioned_dw_tag. */
  unsigned debug_info_version : 8;
} LL_IRFeatures;

#if HAVE_INLINE
/* modern C compilers support 'inline' keyword */

INLINE static bool
ll_feature_use_addrspacecast(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_4;
}

/**
   \brief Use global aliases to refer to offset globals in metadata
 */
INLINE static bool
ll_feature_debug_info_global_aliases(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_4;
}

/**
   \brief Use the pre-3.4 layout for debug info mdnodes
 */
INLINE static bool
ll_feature_debug_info_pre34(const LL_IRFeatures *feature)
{
  return feature->version < LL_Version_3_4;
}

/**
   \brief Need NVVM version?
 */
INLINE static bool
ll_feature_emit_nvvmir_version(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_4;
}

/**
   \brief Encode LLVMDebugVersion in DW_TAGs in debug info metadata
 */
INLINE static bool
ll_feature_versioned_dw_tag(const LL_IRFeatures *feature)
{
  return feature->version <= LL_Version_3_5;
}

INLINE static bool
ll_feature_omit_metadata_type(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_6;
}

/**
   \brief Use specialized <tt> !MDLocation(...) </tt> metadata node syntax.
 */
INLINE static bool
ll_feature_debug_info_mdlocation(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_6;
}

/**
   \brief Alias syntax is <tt> [flags] alias \i AliaseeTy \i @Aliasee </tt>
   instead of <tt> alias [flags] \i AliaseeTy \i @Aliasee </tt>
 */
INLINE static bool
ll_feature_alias_flags_first(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Emit a call instruction with function signature instead of a pointer
   type (to the called function)
 */
INLINE static bool
ll_feature_emit_func_signature_for_call(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Local variable has line number and argument number in the same field
 */
INLINE static bool
ll_feature_dbg_local_variable_embeds_argnum(const LL_IRFeatures *feature)
{
  return feature->version < LL_Version_3_7;
}

/**
   \brief Emit an explicit type on gep and load instructions.
 */
INLINE static bool
ll_feature_explicit_gep_load_type(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   Metadata function arguments require full metadata structure:
   <tt> !{...} </tt>
 */
INLINE static bool
ll_feature_metadata_args_struct(const LL_IRFeatures *feature)
{
  return feature->version < LL_Version_3_7;
}

/**
   \brief Use specialized <tt>!MD*(...)</tt> metadata nodes for debug?

   In LLVM 3.7 and later, use specialized debug info formats.
 */
INLINE static bool
ll_feature_use_specialized_mdnodes(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Need explicit file descriptions instead of references to them
 */
INLINE static bool
ll_feature_debug_info_need_file_descriptions(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Debug intrinsics require an extra argument
 */
INLINE static bool
ll_feature_dbg_declare_needs_expression_md(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Before LLVM 3.7, personality signatures were on landingpads.
   \return true iff old style c++ exception personality placement

   With LLVM 3.7, the personality appends to the function definition.
 */
INLINE static bool
ll_feature_eh_personality_on_landingpad(const LL_IRFeatures *feature)
{
  return feature->version < LL_Version_3_7;
}

/**
    \brief Metadata types start with \c DI instead of \c MD
 */
INLINE static bool
ll_feature_debug_info_DI_syntax(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Debug information: subrange node needs element count instead of
   index's upper bound. Also -1 is used to show that the range is empty
 */
INLINE static bool
ll_feature_debug_info_subrange_needs_count(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_7;
}

/**
   \brief Version 3.8 debug metadata
 */
INLINE static bool
ll_feature_debug_info_ver38(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_8;
}

INLINE static bool
ll_feature_use_distinct_metadata(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_8;
}

/**
   \brief Version 3.9 debug metadata
 */
INLINE static bool
ll_feature_subprogram_not_in_cu(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_3_9;
}

/**
   \brief Do the debug links point from globals to metadata?

   In LLVM 4.0, the direction of the link was reversed to point from globals to
   their metadata descriptions rather than the other way around.
 */
INLINE static bool
ll_feature_from_global_to_md(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_4_0;
}

/** \brief Use the LLVM 5.0 DIExpression */
INLINE static bool
ll_feature_use_5_diexpression(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_5_0;
}

/** \brief Don't bother with \c !DIModule in earlier LLVMs */
INLINE static bool
ll_feature_create_dimodule(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_5_0;
}

/** \brief Use PGI's LLVM debug metadata extensions */
INLINE static bool
ll_feature_has_diextensions(const LL_IRFeatures *feature)
{
  return false;
}

INLINE static bool
ll_feature_no_file_in_namespace(const LL_IRFeatures *feature)
{
  return feature->version >= LL_Version_5_0;
}

#else /* !HAVE_INLINE */
/* support a dusty deck C compiler */

#define ll_feature_use_addrspacecast(f) ((f)->version >= LL_Version_3_4)
#define ll_feature_debug_info_global_aliases(f) ((f)->version >= LL_Version_3_4)
#define ll_feature_debug_info_pre34(f) ((f)->version < LL_Version_3_4)
#define ll_feature_emit_nvvmir_version(f) ((f)->version >= LL_Version_3_4)
#define ll_feature_versioned_dw_tag(f) ((f)->version <= LL_Version_3_5)
#define ll_feature_omit_metadata_type(f) ((f)->version >= LL_Version_3_6)
#define ll_feature_debug_info_mdlocation(f) ((f)->version >= LL_Version_3_6)
#define ll_feature_alias_flags_first(f) ((f)->version >= LL_Version_3_7)
#define ll_feature_emit_func_signature_for_call(f) \
  ((f)->version >= LL_Version_3_7)
#define ll_feature_dbg_local_variable_embeds_argnum(f) \
  ((f)->version < LL_Version_3_7)
#define ll_feature_explicit_gep_load_type(f) ((f)->version >= LL_Version_3_7)
#define ll_feature_metadata_args_struct(f) ((f)->version < LL_Version_3_7)
#define ll_feature_use_specialized_mdnodes(f) ((f)->version >= LL_Version_3_7)
#define ll_feature_debug_info_need_file_descriptions(f) \
  ((f)->version >= LL_Version_3_7)
#define ll_feature_dbg_declare_needs_expression_md(f) \
  ((f)->version >= LL_Version_3_7)
#define ll_feature_eh_personality_on_landingpad(f) \
  ((f)->version < LL_Version_3_7)
#define ll_feature_debug_info_DI_syntax(f) ((f)->version >= LL_Version_3_7)
#define ll_feature_debug_info_subrange_needs_count(f) \
  ((f)->version >= LL_Version_3_7)
#define ll_feature_debug_info_ver38(f) ((f)->version >= LL_Version_3_8)
#define ll_feature_use_distinct_metadata(f) ((f)->version >= LL_Version_3_8)
#define ll_feature_subprogram_not_in_cu(f) ((f)->version >= LL_Version_3_9)
#define ll_feature_from_global_to_md(f) ((f)->version >= LL_Version_4_0)
#define ll_feature_use_5_diexpression(f) ((f)->version >= LL_Version_5_0)
#define ll_feature_create_dimodule(f) ((f)->version >= LL_Version_5_0)
#define ll_feature_has_diextensions(f) (false)
#define ll_feature_no_file_in_namespace(f) ((f)->version >= LL_Version_5_0)

#endif

unsigned ll_feature_dwarf_version(const LL_IRFeatures *feature);

struct LL_Module;
struct LL_Object;

typedef struct LL_Module *LLVMModuleRef;

/**
   \brief LL_Type represents an LLVM type.

   These structs are uniqued, so they should never be modified.
 */
typedef const struct LL_Type_ {
  LLVMModuleRef module;
  const char *str;
  enum LL_BaseDataType data_type;
  unsigned flags;

  /**
     The sub_types field depends on the value of \c data_type
     \li \c LL_PTR      [0] = pointee
     \li \c LL_ARRAY    [0] = element type, sub_elements = \#elements
     \li \c LL_VECTOR   [0] = lane type, sub_elements = \#lanes
     \li \c LL_STRUCT   [0..sub_elements-1] = members
     \li \c LL_FUNCTION [0] = return type, [1..sub_elements-1] = argument types
  */
  const struct LL_Type_ **sub_types;

  /**
     For structure types, keep an optional array of the byte-offset of each
     member of the struct.
  */
  unsigned *sub_offsets;

  char *sub_padding;
  BIGUINT64 sub_elements;
  int addrspace;
} LL_Type;

/* Flags for LL_Type. */
#define LL_TYPE_IS_VARARGS_FUNC 0x01
#define LL_TYPE_IS_PACKED_STRUCT 0x02
#define LL_TYPE_IS_NAMED_STRUCT 0x04

/**
   \brief LLVM Metadata

   Metadata nodes are constant manifestly typed tuples of references to other
   metadata nodes, metadata strings, or LLVM constants. The values that can be
   stored in a metadata node are represented as an array of LL_MDRef references.

   Metadata nodes and strings are normally uniqued to save space, but it is
   possible to create distinct metadata nodes that don't participate in
   uniquing.

   Metadata can appear as annotations on instructions, or as module-global
   named metadata.

   LL_MDRef is a compact manifestly typed reference to the kind of data that can
   be stored in a metadata node. An LL_MDRef can refer to:

   \li A numbered metadata node, or \c NULL.
   \li A metadata string.
   \li A constant LLVM value.

   LL_MDRef is passed around by value. It should be treated as an opaque type.
   It cannot be transferred between different LL_Module instances.
 */
typedef unsigned LL_MDRef;

#define LL_MDREF_kind(md) ((md) & ((1 << 3) - 1))
#define LL_MDREF_value(md) ((md) >> 3)
#define LL_MDREF_ctor(k, v) (((v) << 3) | LL_MDREF_kind(k))
#define LL_MDREF_INITIALIZER(key, val) LL_MDREF_ctor(key, val)
#define LL_MDREF_IS_NULL(md) ((md) == 0)

/**
   \brief Bucket list for object to \c !dbg metadata

   Sized to fit in 64 bytes to keep in a cache line (or 2)
 */
typedef struct LL_ObjToDbgList {
#define LL_ObjToDbgBucketSize 13
  struct LL_ObjToDbgList *next;           ///< pointer to next bucket
  LL_MDRef refs[LL_ObjToDbgBucketSize];   ///< bucket contents
  unsigned used : 4;                      ///< count of objs in bucket
  unsigned marks : LL_ObjToDbgBucketSize; ///< marker bits
} LL_ObjToDbgList;

/**
   \brief Iterator for iterating over a LL_ObjToDbgList
 */
typedef struct LL_ObjToDbgListIter {
  LL_ObjToDbgList *list;
  unsigned pos;
} LL_ObjToDbgListIter;

/**
   \brief Metadata node types
 */
enum LL_MDRef_Kind {
  MDRef_Node,      /**< Value=0 -> null, otherwise MDNode !n */
  MDRef_String,    /**< Value is an index into module->mdstrings. */
  MDRef_Constant,  /**< Value is an index into module->constants. */
  MDRef_SmallInt1, /**< Valus is an i1 constant (0 or 1). */
  /** Value is a small unsigned i32 value (within the range of mdref.value). */
  MDRef_SmallInt32,
  /** Value is a small unsigned i64 value (within the range of mdref.value). */
  MDRef_SmallInt64
};

/**
   \brief Classes for specialized metadata nodes.

   Plain metadata nodes are simply printed as !{ ... }, but starting with LLVM
   3.6, metadata nodes can be specialized to contain a certain class of data,
   and they look like: !MDLocation(line: 2900, column:42, ...).

   The metadata node classes should also be used when targeting LLVM < 3.6.
   They will be used to annotate the emitted plain metadata nodes in comments.
 */
typedef enum LL_MDClass {
  LL_PlainMDNode, /**< \e not a DIxxx metadata */
  LL_DICompileUnit,
  LL_DIFile,
  LL_DIBasicType,
  LL_DISubroutineType,
  LL_DIDerivedType,
  LL_DICompositeType,
  LL_DIFortranArrayType,
  LL_DISubRange,
  LL_DIFortranSubrange,
  LL_DIEnumerator,
  LL_DITemplateTypeParameter,
  LL_DITemplateValueParameter,
  LL_DINamespace,
  LL_DIModule,
  LL_DIGlobalVariable,
  LL_DISubprogram,
  LL_DILexicalBlock,
  LL_DILexicalBlockFile,
  LL_DILocation,
  LL_DILocalVariable,
  LL_DIExpression,
  LL_DIObjCProperty,
  LL_DIImportedEntity,
  LL_DIGlobalVariableExpression,
  LL_DIBasicType_string, /* deprecated */
  LL_DIStringType,
  LL_MDClass_MAX /**< must be last value and < 64 (6 bits) */
} LL_MDClass;

/**
   The internal representation of metadata nodes shouldn't be needed outside
   ll_structure.c and ll_write.c.
 */
typedef struct LL_MDNode {
  unsigned num_elems : 24;
  LL_MDClass mdclass : 6;
  unsigned is_distinct : 1;
  unsigned is_flexible : 1;
  LL_MDRef elem[];
} LL_MDNode;

typedef enum LL_DW_OP_t {
  LL_DW_OP_NONE, /**< bogus value */
  LL_DW_OP_deref,
  LL_DW_OP_plus,
  LL_DW_OP_LLVM_fragment,
  LL_DW_OP_swap,
  LL_DW_OP_xderef,
  LL_DW_OP_stack_value,
  LL_DW_OP_constu,
  LL_DW_OP_plus_uconst,
  LL_DW_OP_int,
  LL_DW_OP_MAX /**< must be last value */
} LL_DW_OP_t;

INLINE static bool
ll_dw_op_ok(LL_DW_OP_t op)
{
  return (op > LL_DW_OP_NONE) && (op < LL_DW_OP_MAX);
}

/**
   \brief Named module-level metadata.

   We support a predefined set of well-known metadata names. When adding a new
   name here, also update get_metadata_name() in ll_write.c.
 */
enum LL_MDName {
  /** Module flags, defined in the LLVM Language Reference Manual. */
  MD_llvm_module_flags,
  /** DWARF compilation unit descriptors, from "Source Level Debugging with
      LLVM". */
  MD_llvm_dbg_cu,
  MD_llvm_linker_options,
  MD_opencl_kernels,   /**< SPIR */
  MD_nvvm_annotations, /**< CUDA */
  MD_nvvmir_version,   /**< CUDA */
  MD_NUM_NAMES         /**< Must be last. */
};

typedef struct LL_Value {
  const char *data;
  struct LL_Value *storage;
  LL_Type *type_struct;
  enum LL_ModuleVarType mvtype;
  enum LL_LinkageType linkage;
  int flags;
  /* FIXME: This doesn't belong in LL_Value, it would be better in LL_Variable.
   */
  int dbg_sptr;
  int align_bytes; /* For variables definition specify alignment in bytes */
  LL_MDRef dbg_mdnode;
} LL_Value;

#define VAL_IS_GLOBAL_OFFSET 0x1
#define VAL_IS_TEXTURE 0x2
/* This value is a formal function parameter. */
#define VAL_IS_PARAM 0x4
/* The flags below are only valid when VAL_IS_PARAM is set. */
#define VAL_IS_BYVAL_PARAM 0x08
#define VAL_IS_NOALIAS_PARAM 0x10
#define VAL_IS_KERNEL_REF 0x1000

/**
   \brief LL_Object represents an object stored in memory.

   An object belongs to an address space, and it always has an address.
   LL_Object can represent different kinds of objects.
 */
enum LL_ObjectKind {
  LLObj_Global, /**< A global variable */
  LLObj_Const,  /**< A global constant */
  LLObj_Alias,  /**< An alias referencing part of on other object */
  LLObj_Local   /**< A local variable on the stack */
};

enum LL_ObjectInitStyle {
  /** Declaration of an object that is initialized in another module. */
  LLInit_Declaration,

  LLInit_Zero, /**< Object is initialized with all zeros. */

  /** Object is initialized with a constant expression in
      LL_Object::initializer */
  LLInit_ConstExpr,

  /** Object's initializer is printed by the provided function pointer. */
  LLInit_Function
};

/**
   \brief Ptr to function used to print out the initializer for an LL_Object

   The function should print both the type and the value of the initializer. It
   should not print a newline after the initializer.
 */
typedef void (*LL_PrintInitializerFunction)(FILE *out,
                                            struct LL_Object *object);

/**
   \brief Data structure representing an LLVM Object

   Warning: we don't support linkage types yet, that is a drawback in some cases
 */
typedef struct LL_Object {

  /** The address and name of the object. Also encodes the address space. */
  LL_Value address;

  /** The type of this object, without address space. When targeting LLVM
      versions with typed pointers, this type should match the pointee type of
      address above. */
  LL_Type *type;

  /** Alignment of object in bytes, or 0 to use LLVM's default alignment. */
  unsigned align_bytes;

  /** Linkage type for this object.  The \c linkage field should be removed from
      LL_Value, in favor of this field here. */
  enum LL_LinkageType linkage;

  enum LL_ObjectKind kind;
  enum LL_ObjectInitStyle init_style;

  union {
    LL_Value *const_expr;
    LL_PrintInitializerFunction function;
  } init_data;

  /** Symbol table reference. This value isn't used by ll_structure.c or
      ll_write.c. It could be used by the init function to locate the data for
      the object initializer.

      Note that the lifetimes of Fortran symbol table entries are shorter than
      the lifetime of an LL_Module. */
  int sptr;

  /** All the global objects in a module are kept in a linked list. */
  struct LL_Object *next;
} LL_Object;

typedef struct LL_Symbols_ {
  LL_Value **values;
  unsigned int num_values;
} LL_Symbols;

typedef struct LL_ManagedMallocs_ {
  void *storage;
  struct LL_ManagedMallocs_ *next;
} LL_ManagedMallocs;

typedef struct LL_Instruction_ {
  enum LL_Op op;
  char *comment;
  LL_Value **operands;          /* Get off ground by limiting operands */
  struct LL_Instruction_ *next; /**< Next instruction in basic block */
  int num_operands;
  LL_MDRef dbg_line_op; /**< scope debug info attached to instruction */
  int flags;            /**< HACK: use to record this as a within module call */
} LL_Instruction;

#define IN_MODULE_CALL 0x1
#define INST_CANCELED 0x2
#define INST_VOLATILE 0x4

typedef struct LL_BasicBlock_ {
  char *name;
  LL_Instruction *first;
  LL_Instruction *last;
  struct LL_BasicBlock_ *next; /**< Next basic block in function */
} LL_BasicBlock;

typedef struct LL_Function_ {
  const char *name;
  LL_Type *return_type;
  const char *attribute;
  LL_BasicBlock *first;
  LL_BasicBlock *last;
  LL_Value **arguments;
  unsigned int num_args;
  struct LL_Function_ *next; /**< Next function in module */
  struct LL_Symbols_ local_vars;
  unsigned int num_locals;
  int launch_bounds;
  int is_kernel;
  enum LL_LinkageType linkage;
  const char *calling_convention;

  /** Linked list of local variables. */
  LL_Object *first_local, *last_local;

  /** Set of names used for local values in this function. This does not include
      values which are simply numbered (%1, %2, ...). */
  hashset_t used_local_names;
} LL_Function;

/* Debug info state associated with a compilation unit. See lldebug.c. */
struct LL_DebugInfo;
typedef struct LL_DebugInfo LL_DebugInfo;

struct LL_ABI_Info_;

/**
   \brief LLVM Module proxy
 */
typedef struct LL_Module {
  const char *module_name;
  const char *target_triple;
  const char *datalayout_string;
  LL_IRFeatures ir;
  LL_DebugInfo *debug_info;
  LL_Function *first;
  LL_Function *last;
  LL_ManagedMallocs *first_malloc;
  hashset_t anon_types;
  struct LL_Symbols_ module_vars;
  struct LL_Symbols_ user_structs;
  hashmap_t user_structs_byid;
  hashset_t used_type_names;
  hashset_t used_global_names;
  unsigned num_module_vars;
  unsigned num_user_structs;
  unsigned written_user_structs;
  int num_refs;
  LL_Value **extern_func_refs; /**< reference to external func */

  /** Interned constants: Unmanaged array of managed LL_Values. */
  LL_Value **constants;
  unsigned constants_count;
  unsigned constants_alloc;
  hashmap_t constants_map;

  /** Interned metadata strings: Unmanaged malloc'ed array of malloced
      strings. */
  const char **mdstrings;
  unsigned mdstrings_count;
  unsigned mdstrings_alloc;
  hashmap_t mdstrings_map;

  /** Numbered metadata nodes. <tt> MDNode !1 </tt> is in <tt> mdnodes[0] </tt>
      and so on. */
  LL_MDNode **mdnodes;
  unsigned mdnodes_count;
  unsigned mdnodes_alloc;
  hashmap_t mdnodes_map;
  hashmap_t mdnodes_fwdvars;

  /** Named metadata in the module indexed by <tt>enum LL_MDName</tt>. Array of
      unmanaged nodes */
  LL_MDNode *named_mdnodes[MD_NUM_NAMES];
  LL_MDRef omnipotentPtr;
  LL_MDRef unrefPtr;
  LL_MDRef loop_md;

  /** Contents of the special global \c @llvm.used. List of pointers or constant
      exprs. */
  LL_Symbols llvm_used;
  unsigned num_llvm_used;

  hashmap_t globalDebugMap; /**< sptr globalVar -> LL_MDRef */

  /** Linked list of objects with global scope in this module. This list only
      contains objects that haven't been printed yet. The memory of LL_Objects
      is managed by LL_ManagedMallocs. */
  LL_Object *first_global;
  LL_Object *last_global;

} LL_Module;

/**
   \brief Map from function (keyed by linker name) to ABI
 */
typedef struct LL_FnProto_ {
  char *fn_name;            /**< Key in map */
  struct LL_ABI_Info_ *abi; /**< This is used to emit the function signature */
  unsigned has_defined_body : 1; /**< True if this fn has a body */
  unsigned is_weak : 1;          /**< True if this fn has the attribute weak */
  /** Only set if this is an intrinsic (has a predefined func decl string) */
  char *intrinsic_decl_str;
  /** Maintain a list (easier to read/debug ll output) */
  struct LL_FnProto_ *next;
} LL_FnProto;

void ll_proto_init(void);
const char *ll_proto_key(int func_sptr);
LL_FnProto *ll_proto_add(const char *fnname, struct LL_ABI_Info_ *abi);
LL_FnProto *ll_proto_add_sptr(int sptr, struct LL_ABI_Info_ *abi);
void ll_proto_set_abi(const char *fnname, struct LL_ABI_Info_ *abi);
struct LL_ABI_Info_ *ll_proto_get_abi(const char *fnname);
void ll_proto_set_defined_body(const char *fnname, LOGICAL has_defined);
LOGICAL ll_proto_has_defined_body(const char *fnname);
void ll_proto_set_weak(const char *fnname, LOGICAL is_weak);
LOGICAL ll_proto_is_weak(const char *fnname);
void ll_proto_set_intrinsic(const char *fnname, const char *intrinsic_decl_str);

/**
   \brief Iterate across all entries in the map and callback the handler.
 */
typedef void (*LL_FnProto_Handler)(LL_FnProto *proto);

void ll_proto_iterate(LL_FnProto_Handler callback);
void ll_proto_dump(void);

LLVMModuleRef ll_create_module(const char *module_name,
                               const char *target_triple,
                               enum LL_IRVersion llvm_ir_version);

LL_Function *ll_create_function(LLVMModuleRef, const char *, LL_Type *, int,
                                int, const char *, enum LL_LinkageType);
LL_Function *ll_create_function_from_type(LL_Type *func_type, const char *name);
void ll_destroy_function(LL_Function *function);

void ll_set_function_num_arguments(struct LL_Function_ *, int);

void ll_create_sym(struct LL_Symbols_ *, int, LL_Value *);

LL_Value *ll_named_struct_type_exists(LLVMModuleRef module, int id,
                                      const char *format, ...);
LL_Type *ll_create_named_struct_type(LLVMModuleRef, int id, LOGICAL unique,
                                     const char *format, ...);
void ll_remove_struct_type(LLVMModuleRef, int id);
LL_Type *ll_get_struct_type(LLVMModuleRef, int id, int required);
void ll_set_struct_body(LL_Type *ctype, LL_Type *const *elements,
                        unsigned *const offsets, char *pads,
                        unsigned num_elements, int is_packed);

LL_Type *ll_create_anon_struct_type(LLVMModuleRef, LL_Type *elements[],
                                    unsigned num_elements, bool is_packed);

LL_Type *ll_create_function_type(LLVMModuleRef, LL_Type *args[],
                                 unsigned num_args, int is_varargs);

void ll_reset_module_types(LLVMModuleRef);
void ll_destroy_module(LLVMModuleRef);

LL_Value *ll_create_pointer_value(LLVMModuleRef, enum LL_BaseDataType,
                                  const char *, int addrspace);

LL_Value *ll_create_value_from_type(LLVMModuleRef, LL_Type *, const char *);

LL_Value *ll_create_array_value_from_type(LLVMModuleRef, LL_Type *,
                                          const char *, int addrspace);

LL_Value *ll_create_pointer_value_from_type(LLVMModuleRef, LL_Type *,
                                            const char *, int addrspace);

LL_Type *ll_create_basic_type(LLVMModuleRef, enum LL_BaseDataType,
                              int addrspace);
LL_Type *ll_create_int_type(LLVMModuleRef, unsigned bits);
LL_Type *ll_get_pointer_type(LL_Type *pointee);
LL_Type *ll_get_array_type(LL_Type *elem_type, BIGUINT64 num_elements,
                           int addrspace);
LL_Type *ll_get_vector_type(LL_Type *elem_type, unsigned num_elements);
LL_Type *ll_get_addrspace_type(LL_Type *type, int addrspace);

int ll_get_pointer_addrspace(LL_Type *ptr);
const char *ll_get_str_type_for_basic_type(enum LL_BaseDataType type);

const char *ll_get_calling_conv_str(enum LL_CallConv);
ISZ_T ll_type_bytes(LL_Type *type);
ISZ_T ll_type_bytes_unchecked(LL_Type *type);
unsigned ll_type_int_bits(LL_Type *type);
LL_Type *ll_type_array_elety(LL_Type *arrTy);
int ll_type_is_pointer_to_function(LL_Type *ty);
LL_Type *ll_type_array_elety(LL_Type *ty);
int ll_type_is_fp(LL_Type *ty);
int ll_type_is_mem_seq(LL_Type *ty);

/*
 * Interned constants.
 *
 * LL_Values representing LLVM constants can be reused everywhere in a module
 * to save memory. These interned LL_Values must never be modified since they
 * are shared.
 */

LL_Value *ll_get_const_int(LLVMModuleRef, unsigned bits, long long value);

LL_Value *ll_get_const_gep(LLVMModuleRef, LL_Value *ptr, unsigned num_idx, ...);
LL_Value *ll_get_const_bitcast(LLVMModuleRef, LL_Value *value, LL_Type *type);
LL_Value *ll_get_const_addrspacecast(LLVMModuleRef, LL_Value *value,
                                     LL_Type *type);

/* Metadata */

#if HAVE_INLINE
/** \brief Get an LL_MDRef representing null. */
INLINE static LL_MDRef
ll_get_md_null(void)
{
  return LL_MDREF_INITIALIZER(MDRef_Node, 0);
}
#else /* !HAVE_INLINE */
#define ll_get_md_null() LL_MDREF_INITIALIZER(MDRef_Node, 0)
#endif /* HAVE_INLINE */

LL_MDRef ll_get_md_i1(int value);
LL_MDRef ll_get_md_i32(LLVMModuleRef, int value);
LL_MDRef ll_get_md_i64(LLVMModuleRef, long long value);

LL_MDRef ll_get_md_string(LLVMModuleRef, const char *str);
LL_MDRef ll_get_md_rawstring(LLVMModuleRef, const void *str, size_t length);
LL_MDRef ll_get_md_value(LLVMModuleRef, LL_Value *value);
unsigned ll_reserve_md_node(LLVMModuleRef module);
LL_MDRef ll_get_md_node(LLVMModuleRef, LL_MDClass mdclass,
                        const LL_MDRef *elems, unsigned nelems);
void ll_set_md_node(LLVMModuleRef module, unsigned mdNum, LL_MDNode *node);
LL_MDRef ll_create_distinct_md_node(LLVMModuleRef, LL_MDClass mdclass,
                                    const LL_MDRef *elems, unsigned nelems);
LL_MDRef ll_create_flexible_md_node(LLVMModuleRef);
void ll_extend_md_node(LLVMModuleRef, LL_MDRef flexnode, LL_MDRef elem);
void ll_update_md_node(LLVMModuleRef, LL_MDRef node_to_update,
                       unsigned elem_index, LL_MDRef elem);
void ll_set_named_md_node(LLVMModuleRef, enum LL_MDName name,
                          const LL_MDRef *elems, unsigned nelems);
void ll_extend_named_md_node(LLVMModuleRef, enum LL_MDName name, LL_MDRef elem);

void ll_append_llvm_used(LLVMModuleRef module, LL_Value *ptr);

LL_Object *ll_create_global_alias(LL_Value *aliasee_addr, const char *format,
                                  ...);
const char *ll_create_local_name(LL_Function *function, const char *format,
                                 ...);
LL_Object *ll_create_local_object(LL_Function *function, LL_Type *type,
                                  unsigned align_bytes, const char *format,
                                  ...);

void write_mdref(FILE *out, LLVMModuleRef module, LL_MDRef rmdref,
                 int omit_metadata_type);
void ll_add_global_debug(LLVMModuleRef module, int sptr, LL_MDRef mdnode);
LL_MDRef ll_get_global_debug(LLVMModuleRef module, int sptr);
char *get_llvm_name(int sptr); /* see llassem*.c */

INLINE static LL_ObjToDbgList *
llObjtodbgCreate(void)
{
  return (LL_ObjToDbgList *)calloc(sizeof(LL_ObjToDbgList), 1);
}

INLINE static void
llObjtodbgFirst(LL_ObjToDbgList *ods, LL_ObjToDbgListIter *iter)
{
  iter->list = ods;
  iter->pos = 0;
}

INLINE static bool
llObjtodbgAtEnd(LL_ObjToDbgListIter *iter)
{
  LL_ObjToDbgList *l = iter->list;
  return (!l) || ((!l->next) && (iter->pos == l->used));
}

INLINE static void
llObjtodbgNext(LL_ObjToDbgListIter *iter)
{
  iter->pos++;
  if ((iter->pos == LL_ObjToDbgBucketSize) && iter->list->next) {
    iter->list = iter->list->next;
    iter->pos = 0;
  }
}

INLINE static LL_MDRef
llObjtodbgGet(LL_ObjToDbgListIter *iter)
{
  return iter->list->refs[iter->pos];
}

void llObjtodbgPush(LL_ObjToDbgList *odl, LL_MDRef md);
void llObjtodbgFree(LL_ObjToDbgList *ods);
void add_linker_directives(LLVMModuleRef module);

#endif
