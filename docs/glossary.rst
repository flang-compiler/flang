.. _flang-glossary:

**************
Flang glossary
**************

+------+--------------------------------------------------------------------------------------------+
| Name | Explanation                                                                                |
+======+============================================================================================+
| ADT  | Abstract Data Types.                                                                       |
+------+--------------------------------------------------------------------------------------------+
| AST  | Abstract Syntax Tree.                                                                      |
+------+--------------------------------------------------------------------------------------------+
| BE   | (used heavily in code and comments, short for "backend"                                    |
+------+--------------------------------------------------------------------------------------------+
| FE   | (used heavily in code and comments, short for "frontend"                                   |
+------+--------------------------------------------------------------------------------------------+
| ILM  | Intermediate Language Macros.                                                              |
|      | Representation of executable statements.                                                   |
|      | This is an output of flang1 and input for flang2.                                          |
+------+--------------------------------------------------------------------------------------------+
| ILI  | Intermediate Language Instructions.                                                        |
|      | The IR used by flang2 for optimisations.                                                   |
+------+--------------------------------------------------------------------------------------------+
| ILT  | Terminal node of an ILI statement which corresponds to a source language statement.        |
|      | A sequence of ILTs represent a block. ILTs have links to next and previous.                |
+------+--------------------------------------------------------------------------------------------+
| IPA  | InterProcedural Analysis https://en.wikipedia.org/wiki/Interprocedural_optimization        |
+------+--------------------------------------------------------------------------------------------+
| IR   | Intermediate Representation. A general term which may refer to many representations.       |
+------+--------------------------------------------------------------------------------------------+
| PGI  | Older version of Fortran compiler, which Flang front-end bases on.                         |
|      | PGI Compilers & Tools have evolved into the NVIDIA HPC SDK.                                |
+------+--------------------------------------------------------------------------------------------+
| STB  | (used heavily in code)                                                                     |
|      | Symbol TaBle, created symbol after symbol is identified by the parser                      |
+------+--------------------------------------------------------------------------------------------+
| TPV  | Target Processor Value (used in semant.c)                                                  |
+------+--------------------------------------------------------------------------------------------+
