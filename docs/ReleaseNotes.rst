.. Copyright (c) 2017, NVIDIA CORPORATION.  All rights reserved.
..
.. Licensed under the Apache License, Version 2.0 (the "License");
.. you may not use this file except in compliance with the License.
.. You may obtain a copy of the License at
..
..     http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS,
.. WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
.. See the License for the specific language governing permissions and
.. limitations under the License.

=====================================
Flang 1.0 (In-Progress) Release Notes
=====================================

.. contents::
   :local:
   :depth: 2

Written by the `Flang Team <http://flang.nvidia.com/>`_

.. warning::

   These are in-progress notes for the upcoming Flang 1.0 release. You may
   prefer the `Flang 0.0 Release Notes
   <http://flang.nvidia.com/releases/0.0/tools/flang/docs/ReleaseNotes.html>`_.

Introduction
============

This document contains the release notes for the Flang Fortran 2008
frontend, release 1.0. Here we describe the status of Flang in some
detail, including major improvements from the previous release and new
feature work.

For more information about Flang, including information about
the latest release, please check out the main please see the `Flang Web
Site <http://flang.nvidia.com>`_.

What's New in Flang 1.0?
========================

Some of the major new features and improvements to Flang are listed
here.  Generic improvements to Flang as a whole or to its underlying
infrastructure are described first, followed by improvements to
Flang's support for Fortran versions.

Major New Features
------------------

- Feature1...

Improvements to Flang's diagnostics
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Flang's diagnostics are constantly being improved to catch more issues,
explain them more clearly, and provide more accurate source information
about them.  The improvements since the 0.0 release include:

-  ...

New Compiler Flags
------------------

The option ....

Fortran Language Changes in Flang
---------------------------------
The -faltivec and -maltivec flags no longer silently include altivec.h on Power platforms.

...

Fortran 2008 Feature Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

...

Internal API Changes
--------------------

These are major API changes that have happened since the 0.0 release of
Flang. If upgrading an external codebase that uses Flang as a library,
this section should help get you past the largest hurdles of upgrading.

-  ...

New Issues Found
================

- ...

Significant Known Problems
==========================

Additional Information
======================

A wide variety of additional information is available on the `Flang web
page <http://flang.nvidia.com/>`_. The web page contains versions of the
API documentation which are up-to-date with the Subversion version of
the source code. You can access versions of these documents specific to
this release by going into the "``flang/docs/``" directory in the Flang
tree.

If you have any questions or comments about Flang, please feel free to
contact us via the `mailing
list <http://lists.llvm.org/mailman/listinfo/cfe-dev>`_.
