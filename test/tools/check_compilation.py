#
# Copyright (c) 2016-2018, NVIDIA CORPORATION.  All rights reserved.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

"""
Helper for checking the output of deja-gnu style tests.

This script prints all non-error lines to allow external tools to find other
problems. It then tries to match up lines with dg-type errors with the errors
in the ouput file & prints any non-matching errors.

Script allows to match F90 (emited by flang) and PGF90 (emited by pgfortran)
error prefixes. This functionality can be modified to disable checking error
prefixes in the future.
"""

import re
import argparse
from collections import namedtuple
from os.path import basename

# Regex notes:
#     \S matches any non-whitespace character
#     \d matches [0-9]
#     \w matches [A-Za-z_0-9]
#     .  matches any character except (possibly) line terminators
#     $  matches end-of-input
#     Parentheses define a capturing group
#     (?P<foo> ... ) defines a capturing group accessed with m.group("foo")

# an error listed in the source file:
dgError = re.compile(r"""
	^(?P<stmt>.*?)\s*!\s*{\s*   # fortran statement before error marker
	(?P<dg>dg-)?                # dg- prefix means text need not match
	(?P<sev>(error|warning))\s* # severify
	\"(?P<prefix>(?:PG)?F\d\d)  # error prefix
	-[WSF]-\d+-                 # PGI severity and error number
	(?P<text>.*)\"              # text of message
	\s*(?P<lineno>\d*)          # optional line number of message
	\s*}\s*$
	""", re.VERBOSE)

# an error output from the fortran compiler:
pgfError = re.compile(r"""
	(?P<prefix>(?:PG)?F\d\d)-            # error prefix
	(?P<sev>[WSF])-\d+-(?P<text>.+?)\s*  # text & sev of error
	\((?P<file>\S+)                      # file name
	(:\s*(?P<lineno>\d+))?\)$            # optional line number
	""", re.VERBOSE)

# Compiler crash
terminatedError = re.compile(r"TERMINATED by signal")

ErrorInfo = namedtuple("ErrorInfo", "file lineno sev prefix text")

def main():
	args = processArgs()
	terminated, logErrors = getLogErrors(args.log)
	sourceErrors = getSourceErrors(getFileSet(args.source, logErrors))
	if args.dump:
		dumpErrorSet(logErrors, title="Log Errors:")
		dumpErrorSet(sourceErrors, title="Source Errors:")
	checkErrorsMatch(terminated, logErrors, sourceErrors, basename(args.compiler))

def processArgs():
	""" Process command line arguments """

	parser = argparse.ArgumentParser()
	parser.add_argument("source", help="source file")
	parser.add_argument("log", help="log file")
	parser.add_argument("compiler", nargs='?', help="Name of the compiler executable or path to it", default="pgf90")
	parser.add_argument("--dump", help="dump the error lists", action="store_true")
	return parser.parse_args()

def getLogErrors(log):
	"""
	Extract erros from the log file

	:param log: log file path
	"""

	terminated = False
	errorList = []
	with open(log, "r") as handle:
		for line in handle:
			m = pgfError.match(line)
			if m:
				text = m.group("text")
				prefix = m.group("prefix")
				sev = "warning" if m.group("sev") == "W" else "error"
				file = m.group("file")
				lineno = m.group("lineno") if m.group("lineno") else ""
				errorList.append(ErrorInfo(file, lineno, sev, prefix, text))
			elif terminatedError.search(line):
				terminated = True
			else:
				print line,
	return terminated, errorList

def getFileSet(source, logErrors):
	"""
	Get set of files to extract errors, containing original source file and
	any files mentioned in the errors

	:param source: source file that was compiled
	:parma logErrors: list of error tuples from the log
	"""

	fileSet = set([source])
	for e in logErrors:
		fileSet.add(e.file)
	return fileSet

def getSourceErrors(fileSet):
	"""
	Collect error messages from source files

	:param fileSet: list of file names to process
	"""
	errorList = []
	saveList = []  # errors saved up for next line that isn't just dg-error
	for f in fileSet:
		with open(f, "r") as handle:
			lineno = 0
			for line in handle:
				lineno += 1
				m = dgError.search(line)
				if m:
					dg = m.group("dg")  # dg-error vs. error
					stmt = m.group("stmt")  # fortran stmt before dg-error
					text = m.group("text") if not dg else ""
					prefix = m.group("prefix") if not dg else ""
					sev = m.group("sev")
					l = m.group("lineno")  # lineno specific in dg-error
					if l != "":
						errorList.append(ErrorInfo(f, l, sev, prefix, text))
					elif stmt != "" or dg:
						errorList.append(ErrorInfo(f, str(lineno), sev, prefix, text))
					else:
						saveList.append(ErrorInfo(f, "", sev, prefix, text))
				if len(saveList) > 0 and (not m or m.group("stmt")):
					# this was not a full-line dg-error, so saveList goes with this line
					for e in saveList:
						errorList.append(ErrorInfo(e.file, str(lineno), e.sev, e.prefix, e.text))
					saveList = []
	return errorList + saveList

def checkErrorsMatch(terminated, logErrors, sourceErrors, compiler):
	"""
	Report error mismatches

	:param terminated: log indicates that compiler was terminated
	:param logErrors: error tuples from the compilation log
	:param sourceErrors: error tuples from the source file(s)
	:param compiler: compiler executable name
	"""

	fail = None
	for e in logErrors:
		if not removeMatch(e, sourceErrors, (compiler == "flang")):
			fail = True
			print "Unexpected:", e.sev, e.file, e.lineno, e.text
	for e in sourceErrors:
		fail = True
		print "Missed:", e.sev, e.file, e.lineno, e.text
	if fail or terminated:
		print "FAIL"
	else:
		print "PASS"

def removeMatch(e, errors, matchF90):
	"""
	If e matches one of errors, remove it and return True

	:param matchF90: match F90 prefix in error to PGF90 in entry in e
	"""
	for e2 in errors:
		if e.file != e2.file: continue
		if e.lineno != e2.lineno: continue
		if e.sev != e2.sev: continue
		if e2.text != "" and e.text != e2.text: continue
		if e2.prefix != "":
			if (not matchF90) and e.prefix != e2.prefix: continue
			if matchF90 and (e.prefix != "F90" or e2.prefix != "PGF90"): continue
		errors.remove(e2)
		return True
	return False

def dumpErrorSet(errors, title):
	"""
	Dump collected errors 

	:param erros: error list
	:param title: optional title
	"""
	if title:
		print title
	if len(errors) == 0:
		print "    (none)"
	else:
		for e in errors:
			print "   ", e.sev, e.file, e.lineno, e.prefix if e.prefix != None else "", e.text if e.text != None else ""


if __name__=="__main__":
   main()
