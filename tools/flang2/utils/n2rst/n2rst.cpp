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

/**
   \file
   \brief Nroff to Sphinx conversion utility.

   This program takes arbitrary number of input file names on the command line
   and creates corresponding files with the extension "rst", converting the
   input files from Nroff to Sphinx format.
 */

#include "utils.h"
#include <iostream>

/**
   \class is the main application class.
 */
class N2Rst : public UtilityApplication
{

  bool verbose; /// whether the program should print what it does.
  std::vector<std::string> input_filenames; /// the arrays of input filenames.

public:
  /**
     Constructor takes as argument the vector of command line options given to
     the program and assumes args doesn't contain the program name as args[0].
   */
  N2Rst(const std::vector<std::string> &args) : verbose(false)
  {
    for (std::vector<std::string>::const_iterator it = args.begin(),
                                                  E = args.end();
         it != E; ++it) {
      if (*it != "-v") {
        input_filenames.push_back(*it);
      } else {
        verbose = true;
      }
    }
  }

  /**
     \brief Implements the logic of the program.
   */
  int
  run()
  {
    if (input_filenames.empty()) {
      std::cerr << "No input files." << std::endl;
      return 1;
    }
    for (std::vector<std::string>::const_iterator it = input_filenames.begin(),
                                                  E = input_filenames.end();
         it != E; ++it) {
      std::ifstream ifs(it->c_str());
      if (!ifs) {
        std::cerr << "Can't open file " << *it << std::endl;
        return 1;
      }
      if (verbose) {
        std::cout << *it << " -> " << get_output_filename(*it) << std::endl;
      }
      sphinx.setOutputFile(get_output_filename(*it));
      for (std::string line; std::getline(ifs, line);) {
        sphinx.process(line);
      }
    }
    return 0;
  }

private:
  /**
     \brief Generate the name of an output file given the name of an input file.
   */
  std::string
  get_output_filename(const std::string &s)
  {
    auto pos = s.find_last_of('/');
    std::string filename = pos != std::string::npos ? s.substr(pos + 1) : s;
    pos = filename.find_last_of('.');
    if (pos != std::string::npos) {
      return filename.substr(0, pos + 1) + "rst";
    }
    return filename + ".rst";
  }
};

int
main(int argc, char **argv)
{
  // DO NOT add the program name to the vector of arguments.
  N2Rst app(std::vector<std::string>(argv + 1, argv + argc));
  return app.run();
}
