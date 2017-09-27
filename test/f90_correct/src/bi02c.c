/*
 * Copyright (c) 2015, NVIDIA CORPORATION.  All rights reserved.
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
 */

/* C counterpart to test Fortran parameter passing of C_LOC()
   and TYPE(C_PTR) 
 */


#define N 77
#define ND 4


extern int expect[N]= {  3, 55, 11, 33,  3, 12, 12, 12, 12, 3, 
			 3, 3,  3,  3,   3,  3,  3,  3,  3, 3,
			 3, 3,  3, 12,  12, 12, 12,  3,  3, 3, 
			 3, 3,  3,  3,   3,  3,  3,  3,  3, 3,
			 3, 3,  3,  3,   3,  3,  3,  3,  3, 3,
			 3, 12, 55, 11,  5,  3, 12,  5,  5, 3,
			12, 5,  33, 3,  12, 12, 12, 12,  3, 3,
			 5, 3,  12, 5, 3, 3, 3
		      };
extern int result[N]= { 0};

int c= 0;

extern void cp_call (int i, int * j, int *k, int ll) { 
	result[c++] = i;
	result[c++] = *j;
	result[c++] = *k;
	result[c++] = ll;
}
extern void c_call_ref (int ** ii) {
	result[c++] = **ii;
}

extern void c_call (int * i) { 
	result[c++] = *i;
}
extern int * c_fun (int * i) { 

	result[c++] = *i;
	return i;
}
extern int * c_fun_ptr (int ** i) { 
	result[c++] = **i;
	return *i;
}
extern int * c_fun_ref (int ** i) { 
	result[c++] = **i;
	return *i ;
}
extern int * cp_fun (int i, int * j, int *k, int i2) { 
	result[c++] = i;
	result[c++] = *j;
	result[c++] = *k;
	result[c++] = i2;
	return j;
}

extern int * get_ptr (int * pp) { 

	result[c++] = *pp;
        return pp; 
}

extern int * get_ptr_ref ( int ** pp2) {
	result[c++] = **pp2;
	return*  pp2; 
} 


