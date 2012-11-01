# ##################################################
#
# Copyright IBM Corp. 2011, 2012
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
# ##################################################

ONEMIN = 60000  # ms
RHIPE_MAP_BUFFSIZE    = 100  # number of records per batch **250MB Rhipe limit**
RHIPE_REDUCE_BUFFSIZE = 100  # much safer than the default of 10,000 records

.onAttach <- function(libname, pkgname) {
  version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), fields="Version")
  packageStartupMessage(paste("\n", pkgname, version, "\n", "
    # ##################################################
    #
    # Copyright IBM Corp. 2011, 2012
    # 
    # Licensed under the Apache License, Version 2.0 (the \"License\");
    # you may not use this file except in compliance with the License.
    # You may obtain a copy of the License at
    # 
    #     http://www.apache.org/licenses/LICENSE-2.0
    # 
    # Unless required by applicable law or agreed to in writing, software
    # distributed under the License is distributed on an \"AS IS\" BASIS,
    # WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    # See the License for the specific language governing permissions and
    # limitations under the License.
    # 
    # ##################################################
  ")
  )
}
