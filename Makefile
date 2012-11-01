# Robert J. Prill <rjprill@us.ibm.com>

PACKAGE="BlueSNP"
VERSION="0.1.0"
DATE="11-01-2012"

# Some tasks require that environment variables 
# BLUESNP_REMOTE_SERVER,
# BLUESNP_REPORT_PORT (ssh)
# be set in .bashrc

all:	setup description R man package

setup:	
	if [ ! -d build ]; then mkdir build; fi
	if [ ! -d build/R ]; then mkdir build/R; fi
	cp src/NAMESPACE build

description:	setup
	sed "s/::PACKAGE::/${PACKAGE}/" src/DESCRIPTION > build/temp1
	sed "s/::VERSION::/${VERSION}/" build/temp1 > build/temp2
	sed "s/::DATE::/${DATE}/" build/temp2 > build/temp3
	mv build/temp3 build/DESCRIPTION
	rm build/temp1
	rm build/temp2

R:	setup
	cat src/R/*/*.R > build/R/temp1
	cat src/R/*.R > build/R/temp2  # aaa.R is first
	cat build/R/temp2 build/R/temp1 > build/R/${PACKAGE}.R
	rm build/R/temp1
	rm build/R/temp2

man:	setup
	cp -r src/man build

inst:	setup
	if [ ! -d build/inst ]; then mkdir build/inst; fi
	cp etc/tutorial/cc/data/simulated* build/inst  # simulated data in tutorial/

package:	setup description R man
	if [ -d ${PACKAGE} ]; then rm -r ${PACKAGE}; fi
	cp -r build ${PACKAGE}
	tar czf - ${PACKAGE} > temp1
	mv temp1 ${PACKAGE}_${VERSION}.tar.gz

install:	all
	R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz

uninstall:	
	R CMD REMOVE ${PACKAGE}

clean:	
	if [ -d build ]; then rm -r build; fi
	if [ -d ${PACKAGE} ]; then rm -r ${PACKAGE}; fi
	if [ -f ${PACKAGE}_${VERSION}.tar.gz ]; then rm ${PACKAGE}_${VERSION}.tar.gz; fi

remote_install:	all
	# expects environment variables BLUESNP_REMOTE_SERVER, BLUESNP_REMOTE_PORT to be set in .bashrc
	# BLUESNP_REMOTE_PORT for when ssh is listening on a non-standard port (e.g., 2222 instead of 22)
	scp -P ${BLUESNP_REMOTE_PORT} ${PACKAGE}_${VERSION}.tar.gz ${BLUESNP_REMOTE_SERVER}:
	ssh -p ${BLUESNP_REMOTE_PORT} ${BLUESNP_REMOTE_SERVER} "R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz"

remote_uninstall:	all
	# expects environment variable BLUESNP_REMOTE_SERVER, BLUESNP_REMOTE_PORT to be set in .bashrc
	# BLUESNP_REMOTE_PORT for when ssh is listening on a non-standard port (e.g., 2222 instead of 22)
	ssh -p ${BLUESNP_REMOTE_PORT} ${BLUESNP_REMOTE_SERVER} "R CMD REMOVE ${PACKAGE}"

remote_copy: R
	scp -P ${BLUESNP_REMOTE_PORT} build/R/BlueSNP.R ${BLUESNP_REMOTE_SERVER}:

remote_tutorial:	
	scp -P ${BLUESNP_REMOTE_PORT} -r etc/tutorial_sweave ${BLUESNP_REMOTE_SERVER}:



