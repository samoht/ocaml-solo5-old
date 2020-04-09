.PHONY: all clean

VERSION=v0.6.4
NAME=solo5-${VERSION}
ARCHIVE=${NAME}.tar.gz
URL=https://github.com/Solo5/solo5/releases/download/${VERSION}/solo5-${VERSION}.tar.gz

ifndef FETCH
  ifneq ($(shell command -v curl 2>/dev/null),)
    FETCH = curl -LSfs -o
  else
    FETCH = wget -O
  endif
endif

all: src
	@echo "=> ${NAME} is extracted in src/"
	dune build

clean:
	rm -rf src ${NAME}
	rm -f ${ARCHIVE}

src: ${ARCHIVE}
	rm -rf src
	tar xvfz ${ARCHIVE}
	mv ${NAME} src

${ARCHIVE}:
	${FETCH} ${ARCHIVE} ${URL}
