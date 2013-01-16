# GNUmakefile
#
# Copyright (C) 2012 Thien-Thi Nguyen
#
# This file is part of IXIN.
#
# IXIN is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# IXIN is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with IXIN.  If not, see <http://www.gnu.org/licenses/>.

include common.mk

hmm := hello alive rcs serveez coreutils
ixin := $(addsuffix .ixin, $(hmm))

all: $(ixin)

%.sxml-not-quite : %.xml
	  $(C)a1-nf1 $< 2>/dev/null		\
	| $(C)a1-nf3-from-nf1			\
	> $@

clean:
	rm -f *.ixin

maintainer-clean: clean
	rm -f *.sxml

sxml := $(addsuffix .sxml, $(hmm))
sxml: $(sxml)

not-quite: $(addsuffix .sxml-not-quite, $(hmm))

demo-commands :=				\
 'dump-meta'					\
 'dump-counts'					\
 'dump-index'					\
 'show-node Top'

demo-%: all
	@printf '\n======================================== %s\n' $*
	@for c in $(demo-commands) ; do		\
	  printf '\f\n' ;			\
	  $(C)retrieve $*.ixin $$c ;		\
	done

demo: demo-hello demo-alive demo-rcs

ZOW = alive

demo-zow: $(ZOW).ixin
	$(C)retrieve $(ZOW).ixin repl 17 < zow

# GNUmakefile ends here
