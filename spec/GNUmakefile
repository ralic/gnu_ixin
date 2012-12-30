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

MAKEINFO = makeinfo
TEXI2PDF = texi2pdf

include ../d/common.mk

all: ixin.info ixin.xml

input := ixin.texi release.texi todo.texi fdl.texi

ixin.info: $(input)
	$(MAKEINFO) -o $@ $<

release.texi: ../GNUmakefile
	{ sed '/^VERSION =/!d;s/.*=/@set VERSION/' $< ; \
	  date -r $< +'@set UPDATED %Y-%m-%d' ; \
	} > $@

todo.texi: ../README
	< $<					\
	  sed -e '/Texinfo..hackers/,/^..-/!d'	\
	| sed -e '1s/.*/@verbatim/'		\
	      -e '$$s/.*/@end verbatim/'	\
	> $@

ixin.xml: $(input)
	$(MAKEINFO) -o $@ --xml --no-split $<

check: all ixin.sxml try-ixin

ixin.pdf: $(input)
	$(TEXI2PDF) -o $@ $< --clean --batch

z4.xml: $(input)
	test 4.13 = $(shell $(MAKEINFO) --version | sed 's/.* //;1q')
	$(MAKEINFO) -o $@ --xml --no-split $<

# GNUmakefile ends here
