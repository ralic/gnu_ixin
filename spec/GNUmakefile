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

all: ixin.info ../d/ixin.xml

input := ixin.texi release.texi fdl.texi

ixin.info: $(input)
	makeinfo -o $@ $<

release.texi: ../GNUmakefile
	{ sed '/^VERSION =/!d;s/.*=/@set VERSION/' $< ; \
	  date -r $< +'@set UPDATED %Y-%m-%d' ; \
	} > $@

../d/ixin.xml: $(input)
	makeinfo -o $@ --xml --no-split $<

check: all
	$(MAKE) -C ../d ixin.sxml try-ixin

ixin.pdf: $(input)
	texi2pdf -o $@ $< --clean --batch

# GNUmakefile ends here
