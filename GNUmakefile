# GNUmakefile
#
# Copyright (C) 2012, 2013 Thien-Thi Nguyen
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

VERSION = 1.7

all clean demo demo-% try-%:
	$(MAKE) $(MAKEFLAGS) -C d $@

dd := ixin-$(VERSION)
dist-files := \
 COPYING README NEWS \
 dtd/* \
 spec/* \
 d/*.xml d/*.sxml d/*.ixin d/zow \
 d/common.mk d/GNUmakefile d/ChangeLog \
 c/a1-* c/mkixin c/retrieve \
 c/z-fixed-pp.scm c/zomg \
 c/spit.el \
 c/ChangeLog \
 ChangeLog

dist:
	rm -rf $(dd)
	mkdir $(dd)
	cp -p --parents $(dist-files) $(dd)
	tar cf $(dd).tar.xz --auto-compress $(dd)
	rm -rf $(dd)

# GNUmakefile ends here
