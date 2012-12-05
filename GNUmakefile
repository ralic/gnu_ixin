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

VERSION = 1.2

hmm := hello alive prob rcs
ixin := $(addsuffix .ixin, $(hmm))

all: $(ixin)

%.ixin : %.xml
	./a2ixin $<

%.sxml : %.xml
	./a1-nf3-mixp $< > $@

clean:
	rm -f *.ixin

sxml := $(addsuffix .sxml, $(hmm))
sxml: $(sxml)

dd := ixin-$(VERSION)
dist-files := \
 *.xml a1-nf3-* a2ixin retrieve GNUmakefile \
 z-fixed-pp.scm zomg zow \
 COPYING NEWS README

dist: $(sxml) $(ixin)
	rm -rf $(dd)
	mkdir $(dd)
	cp -p $(dist-files) $^ $(dd)
	tar cf $(dd).tar.xz --auto-compress $(dd)
	rm -rf $(dd)

r := @printf '\f\n' ; ./retrieve
hello := $(r) hello.ixin
alive := $(r) alive.ixin
rcs   := $(r) rcs.ixin

demo-%: all
	@printf '\n======================================== %s\n' $*
	$($*) dump-meta
	$($*) dump-counts
	$($*) dump-index
	$($*) show-node Top

demo: demo-hello demo-alive demo-rcs

ZOW = alive

demo-zow: $(ZOW).ixin
	./retrieve $(ZOW).ixin repl 17 < zow

try-%:
	./a2ixin $*.sxml
	./retrieve $*.ixin

# GNUmakefile ends here
