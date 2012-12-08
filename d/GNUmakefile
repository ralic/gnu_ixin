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

C := ../c/

hmm := hello alive prob rcs
ixin := $(addsuffix .ixin, $(hmm))

all: $(ixin)

%.ixin : %.sxml
	$(C)mkixin $<

%.sxml : %.xml
	$(C)a1-nf3-mixp $< > $@

clean:
	rm -f *.ixin

maintainer-clean: clean
	rm -f *.sxml

sxml := $(addsuffix .sxml, $(hmm))
sxml: $(sxml)

r := @printf '\f\n' ; $(C)retrieve
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
	$(C)retrieve $(ZOW).ixin repl 17 < zow

try-%:
	$(C)mkixin $*.sxml
	$(C)retrieve $*.ixin

# GNUmakefile ends here
