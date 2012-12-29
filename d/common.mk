# common.mk
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

%.ixin : %.sxml
	$(C)mkixin $<

%.sxml : %.xml
	$(C)a1-nf3-mixp $< > $@

TRY =

try-%:
	$(C)mkixin $*.sxml
	$(C)retrieve $*.ixin $(TRY)

# common.mk ends here
