;;; z-dtd.scm

;; Copyright (C) 2014 Thien-Thi Nguyen
;;
;; This file is part of IXIN.
;;
;; IXIN is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; IXIN is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with IXIN.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; (search-forward "MAINTAIN-ME")

;;; Code:

(define-module (z-dtd)
  #:export (find-dtd-filename)
  #:use-module ((srfi srfi-1) #:select (find)))

(define KNOWN-AND-LOCAL                 ; MAINTAIN-ME
  (list
   ;; Each entry has the form ((PUBLIC-ID SYSTEM-ID) . FILENAME),
   ;; where PUBLIC-ID, SYSTEM-ID and FILENAME are strings.
   '(("-//GNU//DTD TexinfoML V5.0//EN"
      "http://www.gnu.org/software/texinfo/dtd/5.0/texinfo.dtd")
     . "../dtd/5.0.dtd")))

;; simple abstractions
(define ent-id-list  car)
(define ent-filename cdr)

(define (find-dtd-filename id)

  (define (referenced? ent)
    (member id (ent-id-list ent)))

  (and=> (find referenced? KNOWN-AND-LOCAL)
         ent-filename))

;;; z-dtd.scm ends here
