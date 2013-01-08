;;; spit.el

;; Copyright (C) 2013 Thien-Thi Nguyen
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

;; This uses external command ./retrieve and feature ‘shr'
;; to vet the IXIN file format by rendering what it can and
;; complaining about what it can't.
;;
;; (Actually, it doesn't yet use shr.el, but it will Real Soon Now.)
;; This also explains somewhat the shameless angry fruit salad style.
;; We're just playing around this time...

;;; Code:

(require 'cl)
(require 'regexp-opt)

;;;---------------------------------------------------------------------------
;;; variables for the uninquisitive programmer

(defvar spit-retrieve (expand-file-name
                       "retrieve"
                       (if load-file-name
                           (file-name-directory load-file-name)
                         "../c"))
  "Absolute filename of the \"retrieve\" program.")

(defvar spit-mode-map nil
  "Keymap for `spit-mode'.")

;;;---------------------------------------------------------------------------
;;; variables for the inquisitive programmer

(defvar spit--got nil)

(defvar spit--cache nil)

(defvar spit--s-tree-parts
  (regexp-opt
   (mapcar 'symbol-name
           '(top chapter unnumbered appendix
                 section unnumberedsec appendixsec
                 subsection unnumberedsubsec appendixsubsec
                 subsubsection unnumberedsubsubsec appendixsubsubsec))
   'symbols))

;;;---------------------------------------------------------------------------
;;; support

(defun spit--process-filter (proc string)
  (goto-char (point-max))
  (insert string)
  (when (and (= 10 (char-before))
             (= 17 (char-before (1- (point)))))
    (delete-char -2)
    (setq spit--got t)))

(defun spit--do (disposition command &rest args)
  (goto-char (point-min))
  (forward-line 2)
  (delete-region (point) (point-max))
  (let ((proc (get-buffer-process (current-buffer)))
        (form (cons command args))
        (start (point)))
    (setq spit--got nil)
    (message "doing: %S" form)
    (process-send-string proc (format "%S\n" form))
    (while (and (eq 'run (process-status proc))
                (not spit--got))
      (accept-process-output proc))
    (goto-char start)
    (setq spit--got
          (case disposition
            ((nil) t)
            (t (spit--gobble))))
    spit--got))

(defun spit--cache (&rest args)
  (case (length args)
    ((2) (puthash (car args) (cadr args) spit--cache))
    ((1) (gethash (car args) spit--cache))
    ((0)
     (goto-char (point-min))
     (forward-sexp 1)
     (unless (eolp)
       (kill-sexp 1))
     (insert (format "  %S" (let (acc)
                              (maphash (lambda (k v)
                                         (push k acc))
                                       spit--cache)
                              acc)))
     (center-line))))

(defun spit--gobble (&optional finish)
  (let ((beg (point)))
    (prog1 (read (current-buffer))
      (case finish
        ((peek) (goto-char beg))
        ((past) t)
        (t (delete-region beg (point)))))))

(defun spit--selectively-propertize (top-nf face)
  (flet ((rec (nf)
              (if (stringp nf)
                  (propertize nf 'face face)
                (let ((element (car nf)))
                  (flet ((pkid (&rest face) (spit--propertize-kids
                                             (cddr nf) face)))
                    (case element
                      ((sc)
                       (setcdr (cdr nf) (mapcar (lambda (kid)
                                                  (if (stringp kid)
                                                      (upcase kid)
                                                    kid))
                                                (cddr nf)))
                       (pkid :height 0.95
                             :foreground "orange"))
                      ((acronym acronymword)
                       (pkid :height 0.95))
                      ((r)
                       (pkid :inherit 'variable-pitch
                             :foreground "orange"))
                      ((var)
                       (pkid :slant 'italic
                             :foreground "orange"))
                      ((b)
                       (pkid :weight 'bold
                             :foreground "orange"))
                      ((code samp command option)
                       (pkid :inherit 'default
                             :foreground "orange"))
                      (t (concat (format "(%S %S" (car nf) (cadr nf))
                                 (let ((kids (cddr nf)))
                                   (if kids
                                       (concat " " (mapconcat 'rec kids " "))
                                     ""))
                                 ")"))))))))
    (rec top-nf)))

(defun spit--propertize-kids (kids face)
  (apply 'concat
         (map 'list 'spit--selectively-propertize
              kids
              (make-list (length kids)
                         face))))

(defun spit--vp (s &rest args)
  (propertize (apply 'format s args) 'face
              'variable-pitch))

(defun spit--check-unhandled ()
  (let (ugh)
    (while (re-search-backward "(\\(\\w+\\) (@" nil t)
      (pushnew (match-string 1) ugh
               :test 'string=))
    (when ugh
      (setq ugh (list "unhandled elements: %s"
                      (mapconcat 'identity
                                 ugh
                                 ", "))))
    (apply 'spit-spit-spit ugh)))

(put 'spit-mode 'mode-class 'special)   ; ugh
(defun spit-mode ()
  "Major mode for browsing IXIN files (via \\[spit]).

\\{spit-mode-map}"
  (let ((saved-fill-column fill-column))
    (kill-all-local-variables)
    (setq fill-column saved-fill-column))
  (setq mode-name "Spit"
        major-mode 'spit-mode)
  (use-local-map spit-mode-map))

(defun spit-spit-spit (&optional s &rest args)
  (interactive)
  "With no args, clear the spit area.
With args (noninteractively), like `message' for the spit area."
  (let* ((blurb (apply 'format (concat "[ " s " ]") args))
         (len (length blurb))
         (col (ash (- (window-width) len) -1))
         (pos (save-excursion
                (goto-char (point-min))
                (forward-line 1)
                (let ((p (point)))
                  (mapc 'delete-overlay (overlays-in p (line-end-position)))
                  (+ col p)))))
    (when s
      (overlay-put (make-overlay pos (+ pos len))
                   'display blurb))))

;;;---------------------------------------------------------------------------
;;; repl commands

(defun spit-%quit ()
  (interactive)
  "Close connection and kill the buffer."
  (spit--do nil 'quit)
  (kill-buffer nil))

(defun spit-%where ()
  (interactive)
  "Do ‘(where)’."
  (spit--do nil 'where))

(defun spit-%nav ()
  (interactive)
  "Do ‘(nav)’."
  (spit--do nil 'nav))

(defun spit-%next ()
  (interactive)
  "Do ‘(next)’."
  (spit--do nil 'next))

(defun spit-%prev ()
  (interactive)
  "Do ‘(prev)’."
  (spit--do nil 'prev))

(defun spit-%backward ()
  (interactive)
  "Do ‘(backward)’."
  (spit--do nil 'backward))

(defun spit-%forward ()
  (interactive)
  "Do ‘(forward)’."
  (spit--do nil 'forward))

(defun spit-%up ()
  (interactive)
  "Do ‘(up)’."
  (spit--do nil 'up))

(defun spit-%show ()
  (interactive)
  "Do ‘(show)’."
  (spit--do nil 'show)
  (spit-spit-spit "hey ttn, why not use shr.el? (hint hint)"))

(defun spit-%dump-meta ()
  (interactive)
  "Do ‘(dump-meta)’, prettify, save title, and cache some metainfo.
The title replaces the filename on the first line."
  (destructuring-bind ((fn lang title invitations)
                       vars settings copying titlepage toc)
      (spit--do t 'dump-meta)
    (goto-char (point-min))
    (kill-sexp 1)
    (insert ?\( (spit--propertize-kids (if (stringp title)
                                           (list title)
                                         title)
                                       'variable-pitch)
            ?\))
    (forward-line 2)
    (save-excursion
      (flet ((orange (s) (spit--propertize-kids (list s)
                                                (list :inherit 'variable-pitch
                                                      :foreground "orange")))
             (nl () "\n\n"))
        (let ((standard-output (current-buffer))
              (face (list :inherit 'variable-pitch
                          :foreground "red")))
          (insert
           (orange "filename:") (format " %S" fn)
           (nl)
           (orange "lang:") (format " %S" lang)
           (nl)
           (orange (format
                    "invitations (%d cat / %d ent):\n"
                    (length invitations)
                    (length (apply 'append (mapcar 'cdr invitations))))))
          (flet ((pretty (x) (spit--propertize-kids (if (stringp x)
                                                        (list x)
                                                      x)
                                                    face)))
            (dolist (inv invitations)
              (destructuring-bind (name &rest ents) inv
                (insert "  " (pretty name) "\n")
                (dolist (ent ents)
                  (destructuring-bind (title node &rest description) ent
                    (insert "    " (pretty title) " -- " node)
                    (when description
                      (insert " -- " (mapconcat 'pretty description
                                                "\n      -- ")))
                    (insert "\n"))))))
          (flet ((out (x) (let ((blurb (format "%s:" x))
                                (obj (symbol-value x)))
                            (insert "\n" (orange blurb) "\n")
                            (pp obj)
                            (unless (bolp)
                              (newline)))))
            (mapc 'out '(vars settings copying titlepage toc))))
        (spit--cache 'invitations invitations)
        (spit--cache 'settings settings)
        (spit--cache)))))

(defun spit-%dump-index ()
  (interactive)
  "Do ‘(dump-index)’, prettify, and cache some metainfo."
  (spit--do nil 'dump-index)
  (save-excursion
    (let ((nid<-name (make-hash-table :test 'equal))
          (name<-nid (unless (spit--cache 'name<-nid)
                       (make-hash-table :test 'eq)))
          nid)
      (while (progn (skip-syntax-forward "-")
                    (< (point) (point-max)))
        (setq nid (spit--gobble 'past))
        (destructuring-bind (name blen n p u &rest etc) (spit--gobble)
          (insert (format " %6d  %s  " blen
                          (map 'string (lambda (n c)
                                         (if (> 0 n)
                                             ?-
                                           c))
                               (list n p u)
                               '(?n ?p ?u)))
                  (spit--vp name))
          (puthash name nid nid<-name)
          (when name<-nid
            (puthash nid (spit--vp name) name<-nid))))
      (spit--cache 'nid<-name nid<-name)
      (when name<-nid
        (spit--cache 'name<-nid name<-nid))
      (spit--cache))))

(defun spit-%show-labels ()
  (interactive)
  "Do ‘(show-labels)’."
  (spit--do nil 'show-labels))

(defun spit-%list-dts ()
  (interactive)
  "Do ‘(list-dts)’."
  (spit--do nil 'list-dts))

(defun spit-%dump-dts ()
  (interactive)
  "Do ‘(dump-dts)’ and prettify.
Report unhandled elements."
  (spit--do nil 'dump-dts)
  (let ((cache (spit--cache 'name<-nid)))
    (save-excursion
      (while (< (point) (point-max))
        (re-search-forward "\\(.+\\): ")
        (let* ((name (match-string 1))
               (details (spit--gobble))
               (default-font (cadr details))
               (normal (case default-font
                         ((r) '(:inherit variable-pitch :foreground "red"))
                         ((code) '(:foreground "blue"))))
               (invert (case default-font
                         ((code) '(:inherit variable-pitch :foreground "green"))
                         ((r) '(:foreground "yellow"))))
               col)
          (delete-region (line-beginning-position) (point))
          (insert (spit--vp "DTS: %s / entries: %d / " name (car details))
                  (propertize "normal" 'face normal)
                  " "
                  (propertize "invert" 'face invert))
          (while (progn
                   (skip-syntax-forward "-")
                   (not (zerop (current-column))))
            (kill-sexp 1)
            (destructuring-bind
                (face term nid &rest more)
                (let ((form (spit--gobble)))
                  (if (eq '- (car form))
                      (list* invert (cdr form))
                    (list* normal form)))
              (delete-horizontal-space)
              (setq term (spit--propertize-kids (if (stringp term)
                                                    (list term)
                                                  term)
                                                face))
              (if cache
                  (flet ((pretty (nid) (gethash nid cache)))
                    (insert " " term)
                    ;; FIXME: Neither ‘current-column’ nor ‘string-width’
                    ;;        take into account the actual displayed face.
                    (setq col (1+ (string-width term)))
                    (insert " -- " (pretty nid))
                    (dolist (nid more)
                      (newline)
                      (insert (propertize " " 'display `(space :align-to ,col))
                              " -- "
                              (pretty nid))))
                (insert " " term
                        " -- " (mapconcat 'number-to-string
                                          (cons nid more)
                                          ", "))))))
        (newline))
      (spit--check-unhandled))))

(defun spit-%dump-s-tree ()
  (interactive)
  "Do ‘(dump-s-tree)’, prettify and cache some metainfo.
Report unhandled elements."
  (spit--do nil 'dump-s-tree)
  (let ((name<-nid (or (spit--cache 'name<-nid)
                       (make-hash-table :test 'eq)))
        (p (point))
        fmt)
    (let (lens)
      (while (re-search-forward spit--s-tree-parts nil t)
        (push (length (match-string 0)) lens))
      (setq lens (apply 'max lens)
            fmt (format " %%3d  %%%ds  --  %%s" lens)))
    (goto-char p)
    (while (re-search-forward "^.*([0-9]" nil t)
      (backward-up-list)
      (destructuring-bind (nid what &rest rs) (spit--gobble)
        (let ((prefix (make-string (* 3 (- (current-column) 2))
                                   ?\s))
              (pretty (spit--propertize-kids
                       rs '(:inherit variable-pitch :foreground "red"))))
          (delete-region (line-beginning-position) (point))
          (puthash nid pretty name<-nid)
          (insert (format fmt nid what prefix) pretty)))
      (delete-region (point) (line-end-position)))
    (spit--check-unhandled)
    (goto-char p)
    (spit--cache 'name<-nid name<-nid)
    (save-excursion
      (spit--cache))))

(defun spit (filename)
  "Browse IXIN FILENAME in a buffer in `spit-mode'. \\<spit-mode-map>

The first line shows the filename or title (from \\[spit-%dump-meta])
and the keys in the cache, if any.  The second is the \"spit area\",
normally a series of hyphens, but occasionally showing some comment.
To clear the comment, use \\[spit-spit-spit].

The rest of the buffer displays various output.
See also variable `spit-retrieve'."
  (interactive (list (read-file-name
                      "File (.ixin only): " nil nil t nil
                      (lambda (maybe)
                        (string-match "\\(/\\|[.]ixin\\)$"
                                      maybe)))))
  (switch-to-buffer (get-buffer-create
                     (format " %s" (file-name-nondirectory filename))))
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (delete-process proc)
      (erase-buffer)))
  (insert "(" filename ")")
  (setq fill-column (1- (window-width)))
  (center-line)
  (insert (format "\n%s\n" (make-string fill-column ?-)))
  (redisplay)
  (spit-mode)
  (set (make-local-variable 'spit--cache)
       (make-hash-table :test 'eq))
  (setq filename (expand-file-name filename))
  (let* ((process-connection-type nil)  ; pipe
         (proc (start-process "spit" (current-buffer) spit-retrieve
                              filename
                              "repl" "17")))
    (set-process-filter proc 'spit--process-filter))
  (setq default-directory (file-name-directory filename)
        mode-line-process '(" [%s]")))

;;;---------------------------------------------------------------------------
;;; load-time actions

(unless spit-mode-map
  (setq spit-mode-map
        (let ((m (make-sparse-keymap))
              (plist '(" "           scroll-up-command
                       [(backspace)] scroll-down-command
                       "\C-l"    spit-spit-spit
                       "\C-q"    spit-%quit
                       "w"       spit-%where
                       "N"       spit-%nav
                       "n"       spit-%next
                       "p"       spit-%prev
                       "["       spit-%backward
                       "]"       spit-%forward
                       "u"       spit-%up
                       "s"       spit-%show
                       "\M-m"    spit-%dump-meta
                       "\M-i"    spit-%dump-index
                       "l"       spit-%show-labels
                       "\C-\M-d" spit-%list-dts
                       "\M-d"    spit-%dump-dts
                       "\M-s"    spit-%dump-s-tree)))
          (suppress-keymap m)
          (while plist
            (define-key m (car plist) (cadr plist))
            (setq plist (cddr plist)))
          m)))

(provide 'spit)

;;; spit.el ends here
