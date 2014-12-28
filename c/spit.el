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

;; This uses external command ./retrieve and face-mucking
;; to vet the IXIN file format by rendering what it can and
;; complaining about what it can't.

;;; Code:

(require 'cl)
(require 'regexp-opt)
(require 'button)

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

(defvar spit-mode-syntax-table (let ((st (copy-syntax-table
                                          emacs-lisp-mode-syntax-table)))
                                 (modify-syntax-entry ?\n "-" st)
                                 st)
  "Syntax table for `spit-mode'.")

;;;---------------------------------------------------------------------------
;;; variables for the inquisitive programmer

(defvar spit--cur nil)

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

(defun spit--get-blob (n)
  (let ((orig (point))
        (beg (goto-char (point-max)))
        (proc (get-buffer-process (current-buffer))))
    (setq spit--got nil)
    (process-send-string proc (format "(blob %d)\n" n))
    (while (and (eq 'run (process-status proc))
                (not spit--got))
      (accept-process-output proc))
    (goto-char beg)
    (let ((ent (cdr (spit--gobble)))
          (blob (progn (delete-char 1)  ; newline
                       (delete-and-extract-region (point) (point-max)))))
      (goto-char orig)
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert blob)
        (case (pop ent)
          ((base64) (shell-command-on-region
                     (point-min) (point-max)
                     "base64 -d" (current-buffer) t)))
        (destructuring-bind (major minor &rest attrs) (pop ent)
          (case major
            ((image) (create-image (buffer-string) minor t
                                   :originally (pop ent)))
            (t nil)))))))

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
            ((cur) (save-excursion
                     (down-list)
                     (setq spit--cur (read (current-buffer)))))
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

(defun spit--orange (&rest kids)
  (spit--propertize-kids
   kids '(:inherit variable-pitch :foreground "orange")))

(defun spit--ixcc-explanation (fam pos)
  (let (ht)
    (case fam
      ((node) (when (setq ht (spit--cache 'node-name))
                (gethash pos ht)))
      ((blob) (let ((blob (spit--get-blob pos)))
                (case (when (consp blob)
                        (car blob))
                  ((image) (propertize " " 'display blob))
                  (t nil))))
      (t (when (consp fam)
           (case (car fam)
             ((dts) (when (setq ht (spit--cache
                                    (intern (format "dts:%s" (cadr fam)))))
                      (gethash pos ht)))
             (t nil)))))))

(defun spit--decorate-ixcc ()
  (let* ((fam (read (match-string 3)))
         (pos (read (match-string 4)))
         (ixcc (cons fam pos))
         (s (save-match-data (spit--ixcc-explanation fam pos))))
    (cond ((stringp s)
           (replace-match s nil t nil 2)
           (make-text-button (match-beginning 1)
                             (match-end 1)
                             'ixcc ixcc
                             'help-echo (format "%S" ixcc))
           (goto-char (match-beginning 0)))
          (t
           (make-text-button (match-beginning 1) (match-end 1)
                             'ixcc ixcc
                             'help-echo nil)
           (add-text-properties (match-beginning 3) (match-end 3)
                                '(face (:foreground "red")))
           (add-text-properties (match-beginning 4) (match-end 4)
                                '(face (:foreground "yellow")))))))

(defun spit--hsdp ()                ; highlight structure / diminish parens
  (save-restriction
    (let (p aft dq att)
      (skip-syntax-forward "-")
      (setq p (point)
            aft (char-after))
      (when aft
        (setq dq (= ?\" aft))
        (forward-sexp 1)
        (unless dq
          (narrow-to-region p (point))
          (goto-char p)
          (delete-pair)
          (forward-sexp 1)
          (add-text-properties p (point) '(face font-lock-builtin-face))
          (when (looking-at "\\s-+")
            (delete-region (match-beginning 0)
                           (match-end 0)))
          (insert " ")
          (let ((nattr (length (cdr (spit--gobble 'peek)))))
            (cond ((zerop nattr)
                   (kill-sexp 1)
                   (if (eolp)
                       (delete-char -1)
                     (insert (propertize "-" 'face 'font-lock-comment-face))))
                  (t
                   (down-list 1)
                   (when (looking-at "@\\s-+")
                     (delete-region (match-beginning 0)
                                    (match-end 0)))
                   (while (not (zerop nattr))
                     (down-list 1)
                     (if (looking-at "\\(ixcc\\) \"\\((\\(.+\\) \\([0-9]+\\))\\)\"")
                         (spit--decorate-ixcc)
                       (add-text-properties
                        (point) (progn (forward-sexp 1) (point))
                        '(face font-lock-comment-face))
                       (insert " \"" (spit--orange (spit--gobble))
                               "\""))
                     (backward-up-list)
                     (forward-sexp 1)
                     (decf nattr))
                   (backward-up-list)
                   (forward-sexp 1)
                   (save-excursion
                     (forward-sexp -1)
                     (delete-pair)))))
          (while (spit--hsdp)))
        t))))

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
  (set-syntax-table spit-mode-syntax-table)
  (use-local-map spit-mode-map))

(defun spit-spit-spit (&optional s &rest args)
  "With no args, clear the spit area.
With args (noninteractively), like `message' for the spit area."
  (interactive)
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
  "Close connection and kill the buffer."
  (interactive)
  (spit--do nil 'quit)
  (kill-buffer nil))

(defun spit-%where ()
  "Do ‘(where)’."
  (interactive)
  (spit--do 'cur 'where))

(defun spit-%nav ()
  "Do ‘(nav)’."
  (interactive)
  (spit--do nil 'nav))

(defun spit-%next ()
  "Do ‘(next)’."
  (interactive)
  (spit--do 'cur 'next))

(defun spit-%prev ()
  "Do ‘(prev)’."
  (interactive)
  (spit--do 'cur 'prev))

(defun spit-%backward ()
  "Do ‘(backward)’."
  (interactive)
  (spit--do 'cur 'backward))

(defun spit-%forward ()
  "Do ‘(forward)’."
  (interactive)
  (spit--do 'cur 'forward))

(defun spit-%up ()
  "Do ‘(up)’."
  (interactive)
  (spit--do 'cur 'up))

(defun spit-%show ()
  "Do ‘(show)’ and prettify.
If there are any settings (cached via \\[spit-%dump-meta]) or
tweaks (cached via \\[spit-%dump-index]) in effect at the start
of the node, show them, too.

The settings and tweaks are ordered most recent to most past,
with same-NAMEd past entries suppressed, one per line:

  NAME (N) [VALUE...]

N, usually 1 but not always, is the count of VALUEs.
The type of each VALUE depends on NAME."
  (interactive)
  (let ((form (spit--do t 'show))
        (p (point)))
    (pp form (current-buffer))
    (goto-char p)
    (spit--hsdp)
    (goto-char p)
    (let* ((tweaks (spit--cache 'tweaks))
           (infl (append (when tweaks
                           (gethash (1- spit--cur) tweaks))
                         (spit--cache 'settings))))
      (when infl
        (goto-char p)
        (while infl
          (let* ((x (pop infl))
                 (name (car x))
                 (vals (cdr x)))
            (insert (format "%S (%S) %s\n"
                            name (length vals)
                            (mapconcat (lambda (v)
                                         (format "%S" v))
                                       vals
                                       " ")))
            (setq infl (assq-delete-all name infl))))
        (insert "\n"))))
  (spit-spit-spit "hey ttn, why not use something more high-level (hint hint)"))

(defun spit-%dump-meta ()
  "Do ‘(dump-meta)’, prettify, save title, and cache some metainfo.
The title replaces the filename on the first line."
  (interactive)
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
      (flet ((orange (s) (spit--orange (concat s ":")))
             (nl () "\n\n"))
        (let ((standard-output (current-buffer))
              (face (list :inherit 'variable-pitch
                          :foreground "red")))
          (insert
           (orange "filename") (format " %S" fn)
           (nl)
           (orange "lang") (format " %S" lang)
           (nl)
           (orange (format
                    "invitations (%d cat / %d ent)"
                    (length invitations)
                    (length (apply 'append (mapcar 'cdr invitations)))))
           "\n")
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
          (flet ((out (x) (let ((blurb (format "%s" x))
                                (obj (symbol-value x)))
                            (insert "\n" (orange blurb) "\n")
                            (pp obj)
                            (unless (bolp)
                              (newline))))
                 (hsdp-maybe (h) (when (search-forward
                                        (format "\n%s:\n(" h)
                                        nil t)
                                   (forward-char -1)
                                   (save-excursion
                                     (spit--hsdp))
                                   (kill-sexp 1)
                                   (when (eolp)
                                     (delete-char 1)))))
            (mapc 'out '(vars settings copying titlepage toc))
            (goto-char (point-min))
            (hsdp-maybe 'copying)
            (hsdp-maybe 'titlepage)))
        (spit--cache 'invitations invitations)
        (spit--cache 'settings settings)
        (spit--cache)))))

(defun spit-%dump-index ()
  "Do ‘(dump-index)’, prettify, and cache some metainfo."
  (interactive)
  (spit--do nil 'dump-index)
  (save-excursion
    (let ((node-name (unless (spit--cache 'node-name)
                       (make-hash-table :test 'eq)))
          (tweaks (or (spit--cache 'tweaks)
                      (make-hash-table :test 'eq)))
          t-acc nid)
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
                  (spit--vp name)
                  (cond (etc (format " [+%d]" (length etc)))
                        (t "")))
          (setq t-acc (append (reverse etc) t-acc))
          (puthash nid t-acc tweaks)
          (when node-name
            (puthash nid (spit--vp name) node-name))))
      (spit--cache 'tweaks tweaks)
      (when node-name
        (spit--cache 'node-name node-name))
      (spit--cache))))

(defun spit-%show-labels ()
  "Do ‘(show-labels)’."
  (interactive)
  (spit--do nil 'show-labels))

(defun spit-%dump-dts ()
  "Do ‘(dump-dts)’ and prettify.
Report unhandled elements."
  (interactive)
  (spit--do nil 'dump-dts)
  (let ((cache (spit--cache 'node-name)))
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
               (ht (make-hash-table :test 'eq))
               (pos -1)
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
              (puthash (incf pos) term ht)
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
                                          ", ")))))
          (spit--cache (intern (concat "dts:" name)) ht))
        (newline))
      (spit--check-unhandled)
      (spit--cache))))

(defun spit-%dump-s-tree ()
  "Do ‘(dump-s-tree)’, prettify and cache some metainfo.
Report unhandled elements."
  (interactive)
  (spit--do nil 'dump-s-tree)
  (let ((node-name (or (spit--cache 'node-name)
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
          (puthash nid pretty node-name)
          (insert (format fmt nid what prefix) pretty)))
      (delete-region (point) (line-end-position)))
    (spit--check-unhandled)
    (goto-char p)
    (spit--cache 'node-name node-name)
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
  (set (make-local-variable 'spit--cur) 0)
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
                       [(tab)]       forward-button
                       [(backtab)]   backward-button
                       "t"           toggle-truncate-lines
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
                       "\M-d"    spit-%dump-dts
                       "\M-s"    spit-%dump-s-tree)))
          (suppress-keymap m)
          (while plist
            (define-key m (car plist) (cadr plist))
            (setq plist (cddr plist)))
          m)))

(provide 'spit)

;;; spit.el ends here
