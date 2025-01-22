;;; surround.el --- Easily add/delete/change parens, quotes, and more  -*- lexical-binding: t -*-


;; Copyright (C) 2023 Michael Kleehammer
;;
;; Author: Michael Kleehammer <michael@kleehammer.com>
;; Maintainer: Michael Kleehammer <michael@kleehammer.com>
;; URL: https://github.com/mkleehammer/surround
;; Version: 1.0.3
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is NOT part of GNU Emacs.
;;
;; MIT License
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Commentary:
;;
;; Provides functions for working with pairs like parentheses and quotes,
;; including wrapping text, deleting pairs, marking pairs or within them, and
;; changing them.  This is similar to vim's surround plugin.
;;
;; For a detailed introduction and example usage, see the readme file at
;; https://github.com/mkleehammer/surround
;;
;; This package provides a keymap with functions for working with pairs of
;; delimiters like (), {}, quotes, etc.  If you are using use-package, the
;; easiest way to configure is like so:
;;
;;    (use-package surround
;;     :ensure t
;;     :bind-keymap ("M-'" . surround-keymap))
;;
;; This will bind M-', but obviously choose the key you prefer.  C-c s may be a
;; good option.  The keymap will have the following keys bound:
;;
;; - s :: Surrounds the region or current symbol with a pair
;; - i :: Selects the text within a pair (inner select / mark)
;; - o :: Selects the pair and text within (outer select / mark)
;; - k :: Kills text within a pair (inner kill)
;; - K :: Kills the pair and text within (outer kill)
;; - d :: Deletes the pair, but leaves text within
;; - c :: Change the pair for another (e.g. change [1] to {1})
;;
;; For convenience, the 'i' and 'k' commands perform an outer select or kill if
;; given a closing pair character, such as ')'.  That is, "M-' i (" will perform
;; an inner mark but "M-' i )" will perform an outer mark.  When the left and
;; right pair characters are the same, such as when using quotes, the commands
;; always perform the inner command.
;;
;; The keymap also has shortcut commands for marking some pairs, bound to just
;; the pair characters themselves.  For example, "M-' (" will mark within
;; parentheses.  These all work like the 'i' command, so they default to inner
;; but will work as an outer select if a right / closing pair character is
;; entered:
;;
;; Inner: ( { [ < ' ` "
;; Outer: ) } ] >

;;; Code:

(defvar surround-pairs
  ;; We only need pairs here where the left and right are different, though
  ;; shortcuts are created for each entry which is why quotes are here.  The
  ;; default behavior is to insert the same char at both ends.
  ;;
  ;; The keymap is created from this, so if you modify this after loading the
  ;; package, either recreate the keymap or restart Emacs.
  '(("("  . ")")
    ("{"  . "}")
    ("["  . "]")
    ("<"  . ">")
    ("'"  . "'")
    ("`"  . "`")
    ("\"" . "\"")))

;;;###autoload
(defun surround-make-keymap ()
  "Create and return the keymap for surround."
  (let ((map (make-sparse-keymap)))
    (define-key map "s" 'surround-insert)
    (define-key map "k" 'surround-kill)
    (define-key map "K" 'surround-kill-outer)
    (define-key map "i" 'surround-mark)
    (define-key map "o" 'surround-mark-outer)
    (define-key map "d" 'surround-delete)
    (define-key map "c" 'surround-change)

    ;; Note that we're using lexical-binding, enabled by the comment at the top
    ;; of the file, which makes lambdas closures, so `left' and `right' are
    ;; captured.

    (dolist (pair surround-pairs)
      (let* ((left  (car pair))
             (right (cdr pair)))
        ;; define-key supports binding keys to a cons cell of (string .
        ;; function). which-key will read the string out of the cons cell and
        ;; display it instead of the default "function" which it uses for
        ;; lambdas. See
        ;; https://github.com/justbur/emacs-which-key?tab=readme-ov-file#keymap-based-replacement
        ;; for details.
        (define-key map left (cons (concat "mark-" left)
                                   (lambda () (interactive) (surround-mark left))))
        (unless (string= left right)
          (define-key map right (cons (concat "mark-" right)
                                      (lambda () (interactive) (surround-mark right)))))))
    map))


(defvar surround-keymap (surround-make-keymap)
  "Keymap of surround functions.")

;;;###autoload
(defun surround-mark-outer (char)
  "Mark a pair including the pair characters defined by CHAR."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'outer)))
    (surround--op-mark bounds)))

;;;###autoload
(defun surround-mark-inner (char)
  "Mark a pair including the pair characters defined by CHAR."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'inner)))
    (surround--op-mark bounds)))

;;;###autoload
(defun surround-mark (char)
  "Mark text in a pair.  If CHAR is a closing character, mark pair also."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'auto)))
    (surround--op-mark bounds)))

(defun surround--op-mark (bounds)
  "Internal function to set the active region to the cons cell BOUNDS."
  (push-mark (point) t)
  (goto-char (car bounds))
  (push-mark (point) t t)
  (goto-char (cdr bounds)))

;;;###autoload
(defun surround-kill-outer(char)
  "Kill pair defined by CHAR and text within it."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'outer)))
    (kill-region (car bounds) (cdr bounds))))

;;;###autoload
(defun surround-kill-inner(char)
  "Kill text within pair defined by CHAR."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'inner)))
    (kill-region (car bounds) (cdr bounds))))

;;;###autoload
(defun surround-kill (char)
  "Kill text within in a pair.  If CHAR is a closing character, mark pair also."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'auto)))
    (kill-region (car bounds) (cdr bounds))))

;;;###autoload
(defun surround-insert (char)
  "Surrounds region or current symbol with a pair defined by CHAR."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let* ((pair (surround--make-pair char))
         (left  (car pair))
         (right (cdr pair))
         (bounds (surround--infer-bounds t)))
    (save-excursion
      (goto-char (cdr bounds))
      (insert right)
      (goto-char (car bounds))
      (insert left))
    (if (eq (car bounds) (point))
        (forward-char))))

;;;###autoload
(defun surround-delete (char)
  "Deletes a pair defined by CHAR, leaving the contents."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let ((bounds (surround--get-pair-bounds char 'outer)))
    (save-excursion
      (goto-char (cdr bounds))
      (delete-char -1)
      (goto-char (car bounds))
      (delete-char 1))))

;;;###autoload
(defun surround-change (old new)
  "Replace OLD pair with NEW."
  (interactive
   (list (char-to-string (read-char "replace: "))
         (char-to-string (read-char "with: "))))
  (message "from %s to %s" old new)
  (let ((bounds (surround--get-pair-bounds old 'outer))
        (newpair (surround--make-pair new)))
    (save-excursion
      (goto-char (cdr bounds))
      (delete-char -1)
      (insert (cdr newpair))
      (goto-char (car bounds))
      (delete-char 1)
      (insert (car newpair)))
    (if (eq (car bounds) (point))
        (forward-char))))


(defun surround--make-pair (char)
  "Return a cons cell of (left . right) defined by CHAR.

If CHAR is either a left or right character in `surround-pairs',
that pair is returned.  Otherwise a cons cell is returned made of
\(CHAR . CHAR)."
  (or (assoc char surround-pairs)
      (rassoc char surround-pairs)
      (cons char char)))


(defun surround--get-pair-bounds (char ends)
  "Return bounds of text surrounded by CHAR, including CHAR.

The ENDS parameter can be \\='outer to include the pairs, \\='inner
to include the region within the pairs, or \\='auto
which defaults to include the pairs unless CHAR is a
right character in `surround-pairs'."
  (let* ((pair (surround--make-pair char))
         (left (car pair))
         (right (cdr pair))
         (bounds (surround--pair-bounds left right)))
    (if (or (eq ends 'inner)
            (and (eq ends 'auto)
                 (string= char left)))
        (surround--shrink bounds)
      bounds)))


(defun surround--pair-bounds (left right)
  "Return the bounds of the enclosing pair LEFT and RIGHT."

  ;; If the left and right are not the same, like ( and ), then we need a
  ;; complex search that handles nesting.  For example if point is on "c" below,
  ;; we need to select the entire expression and not just the closest parens to
  ;; "c".
  ;;
  ;;     (a (b) c (d) e)
  ;;
  ;; Otherwise the open and close are the same and we won't support nesting.
  ;; We'll use a simpler technique.
  (if (string= left right)
      (cons (surround--find-char left -1) (surround--find-char left 1))
    (cons
     (surround--find-char-nestable left right -1)
     (surround--find-char-nestable right left 1))))


(defun surround--find-char (char dir)
  "Move point to CHAR in given direction.

DIR must be -1 to search backwards and 1 to search forward."
  ;; This is barely a wrapper around search-forward, but we do want our own
  ;; user-error.
  (save-excursion
  (if (search-forward char nil t dir)
      (point)
    (user-error "Did not found pair of %s" char))))


(defun surround--find-char-nestable (char other dir)
  "Return the position of the closest, unnested CHAR.

OTHER is the opposite pair character for CHAR.  For example if
CHAR is ( then OTHER would be ).  DIR must be -1 to search
backwards and 1 to search forward."
  ;; This searches in a single direction (-1 or 1) for `char`.  When searching
  ;; forward for the end of a pair of parens, we'd want to find the `char` ")".
  ;; We need to skip nested pairs, so we watch for "(" which we call `other'.
  ;;
  ;; The only good algorithm I know is to walk forward to the next of either
  ;; character and count the nesting level.  Since we are expecting to be
  ;; inside, we start at 1.  When we reach 0, we've found the end of the current
  ;; level.

  (save-excursion
    (if (looking-at (regexp-quote char))
        ;; We're already on the character we're looking for.  If it is the
        ;; opening paren (dir=-1), then this is the position we want.  If it is
        ;; the closing paren (dir=1), then we want the *next* point since the
        ;; selection is exclusive.
        (when (= dir 1)
          (forward-char 1))

      ;; The algorithm below is going to assume we are *inside* a pair (level =
      ;; 1), so if we're on an opening paren step in.  Otherwise the algorithm
      ;; below will find the opening paren we're already on and count it as
      ;; another open.
      (if (and (looking-at (regexp-quote other)) (= dir 1))
          (forward-char dir))

      (let ((level 1))                      ; level of nesting
        (while (> level 0)
          (let* ((current (point))
                 (charpos (search-forward char nil t dir))
                 (otherpos (progn
                             (goto-char current)
                             (search-forward other nil t dir)))
                 (chardist (surround--distance current charpos))
                 (otherdist (surround--distance current otherpos))
                 (pos (if (< chardist otherdist) charpos otherpos))
                 (diff (if (< chardist otherdist) -1 1)))

            (if (null charpos)
                (user-error "Did not find %s" char))

            (setq level (+ level diff))
            (goto-char pos)))))

    (point)))

(defun surround--infer-bounds (&optional strict)
  "Infer the bounds.

With an active region they are those of the region. If looking at
a symbol they are those of the symbol. For empty lines the
current point is considered both start and end.

If STRICT is t, a user error is signaled when the bounds couldn't
be inferred."
  (cond
   ((use-region-p)
    (cons (region-beginning) (region-end)))
   ((thing-at-point 'symbol)
    (bounds-of-thing-at-point 'symbol))
   ((save-excursion
      (beginning-of-line)
      (looking-at-p "[ \t]*$"))
    (cons (point) (point)))
   (strict
    (user-error "No bounds can be inferred"))))

(defun surround--shrink (bounds)
  "Shrink BOUNDS from outer to inner."
  (cons (1+ (car bounds)) (1- (cdr bounds))))


(defun surround--distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  ;; REVIEW: Aren't p1 and p2 positions which can't be negative?
  ;;
  ;; Also, using most-positive-fixnum does simplify other calling code, but
  ;; perhaps we can reformat the other code to not require this.
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))


(provide 'surround)
;;; surround.el ends here
