;; -*- lexical-binding: t -*-
;;; surround.el --- Functions for inserting, deleting, and changing pairs like vim surround.

;;; Commentary:
;;
;; Provides functions for working with pairs like parentheses and quotes, including wrapping
;; text, deleting pairs, marking pairs or within them, and changing them.  This is similar to
;; vim's surround plugin.

;;; Code:

;; As a convention, we deal with pairs as strings, not characters.

(defvar surround-pairs
  ;; We only need pairs here where the left and right are different, though shortcuts are
  ;; created for each entry which is why quotes are here.  The default behavior is to insert
  ;; the same char at both ends.
  ;;
  ;; The keymap is created from this, so if you modify this after loading the package, either
  ;; recreate the keymap or restart Emacs.
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

    ;; Should we use defalias to create functions with names like mark-{ ?  Seems kind of
    ;; weird.  How does which-key get its names?
    ;;
    ;; Note that we're using lexical-binding, enabled by the comment at the top of the file,
    ;; which makes lambdas closures, so `left' and `right' are captured.

    (dolist (pair surround-pairs)
      (let* ((left  (car pair))
               (right (cdr pair)))
          (define-key map left (lambda () (interactive) (surround-mark left)))
          (unless (string= left right)
            (define-key map right (lambda () (interactive) (surround-mark right))))))
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
    ;;  (surround--op-mark bounds)))
    (kill-region (car bounds) (cdr bounds))))

;;;###autoload
(defun surround-insert (char)
  "Surrounds region or current symbol with a pair defined by CHAR."
  (interactive
   (list (char-to-string (read-char "character: "))))
  (let* ((pair (surround--make-pair char))
         (left  (car pair))
         (right (cdr pair))
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                 (bounds-of-thing-at-point 'symbol))))
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

If CHAR is either a left or right character in surround-pairs,
that pair is returned.  Otherwise a cons cell is returned made of
\(CHAR . CHAR)."
  (or (assoc char surround-pairs)
      (rassoc char surround-pairs)
      (cons char char)))


(defun surround--get-pair-bounds (char ends)
  "Return bounds of text surrounded by CHAR, including CHAR.

The ENDS parameter can be 'outer to include the pairs, 'inner
to include the region within the pairs, or 'auto
which defaults to include the pairs unless CHAR is a
right character in surround-pairs."
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

  ;; If the left and right are not the same, like ( and ), then we need a complex search that
  ;; handles nesting.  For example if point is on "c" below, we need to select the entire
  ;; expression and not just the closest parens to "c".
  ;;
  ;;     (a (b) c (d) e)
  ;;
  ;; Otherwise the open and close are the same and we won't support nesting.  We'll use a
  ;; simpler technique.

  (cond ((string= left right)
         (cons (surround--find-char left -1) (surround--find-char left 1)))
        (t
         (cons (surround--find-char-nestable left right -1) (surround--find-char-nestable right left 1)))
         ))


(defun surround--find-char (char dir)
  "Move point to CHAR in given direction.

DIR must be -1 to search backwards and 1 to search forward."
  ;; This is barely a wrapper around search-forward, but we do want our own user-error.
  (save-excursion
  (if (search-forward char nil t dir)
      (point)
    (user-error "Did not found pair of %s" char))))


(defun surround--find-char-nestable (char other dir)
  "Return the position of the closest, unnested CHAR.

OTHER is the opposite pair character for CHAR.  For example if
CHAR is '(' then OTHER would be ')'.  DIR must be -1 to search
backwards and 1 to search forward."
  ;; This searches in a single direction (-1 or 1) for `char`.  When searching forward for the
  ;; end of a pair of parens, we'd want to find the `char` ")".  We need to skip nested pairs,
  ;; so we watch for "(" which we call `other'.
  ;;
  ;; The only good algorithm I know is to walk forward to the next of either character and
  ;; count the nesting level.  Since we are expecting to be inside, we start at 1.  When we
  ;; reach 0, we've found the end of the current level.

  (save-excursion
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
          (goto-char pos)))
      (point))))


(defun surround--shrink (bounds)
  "Shrink BOUNDS from outer to inner."
  (cons (1+ (car bounds)) (1- (cdr bounds))))


(defun surround--distance (p1 p2)
  "Return the absolute distance between the positions P1 to P2."
  ;; REVIEW: Aren't p1 and p2 positions which can't be negative?
  ;;
  ;; Also, using most-positive-fixnum does simplify other calling code, but perhaps we can
  ;; reformat the other code to not require this.
  (let* ((a1 (abs p1))
         (a2 (abs (or p2 most-positive-fixnum))))
    (- (max a1 a2) (min a1 a2))))


(provide 'surround)
;;; surround.el ends here
