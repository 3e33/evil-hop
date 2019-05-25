;;; evil-hop.el --- Hop around the buffer using key combinations

;; Author: 3e33
;; Version: 0.42
;; Package-Requires: ((emacs "24") (evil "1.2"))
;; Keywords: convenience, tools, abbrev
;; URL: https://github.com/3e33/evil-hop

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License.

;;; Commentary:
;;
;; Evil-Hop allows you to quickly jump around a buffer using generated key
;; combinations.  For example you can bind the entry function to your space key
;; and then press "<SPC>w" to attach a highlighted key sequence to each word jump.
;; You can then press this key sequence to jump to that word.
;; If you have used Vim before, this package is similar to Vim's EasyMotion plugin.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Code:

(defcustom evil-hop-highlight-keys
  "asdghklqwertyuiopzxcvbnmfj"
  "Defines the keys used for highlighting jump choices."
  :type '(string)
  :group 'evil-hop)

(defface evil-hop-highlight
  '((t (:foreground "#fabd2f")))
  "Face used for highlighting jump choices."
  :group 'evil-hop)

(defun evil-hop-convert-base (number base)
  "Convert a given NUMBER in base 10 to another BASE for example 10 -> (1 0 1 0) in binary."
  (if (< number base)
      (list number)
    (append (evil-hop-convert-base (/ number base) base)
            (list (% number base)))))

(defun evil-hop-reorder-list (selectors selection)
  "Re-order a given list by supplying your own ordering and return the new list.
'(0 3 2 2) + '(a b c d) = '(a d c c).
Argument SELECTORS a list of integers that can be usd for the nth function.
Argument SELECTION a list."
  (let (result)
    (dolist (position selectors result)
      (setq result (append result (list (nth position selection)))))))

(defun evil-hop-get-overlay-keys (n-positions key-candidates)
  "Make a list of keys to use for the overlay.
Argument N-POSITIONS a positive integer to describe the number of positions to generate keys for.
Argument KEY-CANDIDATES a list of strings to use as key candidates."
  (let* ((base (length key-candidates))
         (power (ceiling (log n-positions base)))
         (block (expt base power))
         (split (floor (- block
                          (* (/ n-positions block)
                             (expt base (- power 1))))))
         (shift (* base split))
         (keys (mapcar 'string key-candidates))
         (result '()))
    (if (= n-positions (expt base power))
        (dotimes (i n-positions)
          (setq result
                (append result
                        (list (evil-hop-reorder-list
                               (evil-hop-zero-padding (evil-hop-convert-base i base) power)
                               keys)))))
      (dotimes (i split)
        (setq result
              (append result
                      (list (evil-hop-reorder-list
                             (evil-hop-zero-padding (evil-hop-convert-base i base) power)
                             keys)))))
      (dotimes (i (- n-positions split))
        (setq result
              (append result
                      (list (evil-hop-reorder-list
                             (evil-hop-convert-base (+ i shift) base)
                             keys))))))
    result))

(defun evil-hop-zero-padding (l power)
  "Pads a list with extra zeroes, i.e, list goes in, more zeroes come out.
Argument L list.
Argument POWER number of zeroes to pad with according to the power of N being described by L."
  (while (< (length l) power)
    (setq l (cons 0 l)))
  l)

;;;###autoload
(defun evil-hop-entry (input)
  "Entry point to evil-hop-hop that takes any key INPUT and turns it into a command."
  (interactive "k")
  (let ((command (key-binding input)))
    (unless (equal command 'evil-hop-entry)
      (evil-hop-hop command))))

;;;###autoload
(defun evil-hop-hop (command)
  "Hop to it.
Argument COMMAND a Lisp function."
  (let* ((inhibit-quit t)
         (jump-alist (let* ((locations (evil-hop-get-position-list command))
                            (keys (evil-hop-get-overlay-keys (length locations)
                                                             evil-hop-highlight-keys)))
                       (evil-hop-make-overlay-alist locations keys)))
         (jump-sequence "")
         (current-prefix-arg nil))
    (evil-hop-show-jump-map (evil-hop-filter-close-positions jump-alist) (length jump-sequence))
    (while (> (length jump-alist)
              1)
      (setq jump-sequence (concat jump-sequence (string (read-char-exclusive))))
      (setq jump-alist (seq-filter #'(lambda (elt) (string-prefix-p jump-sequence
                                                                    (apply 'concat (car elt))))
                                   jump-alist))
      (evil-hop-remove-overlays)
      (evil-hop-show-jump-map jump-alist (length jump-sequence)))
    (evil-hop-remove-overlays)
    (unless (equal jump-alist nil)
      (setq current-prefix-arg (nth 2 (car jump-alist)))
      (call-interactively command))))

(defun evil-hop-make-overlay-alist (locations keys)
  "Return an alist of the jump locations and associated keys.
The list looks like this '(KEY LOCATION REPEATS)
KEY is a string with the key combination needed to jump to LOCATION,
REPEATS is the number of times the original command was repeated to get to this
location."
  (let ((result '()))
    (dotimes (i (length locations))
      (setq result (append result (list `(,(nth i keys) ,(nth i locations) ,(+ 1 i))))))
    result))

(defun evil-hop-show-jump-map (jump-alist combo-length)
  "Create and show the jump map.
Argument JUMP-ALIST an alist describing the jumps.
Argument COMBO-LENGTH max length of key combination."
  (dotimes (i (length jump-alist))
    (let* ((position (nth 1 (nth i jump-alist)))
           (key-combo (nth 0 (nth i jump-alist)))
           (slice-min (* (/ combo-length 2) 2))
           (slice-max (+ slice-min 2))
           (combo-part (evil-hop-get-combo-part key-combo slice-min slice-max))
           (line-end (evil-hop-get-eol-after-point position)))
      (evil-hop-make-overlay position
                             combo-part
                             'evil-hop-highlight
                             (if (= position line-end)
                                 (string 10)
                               (if (= (+ position
                                         (- (length combo-part)
                                            1))
                                      line-end)
                                   (string 10)))))))

(defun evil-hop-filter-close-positions (jump-alist)
  "Clean up JUMP-ALIST positions that are too close together."
  (let ((i 0))
    (while (< (+ 1 i)
              (length jump-alist))
      (if (<= (abs (- (nth 1 (nth i jump-alist))
                      (nth 1 (nth (+ 1 i) jump-alist))))
              1)
          (setcdr (nthcdr i jump-alist)
                  (nthcdr (+ 2 i) jump-alist))
        (setq i (+ 1 i))))
    jump-alist))

(defun evil-hop-get-position-list (command)
  "Run the given COMMAND until the start or end of the window and return a list of positions that have been produced."
  (let ((locations '())
        (saved-evil-state evil-state))
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end))
        (setq evil-state 'normal)
        (while (progn
                 (ignore-errors
                   (command-execute command)
                   (if (equal (car (last locations)) (point))
                       nil
                     (setq locations (append locations (list (point))))))))
        (setq evil-state saved-evil-state)))
    locations))

(defun evil-hop-make-overlay (location text face &optional after-string)
  "Make an overlay at the given LOCATION and give it some properties such as a highlight and changing the TEXT for key press decisions.
Argument FACE emacs face."
  (let ((overlay (make-overlay location (+ location (length text)))))
    (overlay-put overlay 'display text)
    (overlay-put overlay 'face face)
    (when after-string
      (overlay-put overlay 'after-string after-string))))

(defun evil-hop-get-eol-after-point (point)
  "Get the position of the end of line after the provided POINT."
  (save-excursion
    (goto-char point)
    (end-of-line)
    (point)))

(defun evil-hop-remove-overlays ()
  "Clears the jump choice overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'face 'evil-hop-highlight))

(defun evil-hop-slice (list start end)
  "Returns a slice of given LIST.
Argument START slice start.
Argument END slice end."
  (let ((length (length list)))
    (if (> end length)
        (seq-subseq list start length)
      (seq-subseq list start end))))

(defun evil-hop-get-combo-part (key-combo a b)
  "From a list of keys for a position, get A to B of them as a single string.
Argument KEY-COMBO list describing the key combination."
  (apply 'concat (evil-hop-slice key-combo a b)))

(provide 'evil-hop)

;;; evil-hop.el ends here
