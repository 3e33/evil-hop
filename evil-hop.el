;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;          Evil-Hop           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Description: Inspired by Vim's Easy Motion, brings similar functionality to Emacs
;; Version: 0.1
;; Copyright (C) 2019 3e33 @ http://github.com/3e33
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "Convert a given number in base 10 to another base
for example 10 -> (1 0 1 0) in binary."
  (if (< number base)
      (list number)
    (append (evil-hop-convert-base (/ number base) base)
            (list (% number base)))))

(defun evil-hop-reorder-list (selectors selection)
  "Re-order a given list by supplying your own ordering
and return the new list.
'(0 3 2 2) + '(a b c d) = '(a d c c)."
  (let (result)
    (dolist (position selectors result)
      (setq result (append result (list (nth position selection)))))))

(defun evil-hop-get-overlay-keys (n-positions key-candidates)
  "Make a list of keys to use for the overlay."
  (let* ((base (length key-candidates))
         (power (floor (log n-positions base)))
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
  "Pads a list with extra zeroes, i.e, list goes in, more zeroes come out."
  (while (< (length l) power)
    (setq l (cons 0 l)))
  l)

(defun evil-hop-entry (input)
  "Entry point to evil-hop-hop that takes any key input and turns it into a command."
  (interactive "k")
  (evil-hop-hop (key-binding input)))

(defun evil-hop-hop (command)
  "Hop to it."
  (let* ((inhibit-quit t)
         (jump-alist (let* ((locations (evil-hop-get-position-list command))
                            (keys (evil-hop-get-overlay-keys (length locations)
                                                             evil-hop-highlight-keys)))
                       (evil-hop-make-overlay-alist locations keys)))
         (jump-sequence "")
         (current-prefix-arg nil))
    (evil-hop-show-jump-map (evil-hop-filter-close-positions jump-alist))
    (while (> (length jump-alist)
              1)
      (setq jump-sequence (concat jump-sequence (string (read-char-exclusive))))
      (setq jump-alist (seq-filter #'(lambda (elt) (string-prefix-p jump-sequence (apply 'concat (car elt))))
                                   jump-alist))
      (evil-hop-remove-overlays)
      (evil-hop-show-jump-map jump-alist))
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

(defun evil-hop-show-jump-map (jump-alist)
  "Create and show the jump map."
  (dotimes (i (length jump-alist))
    (let* ((position (nth 1 (nth i jump-alist)))
           (key-combo (nth 0 (nth i jump-alist)))
           (combo-part (evil-hop-get-combo-part key-combo 2))
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
  "Clean up jump-alist positions that are too close together."
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
  "Run the given command until the start or end of the window
and return a list of positions that have been produced."
  (let ((locations '()))
    (save-excursion
      (save-restriction
        (narrow-to-region (window-start) (window-end))
        (while (progn
                 (ignore-errors
                   (command-execute command)
                   (if (equal (car (last locations)) (point))
                       nil
                     (setq locations (append locations (list (point))))))))))
    locations))

(defun evil-hop-make-overlay (location text face &optional after-string)
  "Make an overlay at the given location and give it
some properties such as a highlight and changing the text
for key press decisions."
  (let ((overlay (make-overlay location (+ location (length text)))))
    (overlay-put overlay 'display text)
    (overlay-put overlay 'face face)
    (and after-string
         (overlay-put overlay 'after-string after-string))))

(defun evil-hop-get-eol-after-point (point)
  "Get the position of the end of line after the provided point."
  (save-excursion
    (goto-char point)
    (end-of-line)
    (point)))

(defun evil-hop-remove-overlays ()
  "Clears the jump choice overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'face 'evil-hop-highlight))

(defun evil-hop-slice (list start end)
  "Returns a slice of given list."
  (if (< (length list) end)
      list
    (seq-subseq list start end)))

(defun evil-hop-get-combo-part (key-combo n)
  "From a list of keys for a position, get n of them as a single string."
  (apply 'concat (evil-hop-slice key-combo 0 n)))

(provide 'evil-hop)
