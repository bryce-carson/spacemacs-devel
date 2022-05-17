;;; spacemacs-buffer-dark-draw-banner.el --- Experimental Dark Draw banners for Spacemacs home buffer -*- lexical-binding: t; -*-

;; Copyright © 2022  Bryce Carson

;; Author: Bryce Carson <bryce.a.carson@gmail.com>
;; Keywords: lisp, spacemacs, animation, text
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; NOTE: TODO: FIXME: HACK: Alternative to Dark Draw
;; Rexpaint is much friendlier, it just doesn't support animation out of the box, however animation can be achieved using multiple layers!
;; png2rex_rs -> will convert a PNG image to XP format for Rexpaint editing. Instant ASCII art!
;; aaron-santos/rockpick -> will convert XP to a Clojure data format
;; clojure-emacs/parseclj -> will convert Clojure data format to Emacs Lisp!
;;
;; NOTE:
;; 1. Create a drawing with Dark Draw 2: https://github.com/devottys/darkdraw
;; 1.1 Alternatively, download an example drawing (without animation):
;;     wget https://raw.githubusercontent.com/devottys/darkdraw/master/samples/arrows.ddw
;; 2. Sort the JSONL file by X and Y keys:
;;     jq -s -c 'sort_by(.y, .x)[]' ~/arrows.ddw > ~/arrows-sorted.ddw
;; 3. Use `arrows-sorted.ddw', or any sorted DDW file made by Dark Draw 2 as `dotspacemacs-startup-banner' in the `let*' form.

;; TODO: Put a description of the package here


;;; Code:
;;; Libraries

;; Available on MELPA
(require 'jsonl)

;;; NOTE: Variables
;; `dotspacemacs-startup-banner'
(defvar dsm--ddw-banner nil)
(defvar dsm--ddw-banner-frame-times '(0))
(defvar dsm--ddw-banner-x-max '(0))
(defvar dsm--ddw-banner-y-max '(0))
(defvar dsm--ddw-banner-list nil)


;;; NOTE: Functions

;; Insert the rectangle of whitespace to permit moving point to a coordinate.
(defun ddw--insert-whitespace-rectangle (x y)
  "Insert the necessary amount of whitespace into the buffer.

The amount of whitespace inserted is determined by the bounds of
the drawing to be inserted, its width in X characters and its
height in Y characters."
  (goto-char (point-min))
  (dotimes (var y)
    (insert-char #x20 (1+ x))
    (goto-char (point-max))
    (insert-char #x0A)
    (goto-char (point-max)))
  (insert-char #x20 (1+ x))
  (goto-char (point-min)))

(defun ddw--insert-dot-above-grid (x y)
  "Insert the necessary amount of #x02D9 (DOT ABOVE in Unicode) into the buffer.

The amount of whitespace inserted is determined by the bounds of
the drawing to be inserted, its width in X characters and its
height in Y characters."
  (goto-char (point-min))
  (dotimes (var y)
    (insert-char #x02D9 (1+ x))
    (goto-char (point-max))
    (insert-char #x0A)
    (goto-char (point-max)))
  (insert-char #x02D9 (1+ x))
  (goto-char (point-min)))

;; DEPRECATED:
;; (defun ddw--char-coordinate-to-buffer-position (x y)
;;   "Convert the character coordinate pair X and Y to a buffer position."
;;   (+ (1+ x) (* y dsm--ddw-banner-x-max)))

;; (defun ddw--goto-char-coordinate (x y)
;;   "Move point to the position in the buffer that corresponds to
;; the X and Y coordinate pair.

;; The buffer should be restricted to a rectangular area of
;; whitespace such that `point-max' would return (* X Y)."
;;   (goto-char (point-min))
;;   (goto-char (ddw--char-coordinate-to-buffer-position x y)))

(defun ddw--goto-line-then-forward-char (x y)
  "Go to the position in the buffer specified by X and Y
coordinates. First, go to `point-min', then advance point to the
line Y, then move point forward X characters.

This function is more reliable than calculating a position in a
buffer and moving point to that position, as changes in the
number of characters in the buffer will not affect where in the
buffer this function _should_ move point to."
  (goto-char (point-min))
  (goto-line (1+ y))
  (forward-char x))

(defun ddw--sort-list (elt-1 elt-2)
  (let ((elt-1-x (alist-get 'x elt-1))
        (elt-1-y (alist-get 'y elt-1))
        (elt-2-x (alist-get 'x elt-2))
        (elt-2-y (alist-get 'y elt-2)))
    (and (< elt-1-y elt-2-y)
         (< elt-1-x elt-2-x))))


;;; NOTE: TODO: Procedure

;; TODO: refactor this. It's dirtyyy!
;; TODO: barf out the inner `let*' after testing is done.
(let* ((dotspacemacs-startup-banner "/home/brycecarson/6x5.ddw"))
  ;; TODO: FIXME: remove this when testing is done.
  ;; For testing usage
  (setq dsm--ddw-banner nil)
  (setq dsm--ddw-banner-frame-times '(0))
  (setq dsm--ddw-banner-x-max '(0))
  (setq dsm--ddw-banner-y-max '(0))
  (setq dsm--ddw-banner-list nil)

  ;; Bind the file-name if the selected banner is a Dark Draw banner.
  (let* ((ddw-banner-file (if (and (stringp dotspacemacs-startup-banner)
                                   (file-exists-p dotspacemacs-startup-banner)
                                   (string= (file-name-extension dotspacemacs-startup-banner) "ddw"))
                              dotspacemacs-startup-banner)))

    ;; If the selected banner is a Dark Draw banner get frame timing and drawing size information.
    (if ddw-banner-file (progn (setq dsm--ddw-banner-list (jsonl-read-from-file ddw-banner-file))
                               (dolist (jsonl-value dsm--ddw-banner-list)
                                 (let-alist jsonl-value
                                   (if (and .id (string= .type "frame"))
                                       (setq dsm--ddw-banner-frame-times `(,@dsm--ddw-banner-frame-times ,.duration_ms))
                                     (progn
                                       (setq dsm--ddw-banner-x-max (add-to-list 'dsm--ddw-banner-x-max
                                                                                (if (equal (length .text) 1)
                                                                                    .x
                                                                                  (if (> (length .text) 1)
                                                                                      (+ .x (length .text))
                                                                                    0))
                                                                                t))
                                       (setq dsm--ddw-banner-y-max (add-to-list 'dsm--ddw-banner-y-max .y t))

                                       ;; Get and then set text and colour information in a property list.
                                       (setq dsm--ddw-banner
                                             (if (not .frame)
                                                 ;; NOTE: add to the global frame.
                                                 (plist-put dsm--ddw-banner 'frame-global
                                                            (append (plist-get dsm--ddw-banner 'frame-global)
                                                                    (list jsonl-value)))

                                               ;; NOTE: adding to frames.
                                               (plist-put dsm--ddw-banner (intern (concat "frame-" .frame))
                                                          (append (plist-get dsm--ddw-banner (intern (concat "frame-" .frame)))
                                                                  (list jsonl-value)))))))))))

    ;; NOTE: sort the banner's elements.
    (setq dsm--ddw-banner
          (mapcar (lambda (elt)
                    (if (listp elt)
                        (sort elt #'ddw--sort-list)
                      elt))
                  dsm--ddw-banner))

    ;; Determine the size of the Dark Draw banner
    (setq dsm--ddw-banner-x-max (seq-max dsm--ddw-banner-x-max))
    (setq dsm--ddw-banner-y-max (seq-max dsm--ddw-banner-y-max))

    (save-excursion
      (let ((temporary-buffer (set-buffer (get-buffer-create (concat "dsm//ddw/test-buffer-" (number-to-string (random)))))))
        (defvar dsm//ddw-banner
          (loop for frame-index-in-list in (number-sequence 1 (let ((frames-count (length dsm--ddw-banner)))
                                                               (if (oddp frames-count)
                                                                   (1- frames-count)
                                                                 frames-count))
                                                           2)
               collect
               (progn
                 (erase-buffer)
                 (ddw--insert-whitespace-rectangle dsm--ddw-banner-x-max dsm--ddw-banner-y-max)
                 (dolist (value (nth frame-index-in-list dsm--ddw-banner))
                   (let-alist value
                     (ddw--goto-line-then-forward-char .x .y)
                     (insert (eval (cl-multiple-value-apply (lambda (&rest values)
                                                              `(propertize ,.text 'face '(,@values)))
                                                            (if (not (= (length .color) 0))
                                                                `(:foreground ,.color :drawing 't)
                                                              '(:drawing 't)))))
                     (delete-forward-char (length .text))))
                 (list (buffer-string))))
          "The list of lists of `buffer-string's which compose the frames of the Dark Draw 2 animation to be used as a Spacemacs banner.")
        (kill-buffer temporary-buffer)))))

;; Pseudocode
;; DONE:
;; 1. Read and parse Dark Draw file (jsonl-value)
;; 1.1. Convert structure of values into one list per frame
;; 1.2. Create a character vector
;; 1.3. Propertize the character vector with colour, weight, etc.
;; 1.4. Return a list of frames with each frame having a propertized character vector and timing fields
;;
;; TODO:
;; 2. Display each frame
;; 2.1. Narrow buffer to calculated, centered region
;; 2.2. Insert text
;; 2.3. Setup clock
;; 2.4. Setup hooks for performance consideration (don't let the clock tick when the Spacemacs buffer is not in a open window; 🤔 How does this interact with perspectives?)

(provide 'spacemacs-buffer-dark-draw-banner)
;;; spacemacs-buffer-dark-draw-banner.el ends here
