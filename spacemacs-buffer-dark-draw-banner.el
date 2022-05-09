;;; spacemacs-buffer-dark-draw-banner.el --- Experimental Dark Draw banners for Spacemacs home buffer                     -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Bryce Carson

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

;; TODO: Put a description of the package here


;;; Code:

(require 'jsonl)
(require 'core-spacemacs-buffer)

(defvar dsm--ddw-banner-x-max nil)
(defvar dsm--ddw-banner-y-max nil)
(defvar dsm--ddw-banner-frame-times nil)
(defvar dsm--ddw-banner nil)

;; TODO: barf out the inner `let*' after testing is done.
(let* ((dotspacemacs-startup-banner "/home/brycecarson/.spacemacs.d/core/banners/spacemacs-dark-draw-banner-001.ddw"))

  ;; Bind the file-name if the selected banner is a Dark Draw banner.
  (let* ((ddw-banner-file (if (and (stringp dotspacemacs-startup-banner)
                                   (file-exists-p dotspacemacs-startup-banner)
                                   (string= (file-name-extension dotspacemacs-startup-banner) "ddw"))
                              dotspacemacs-startup-banner)))

    ;; If the selected banner is a Dark Draw banner get frame timing and drawing size information.
    (if ddw-banner-file
        (defvar dotspacemacs--ddw-banner (jsonl-read-from-file ddw-banner-file)))
    (dolist (value dotspacemacs--ddw-banner)
      (let-alist value
        (if (and .id (string= .type "frame"))
            (setq dsm--ddw-banner-frame-times `(,@dsm--ddw-banner-frame-times ,.duration_ms))
          (progn
            ;; Get size information TODO: `add-to-list' is likely appropriate
            (setq dsm--ddw-banner-x-max (cons .x dsm--ddw-banner-x-max))
            (setq dsm--ddw-banner-y-max (cons .y dsm--ddw-banner-y-max))

            ;; Get and then set text and colour information in a property list.
            (setq dsm--ddw-banner
                  (if (not .frame)
                      ;; NOTE: add to the global frame.
                      (setq dsm-ddw-banner
                            (plist-put dsm--ddw-banner 'global-frame
                                  (cons (plist-get dsm--ddw-banner 'global-frame)
                                        value)))

                    ;; NOTE: adding to frames.
                    (setq dsm-ddw-banner
                          (plist-put dsm--ddw-banner (intern (concat "frame-" .frame))
                                     (cons (plist-get dsm--ddw-banner (intern (concat "frame-" .frame)))
                                           value)))))))))))

;; Determine the size of the Dark Draw banner
(setq dsm--ddw-banner-x-max (seq-max dsm--ddw-banner-x-max))
(setq dsm--ddw-banner-y-max (seq-max dsm--ddw-banner-y-max))

;; EXAMPLE DRAWING

(defconst ddw-banner-example--read-from-jsonl--formatted-by-hand
  ;; Frames
  (((id . "0") (type . "frame") (text . "") (color . "") (tags . []) (group . "") (duration_ms . 900))
   ((id . "1") (type . "frame") (text . "") (color . "") (tags . []) (group . "") (duration_ms . 250))

   ;; Drawing information not associated with any frames
   ((x . 0) (y . 0) (text . "(") (color . "blue")
    (tags . []) (group . ""))

   ((x . 1) (y . 0) (text . "spacemacs-banner") (color . "pink")
    (tags . []) (group . ""))

   ((x . 18) (y . 0) (text . "()") (color . "red")
    (tags . []) (group . ""))

   ((x . 1) (y . 1) (text . "(") (color . "orange")
    (tags . []) (group . ""))

   ((x . 2) (y . 1) (text . "insert-dark-draw-banner") (color . "pink")
    (tags . []) (group . ""))

   ((x . 26) (y . 1) (text . "\"core/banners/spacemacs-dark-draw-banner-001.ddw\"") (color . "green")
    (tags . []) (group . ""))

   ((x . 75) (y . 1) (text . ")") (color . "orange")
    (tags . []) (group . ""))

   ((x . 76) (y . 1) (text . ")") (color . "blue")
    (tags . []) (group . ""))

   ;; Drawing information associated with a frame
   ((x . 26) (y . 4) (text . "`") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 28) (y . 4) (text . "(:forge \"GitHub\"") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 28) (y . 5) (text . ":owner \"devottys\"") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 28) (y . 6) (text . ":repo \"darkdraw\"") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 44) (y . 6) (text . ")") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 28) (y . 4) (text . ":forge \"GitHub\"") (color . "")
    (tags . []) (group . "")
    (frame . "0"))

   ((x . 27) (y . 4) (text . "(") (color . "")
    (tags . []) (group . "")
    (frame . "0")))

  "`text', `color', `tags', and `group' are present in all values.

`id', `type' and `duration_ms' are the elements of the alist that make a \"frame\" alist.
`frame' is the element of the alist that makes a \"drawing\" value alist.

A \"drawing\"–value may lack a `frame' element, in which case it is displayed at all times.")


;; Result of the code so far, as of 2022-05-09T02:01 FIXME: it should be a
;; property list, with `global-frame' and each `frame-ₙ' property-value being a
;; list of alists.
;;
;; Example:
(example-plist "The plist generated by my code is below the ^L."
               frame-0 '(((x . 44) (y . 6) (text . ")") (color . "")
                          (tags . []) (group . "")
                          (frame . "0"))

                         ((x . 27) (y . 4) (text . "(") (color . "")
                          (tags . []) (group . "")
                          (frame . "0")))

               global-frame '(((x . 0) (y . 0) (text . "(") (color . "blue")
                               (tags . []) (group . ""))

                              ((x . 1) (y . 0) (text . "spacemacs-banner") (color . "pink")
                               (tags . []) (group . ""))

                              ((x . 18) (y . 0) (text . "()") (color . "red")
                               (tags . []) (group . ""))))

(global-frame
 ((((((((nil
         (x . 0)
         (y . 0)
         (text . "(")
         (color . "blue")
         (tags . #1=[])
         (group . #2=""))
        (x . 1)
        (y . 0)
        (text . "spacemacs-banner")
        (color . "pink")
        (tags . #1#)
        (group . #2#))
       (x . 18)
       (y . 0)
       (text . "()")
       (color . "red")
       (tags . #1#)
       (group . #2#))
      (x . 1)
      (y . 1)
      (text . "(")
      (color . "orange")
      (tags . #1#)
      (group . #2#))
     (x . 2)
     (y . 1)
     (text . "insert-dark-draw-banner")
     (color . "pink")
     (tags . #1#)
     (group . #2#))
    (x . 26)
    (y . 1)
    (text . "\"core/banners/spacemacs-dark-draw-banner-001.ddw\"")
    (color . "green")
    (tags . #1#)
    (group . #2#))
   (x . 75)
   (y . 1)
   (text . ")")
   (color . "orange")
   (tags . #1#)
   (group . #2#))
  (x . 76)
  (y . 1)
  (text . ")")
  (color . "blue")
  (tags . #1#)
  (group . #2#))
 frame-0
 (((((((nil
        (x . 26)
        (y . 4)
        (text . "`")
        (color . #2#)
        (tags . #1#)
        (group . #2#)
        (frame . "0"))
       (x . 28)
       (y . 4)
       (text . "(:forge \"GitHub\"")
       (color . #2#)
       (tags . #1#)
       (group . #2#)
       (frame . "0"))
      (x . 28)
      (y . 5)
      (text . ":owner \"devottys\"")
      (color . #2#)
      (tags . #1#)
      (group . #2#)
      (frame . "0"))
     (x . 28)
     (y . 6)
     (text . ":repo \"darkdraw\"")
     (color . #2#)
     (tags . #1#)
     (group . #2#)
     (frame . "0"))
    (x . 44)
    (y . 6)
    (text . ")")
    (color . #2#)
    (tags . #1#)
    (group . #2#)
    (frame . "0"))
   (x . 28)
   (y . 4)
   (text . ":forge \"GitHub\"")
   (color . #2#)
   (tags . #1#)
   (group . #2#)
   (frame . "0"))
  (x . 27)
  (y . 4)
  (text . "(")
  (color . #2#)
  (tags . #1#)
  (group . #2#)
  (frame . "0")))


;; Pseudocode
;; 1. Read and parse Dark Draw file (JSONL)
;; 1.1. Convert structure of values into one list per frame
;; 1.2. Create a character vector
;; 1.3. Propertize the character vector with colour, weight, etc.
;; 1.4. Return a list of frames with each frame having a propertized character vector and timing fields
;;
;; 2. Display each frame
;; 2.1. Narrow buffer to calculated, centered region
;; 2.2. Insert text
;; 2.3. Setup clock
;; 2.4. Setup hooks for performance consideration (don't let the clock tick when the Spacemacs buffer is not in a open window; 🤔 How does this interact with perspectives?)

(provide 'spacemacs-buffer-dark-draw-banner)
;;; spacemacs-buffer-dark-draw-banner.el ends here
