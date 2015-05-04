;;; popup-pack.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Antoine R. Dumont

;; Author: Antoine R. Dumont <tony@dagobah>
;; Keywords: convenience

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

;;

;;; Code:

(require 'flycheck)

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Flycheck errors*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(add-to-list 'display-buffer-alist
             `(,(rx bos "*Help*" eos)
               (display-buffer-reuse-window
                display-buffer-in-side-window)
               (reusable-frames . visible)
               (side            . bottom)
               (window-height   . 0.4)))

(defun popup-pack/quit-bottom-side-windows ()
  "Quit side windows of the current frame."
  (interactive)
  (dolist (window (window-at-side-list))
    (quit-window nil window)))

(defvar popup-pack-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c q") 'popup-pack/quit-bottom-side-windows)
    map)
  "Keymap for Popup-pack mode.")

(define-minor-mode popup-pack-mode
  "Minor mode to consolidate Emacs' popup-pack extensions.

\\{popup-pack-mode-map}"
  :lighter " PP"
  :keymap popup-pack-mode-map)

(define-globalized-minor-mode global-popup-pack-mode popup-pack-mode popup-pack-on)

(defun popup-pack-on ()
  "Turn on `popup-pack-mode'."
  (popup-pack-mode +1))

(global-popup-pack-mode)

(provide 'popup-pack)
;;; popup-pack.el ends here
