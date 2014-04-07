;;; namerwang.el --- transmogrifies names between a variety of styles, camels, snake, dashed, title-ized or humanized.

;; Author: Jason Milkins <jasonm23@gmail.com>
;; Url: https://github.com/jasonm23/namerwang
;; Version: 201404071130
;; Package-Requires: ((emacs "24.1") (s "1.9.0"))
;; Keywords: Strings, Names

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

;; Namerwang transmogrifies names between a variety of styles, camels,
;; snake, dashed, titleized or humanized. It uses the powerful 's'
;; library by the most excellent Magnar Sveen.

;;; Change Log:
;;  0.2.0: fix dependency auto-install - add key bindings - first public release to Marmalade
;;  0.1.0: initial

(require 's)

(defun snake-case-namerwang ()
  "snake_case the current word or the region or at cursor point."
  (interactive)
  (namerwang-operate 's-snake-case))

(defun dashed-namerwang ()
  "dashed-the-current CamelCase or snake_case word or the
region or at cursor point."
  (interactive)
  (namerwang-operate 's-dashed-words))

(defun upper-camelcase-namerwang ()
  "UpperCamelCaseTheCurrent words or name at point or in current
region."
  (interactive)
  (namerwang-operate 's-upper-camel-case))

(defun lower-camelcase-namerwang ()
  "lowerCamelCaseTheCurrent dashed or snake_case word or any
words in the region or at cursor point."
  (interactive)
  (namerwang-operate 's-lower-camel-case))

(defun humanize-namerwang ()
  "Humanize variable names, insert spaces instead of - or _ or
un-CamelCase humps to spaced words."
  (interactive)
  (namerwang-operate 's-capitalized-words))

(defun titleized-namerwang ()
  "Convert dashed, underscored or (both styles of) CamelCase,
  or spaced words in region, Title Case Words."
  (interactive)
  (namerwang-operate 's-titleized-words))

(defun namerwang-operate (fn)
  "Get the current unspaced string at point, or the current
region, if selected, and replace it with the return value of fn -
an ordinary defun."
  (let (pos1 pos2 meat)
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'symbol))
            pos2 (cdr (bounds-of-thing-at-point 'symbol))))
    (setq meat (funcall fn (buffer-substring-no-properties pos1 pos2)))
    (delete-region pos1 pos2)
    (insert  meat)))

;; Default bindings...
(global-set-key (kbd "C-c C-x C-x l") 'lower-camelcase-namerwang)
(global-set-key (kbd "C-c C-x C-x u") 'upper-camelcase-namerwang)
(global-set-key (kbd "C-c C-x C-x d") 'dashed-namerwang)
(global-set-key (kbd "C-c C-x C-x h") 'humanize-namerwang)
(global-set-key (kbd "C-c C-x C-x t") 'titleized-namerwang)
(global-set-key (kbd "C-c C-x C-x s") 'snake-case-namerwang)

(provide 'namerwang)
;;; namerwang.el ends here
