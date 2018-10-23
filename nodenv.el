;;; nodenv.el ---  Integration with nodenv -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; URL: https://github.com/twlz0ne/nodenv.el
;; Created: 2018/06/30
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; Keywords: node, environment, tools

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
;;  Emacs integration for nodenv.
;;

;;; Installation
;;
;; Copy file `nodenv.el` to directory `~/.emacs.d/site-lisp/nodenv.el/`, for example, and add this to your .emacs to load the mode
;;
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/nodenv.el"))
;; (add-to-list 'exec-path (expand-file-name "~/.nodenv/shims"))
;; (require 'nodenv)
;; (add-hook 'js-mode-hook #'nodenv-mode)
;;

;;; Change Log:
;;
;;  0.1.0  2018/06/30  Initial version.
;;

;;; Code:

(defgroup nodenv nil
  "Nodenv virtualenv integration."
  :group 'nodenv)

(defvar nodenv-node-version nil "Local variable to specify node version.")
(make-local-variable 'nodenv-node-version)

(defcustom nodenv-mode-line-format
  '(:eval
    (when (nodenv-version)
      (concat " Nodenv:" (nodenv-version))))
  "How `nodenv' will indicate the current node version in the mode line."
  :group 'nodenv
  :type 'sexp
  :risky t
  :package-version '(nodenv . "0.1.0"))

(defsubst nodenv--trim-path-separator-r (string)
  "Remove path separator from trailing of STRING."
  (let ((i (string-match-p "\\(?:/\\)\\'" string)))
    (if i (substring string 0 i) string)))

(defun nodenv-version ()
  "Return currently active nodenv version."
  (getenv "NODENV_VERSION"))

(defun nodenv-versions ()
  "List installed node versions."
  (let ((versions (shell-command-to-string "nodenv versions --bare")))
    (cons "system" (split-string versions))))

(defun nodenv-read-version ()
  "Read virtual environment from user input."
  (completing-read "Nodenv: " (nodenv-versions)))

(defun nodenv-node-version-file (&optional dir)
  "Lookup `.node-version' from DIR or current folder"
  (let* ((curr-dir (or dir (file-name-directory (buffer-file-name))))
         (ver-file (concat curr-dir ".node-version")))
    (if (file-exists-p ver-file)
        ver-file
      (let ((next-dir (file-name-directory (nodenv--trim-path-separator-r curr-dir))))
        (when next-dir
          (nodenv-node-version-file next-dir))))))

;;;###autoload
(defun nodenv-set (version)
  "Set node shell VERSION."
  (interactive (list (nodenv-read-version)))
  (setenv "NODENV_VERSION" version)
  (force-mode-line-update))

;;;###autoload
(defun nodenv-auto-set ()
  "Auto detect node version."
  (if nodenv-node-version
      (nodenv-set nodenv-node-version)
    (let* ((ver-file (nodenv-node-version-file))
           (ver (if ver-file
                    (replace-regexp-in-string
                     "\\(?:\n\\)\\'" "" (shell-command-to-string (format "head -n 1 %s" ver-file)))
                  (car (reverse (nodenv-versions))))))
      (nodenv-set ver))))

;;;###autoload
(defun nodenv-unset ()
  "Unset node shell version."
  (interactive)
  (setenv "NODENV_VERSION")
  (force-mode-line-update))

(defvar nodenv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'nodenv-set)
    (define-key map (kbd "C-c C-u") 'nodenv-unset)
    map)
  "Keymap for nodenv.")

;;;###autoload
(define-minor-mode nodenv-mode
  "Minor mode for nodenv interaction.

\\{nodenv-mode-map}"
  :global nil
  :lighter nodenv-mode-line-format
  :keymap nodenv-mode-map
  (if nodenv-mode
      (nodenv-auto-set)
    (nodenv-unset)))

(provide 'nodenv)

;;; nodenv.el ends here
