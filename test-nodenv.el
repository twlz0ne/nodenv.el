;;; test-nodenv.el --- Test nodenv -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Gong QiJian <gongqijian@gmail.com>

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

;;; Code:

(require 'ert)
(require 'nodenv)

(defun test-nodenv--make-project (specs)
  "Make empty project and create directory & files specified by SPECS.
Example of SPECS:

        '(\"src/\"
          (\"src/.node-version\" . \"6.0.0\"))

Return project root."
  (let ((root (make-temp-file "test-nodenv--" 'root "/")))
    (dolist (spec specs)
      (cond
       ((consp spec)
        (let ((file-path (concat root (car spec)))
              (file-content (cdr spec)))
          (make-directory (file-name-directory file-path) t)
          (with-temp-buffer
            (insert file-content)
            (write-region (point-min) (point-max) file-path))))
       (t
        (let ((folder-path (concat root spec)))
          (make-directory (file-name-directory folder-path) t)))))
    root))

(defun test-nodenv--newest-version ()
  "Get newest version of node."
  (car (reverse (nodenv-versions))))

(defun test-nodenv--open-file (file-name &optional defer-p)
  "Open file `FILE-NAME', return node version if `DEFER-P' is nil (the default)."
  (setq enable-local-variables :all)
  (find-file file-name)
  (js-mode)
  (unless defer-p
    (nodenv-mode)
    (getenv "NODENV_VERSION")))

(ert-deftest test-nodenv-node-version-file-0 ()
  (let ((root (test-nodenv--make-project '(("test.js" . "\n")))))
    (should (equal (test-nodenv--newest-version)
                   (test-nodenv--open-file (concat root "test.js"))))))

(ert-deftest test-nodenv-node-version-file-1 ()
  (let ((root (test-nodenv--make-project '(("test.js" . "\n")
                                           (".node-version" . "6.0.0")))))
    (should (equal "6.0.0"
                   (test-nodenv--open-file (concat root "test.js"))))))

(ert-deftest test-nodenv-node-version-file-2 ()
  (let ((root (test-nodenv--make-project '(("test1.js" . "\n")
                                           (".node-version" . "6.0.0")
                                           ("src/test2.js" . "\n")
                                           ("src/.node-version" . "7.0.0")))))
    (should (equal "6.0.0" (test-nodenv--open-file (concat root "test1.js"))))
    (should (equal "7.0.0" (test-nodenv--open-file (concat root "src/test2.js"))))))

(ert-deftest test-nodenv-node-version-file-3 ()
  (let ((root (test-nodenv--make-project '(("foo/test1.js" . "\n")
                                           ("foo/.node-version" . "6.0.0")
                                           ("bar/test2.js" . "\n")
                                           ("bar/.node-version" . "7.0.0")))))
    (should (equal "6.0.0" (test-nodenv--open-file (concat root "foo/test1.js"))))
    (should (equal "7.0.0" (test-nodenv--open-file (concat root "bar/test2.js"))))))

(ert-deftest test-nodenv-local-version ()
  (let ((root (test-nodenv--make-project
               (list (cons ".node-version" "6.0.0")
                     (cons "test-without-local-variable.js" "\n")
                     (cons "test-with-local-variable.js" (concat
                                                          "// Local Variables:\n"
                                                          "// nodenv-node-version: \"7.0.0\"\n"
                                                          "// End:\n"))))))
    ;; Use node version specified in .node-version
    (should (equal "6.0.0" (test-nodenv--open-file (concat root "test-without-local-variable.js"))))

    ;; Use node version specified by local variable
    (add-hook 'hack-local-variables-hook
              (lambda ()
                (when nodenv-node-version
                  (nodenv-mode)
                  (should (equal "7.0.0" (nodenv-version))))))
    (test-nodenv--open-file (concat root "test-with-local-variable.js") t)
    ))

(provide 'test-nodenv)

;;; test-nodenv.el ends here
