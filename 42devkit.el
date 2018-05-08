;;; 42devkit.el --- Development mode for 42 school -*- lexical-binding: t -*-

;; Copyright (c) 2018 Clement T.

;; Author: Clement T. <clement@trosa.io>
;; Maintainer: Clement T. <clement@trosa.io>
;; URL: https://github.com/42og/42devkit
;; Created: January 2018
;; Keywords: tools, 42, 42devkit
;; Version: 0.1.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 42devkit: Utils for 42born2code (42.fr) students

;; Some features explained
;;    norminette:
;;      Norminette is a static source code
;;      analyser to check the norm/coding style
;;      of C/C++ files and headers.
;;      The `norminette` function is a convenient
;;      wrapper for the local binary (/usr/bin/norminette)

;;; Code:

;; Norminette:

(defvar norminette--always-prompt nil
  "If the variable is set to true,
   the function will ask to user
   which folder the binary should analyse.
   The default folder where the buffer
   is open.")

(defvar norminette--default-args ""
  "The binary doesn't take options by default,
   but it's possible to specify your own arguments
   by setting up the variable in your scope.")

(defun norminette-ask (p)
  "Config wrapper to configure the norminette
   behaviour. The the type arguments are invalid,
   then ;TODO: he function will ignore it."
  (cond ((eq t p) (setf norminette--always-prompt t)
         (eq t nil) (setf norminette--always-prompt nil))))

(defun norminette-args (args)
  "Configuration wrapper for norminette args"
  (when (stringp args)
    (setf norminette--default-args args)))

(defun norminette ()
  "Prompt in a first time user input for project path
   if the local variable `42project` is not set.
   Then create a buffer with the norminette output.
   This is a convenient wrapper of the 42's norminette
   binary file on workstations."
  (interactive)
  (let* ((binary "/usr/bin/norminette")
         (inhibit-read-only nil)
		 (dir (file-name-directory buffer-file-name))
		 (greper "| grep -B1 \"^Error\""))
	(with-output-to-temp-buffer "*Norminette*"
	  (when (eq norminette--always-prompt t)
		(setq dir (read-file-name "Directory to scan: ")))
	  (when (eq 1 (shell-command (format "%s %s %s %s" binary
                             norminette--default-args dir
							 greper) "*Norminette*" "*Messages*"))
        (with-local-quit
          (message "Files in folder are correctly normed. `q` to close")
          t))
      (pop-to-buffer "*Norminette*"))))

(defun norminette-file-buffer ()
  "Apply the norminette on the current buffer
   instead of directory"
  (interactive)
  (let* ((binary "/usr/bin/norminette")
         (inhibit-read-only nil)
         (inhibit-quit t)
		 (dir (buffer-file-name (current-buffer)))
		 (greper "| grep -B1 \"^Error\""))
	(with-output-to-temp-buffer "*Norminette File*"
      (when (eq 1 (shell-command (format "%s %s %s %s" binary
                                         norminette--default-args dir
                                         greper) "*Norminette File*" "*Messages*"))
        (with-local-quit
          (message "The file is correctly normed. `q` to close")
          t)
      (pop-to-buffer "*Norminette File*")))))

(global-unset-key (kbd "M-n"))
(global-set-key (kbd "M-n")
                'norminette) ; Default binding
(global-unset-key (kbd "C-x M-n"))
(global-set-key (kbd "C-x M-n")
                'norminette-file-buffer) ; Default file binding

;; Norminette ends here

(provide '42devkit)

;;; 42devkit.el ends here
