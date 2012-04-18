;;;  -*- coding: utf-8 -*-
;;; osc-spec-mode.el --- used for editing rpm spec files for opensuse

;; Copyright (C) 2012  Togan Muftuoglu

;; Author: Togan Muftuoglu <toganm@opensuse.org>
;; Keywords: tools, languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  This mode is used for editing spec files used for writing spec files for
;;  openSUSE rpm packages. It tries to follow the packaging guidelines of the
;;  project and does not replace osc commands.

;;  Put this in your .emacs file to enable autoloading
;;  of osc-spec-mode, and auto-recognition of ".spec" files:
;;
;;  (autoload 'osc-spec-mode "osc-spec-mode.el" "RPM spec mode." t)
;;  (setq auto-mode-alist (append '(("\\.spec" . osc-spec-mode))
;;                                auto-mode-alist))

;; TODO use auto-insert mode to initiliaze the spec
;;  (add-hook 'find-file-hook 'auto-insert)'

;;; Code:

(require 'cl)
(require 'dired)
(defconst osc-spec-mode-version "0.0.1" "Version of `osc-spec-mode'.")

(defgroup osc-spec nil
  "RPM spec mode with Emacs/XEmacs enhancements."
  :prefix "osc-spec-"
  :group 'languages)

(defcustom osc-owner ""
  "This variable holds your email address, to be used in changelogs."
  :type 'string
  :group 'osc-spec)




(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (point-marker))
  (end-of-line))

(defun mark-to-end ()
  (interactive)
  (set-mark (point-marker))
  (end-of-line))

;; create the auto-insert thingys read auto-insert and auto-insert-alist functions

;; (("\\.spec" . "rpm spec skeleton ")
;; "#\n# spec file for" (file-name-nondirectory (buffer-file-name) "\n"
;; "# Copyright (C) " (substring (current-time-string) -4) "   "
;;   (getenv "ORGANIZATION")
;;   |
;;   (progn user-full-name)
;;   "\n\n#"
;;   (user-full-name)
;;   '(if
;;        (search-backward "&"
;; 			(line-beginning-position)
;; 			t)
;;        (replace-match
;; 	(capitalize
;; 	 (user-login-name))
;; 	t t))
;;   '(end-of-line 1)
;;   " <"
;;   (progn user-mail-address) 





;; borrowed from 
;; http://wiki.mandriva.com/en/Development/Tasks/Packaging/Tools/Emacs
;; 
;; modified to fit needs

;; (require 'ffap)
;; (defun my-rpm-ffap (name)
;;   (ffap-locate-file name '("" ".gz" ".bz2") 
;;                '("./" "../SOURCES")))              
;; (add-to-list 'ffap-alist 
;;           '(rpm-spec-mode . my-rpm-ffap))

;; borrowed from 
;; http://wiki.mandriva.com/en/Development/Tasks/Packaging/Tools/Emacs
;; 
;; modified to fit needs


;; (defun osc-insert-patch ()
;;   (interactive)
;;   (goto-char (point-min))
;;   (let* ((file 
;;      (completing-read 
;;       "Patch: "
;;       (mapcar (function (lambda (rule) (list rule)))
;;             (directory-files "../SOURCES/" nil "^\\([_-0-9a-zA-Z]+\\).''\\.patch.'''"))) )
;;        (max (search-forward-regexp rpm-section-regexp))
;;        (count 0)
;;        )
;;    (goto-char (point-min))
;;    (while (search-forward-regexp "^Patch?\\([0-9]+\\)?" max t) 
;;      (if (> (string-to-int (match-string 1)) count)
;;         (setq count (string-to-int (match-string 1)))
;;       )
;;      )
;;    (if (eq count 0) (while (search-forward-regexp "^Source?\\([0-9]+\\)?" max t)()))   
;;    (setq count (1+ count))
;;    (end-of-line)
;;    (insert (format "\n%s%d%s%s" "Patch" count ": " file))
;;    (goto-char (point-min))
;;    (if (search-forward-regexp "^%patch?\\([0-9]+\\)?" nil t)
;;       (progn
;;         (beginning-of-line)
;;         (while (search-forward-regexp "^%patch?\\([0-9]+\\)?" nil t) ())
;;         )
;;      (search-forward-regexp "^%setup" nil t))

;;    (end-of-line)
;;    (insert (format "\n%s%d%s" "%patch" count " -p1 "))
;;    (let ((name (rpm-spec-field-value "name" nil))
;;         (version (rpm-spec-field-value "version" nil))
;;         (string)
;;         )
;;      (cond ((string-match 
;;            (concat "^" (regexp-quote (format "%s-%s-" name version))
;;                  "\\([^.]*\\)\\(\\.patch\\.bz2\\)") file)
;;           (setq string (format "%s%s" "-b ." 
;;                           (substring file (match-beginning 1) (match-end 1)))))
;;          ((string-match 
;;            (concat "^" (regexp-quote (format "%s-" name version))
;;                  ".''[0-9]+-" "\\([^.]'''\\)\\(\\.patch\\.bz2\\)") file)
;;           (setq string 
;;                (format "%s%s" "-b ." 
;;                      (substring file (match-beginning 1) (match-end 1)))))
;;          )
;;      (if string
;;         (insert string))
;;      )))



(defun osc-pwd ()
  "Returns the current working directory, sans parent directories similar to pwd"
  (file-name-nondirectory (directory-file-name (expand-file-name default-directory))))



(defun osc-view-readonly (filename)
  "view a file readonly"
  (let* ((buffer-name (format "*%s*" filename)))
    (if (get-buffer buffer-name)
        (kill-buffer buffer-name))
    (let* ((output-buffer (get-buffer-create buffer-name))
           (outwin (display-buffer output-buffer nil t)))
      (save-selected-window
        (select-window outwin)
        (insert-file-contents filename)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun osc-view-changelog ()
  "something like \"less foo.changes\""  
  (interactive)

  (let ((filename (format "%s.changes" (osc-pwd))))
    (osc-view-readonly filename)))


(defun osc-view-spec ()
  "Open the .spec file in the current working directory in read-only mode."
  (interactive)

  ;;"something like \"less foo.spec\""
  (let ((filename (format "%s.spec" (osc-pwd))))
    (osc-view-readonly filename)))


(defun osc-edit-spec ()
  "Edit the .spec file found in the current working directory."
  (interactive)
  (let* ((filename (format "%s.spec" (osc-pwd)))
         (buf (find-file filename)))
    (switch-to-buffer buf))


(defun osc-remove-leading-plusses ()
  "Strip leading `+' characters from the working buffer.  This is
useful when copying and pasting diff output directly."
  (interactive)
  (let ((old-pnt (point-marker)))
    (beginning-of-buffer)
    (replace-regexp "^+" "")
    (goto-char old-pnt)
    (message "I am feeling nonplussed.")))

(defun osc-intltoolize-spec ()
  "Add intltoolize --force to the spec"
  (interactive)
  (let ((old-pnt (point-marker)))
    (beginning-of-buffer)
    (replace-regexp "^autoreconf.*$" "\\& intltoolize --force")

    (message "Now inspect what you've done, since it might not be
    correct.  If you're happy with the change(s) made, save this
    buffer, run osc-intltoolize-changelog, and save that buffer
    too.")))

(defun osc-intltoolize-changelog ()
  "Document what I did after intltoolizing a spec."
  (interactive)
  (osc-changelog)
  (insert (format "Fix the build when against newer versions of intltool.")))

;;; Similar to vc on suse boxen.
;;; FIXME: Make this smarter in the face of a) oddly named working directories,
;;; b) "packages" with multiple .spec and .changes files, c) everything
;;; else.

(defun osc-changelog ()

  "Edit the .changes file in the current working directory,
inserting the correct boilerplate text and positioning the cursor
for immediate typing.  This is similar to the 'vc' command in the
internal SUSE buildsystem. "

  (interactive)
  (if (string-equal "" osc-owner)
      (message "You need to customize the variable osc-owner before using this functionality.  To do so: M-x customize-group RET ab RET, then set `osc-owner' to your email address and save.")
    (let* ((filename (format "%s.changes" (osc-pwd)))
           (time-format "%a %b %e %H:%M:%S %Z %Y")
           (buf (find-file filename)))
      (switch-to-buffer buf)
      (goto-char (point-min))
      (insert (format "-------------------------------------------------------------------\n"))
      (insert (format "%s - %s\n" (format-time-string time-format) osc-owner))
      (insert (format "\n"))
      (insert (format "%s " "-"))
      (let ((old-pnt (point-marker)))
        (insert (format "\n\n"))
        (goto-char old-pnt)))))




(defun osc-br-split ()
  "Split a BuildRequires: line like this \"BuildRequires:  foo bar baz\" into
BuildRequires:  foo
BuildRequires:  bar
BuildRequires:  baz"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (string-equal "BuildRequires" (word-at-point))
        (progn (beginning-of-line)
               (forward-word)
               (forward-word)
               (backward-char)
               (let ((here (point-marker)))
                 (progn (end-of-line)
                        (let ((there (point-marker)))
                          (progn (goto-char here)
                                 (let ((from " ")
                                       (to "\nBuildRequires:  "))
                                   (replace-string from to nil here there)))))))
      (message "You can only run osc-br-split on a line that starts with BuildRequires:"))))


(defun osc-ldconfig ()
  "Replace `%run_ldconfig', which is deprecated, with direct calls to `/sbin/ldconfig'."
  (interactive)          
  (let ((old-pnt (point-marker)))
    (beginning-of-buffer)
    (replace-string "%run_ldconfig" "/sbin/ldconfig")
    (goto-char old-pnt)))


(defun osc-lang-files ()
  "Insert typical lang subpackage text."
  (interactive)
  (insert (format "%%files lang -f %%{name}.lang\n")))

(defun osc-fdupes ()
  "Insert typical %fdupes text."
  (interactive)
  (insert (format "%%fdupes %buildroot\n")))


(defun osc-open-rej ()
  "When a patch fails to apply, it will leave behind files with
the suffix `.rej'. To use this command effectively, open the file
that was not successfully patched, and then run this command,
which will split the window and open the `.rej' file in one
window, and the unpatched or partially unpatched file in the
other."
  (interactive)
  (let* ((this-buffer (buffer-file-name))
         (rej-buffer (concat this-buffer ".rej")))
    (delete-other-windows)
    (split-window)
    (find-file rej-buffer)
    (revert-buffer t t)))

(defun osc-about-osc-spec-mode (&optional arg)
  "About `osc-spec-mode'."
  (interactive "p")
  (message
   (concat "osc-spec-mode version "
           osc-spec-mode-version
           " by Togan Muftuoglu <toganm@opensuse.org> ")))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spec\\(\\.in\\)?$" . osc-spec-mode))

(provide 'osc-spec-mode)

;;; osc-spec-mode.el ends here
