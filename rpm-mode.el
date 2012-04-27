;;; rpm-mode.el --- major mode for rpm spec files

;; Copyright (C) 2012  Togan Muftuoglu

;; Author: Togan Muftuoglu <toganm@opensuse.org>
;; Keywords: tools, unix, languages, rpm

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

;;  Writing rpm spec files can sometimes be a tedious job. This major mode
;;  tries to bring ease to the process of creating specs for openSUSE rpm
;;  packages. It tries to follow the packaging guidelines of the
;;  project and does not replace osc commands. 
;;
;;  This major mode is based on ideas and some code from the rpm-spec-mode
;;  and is so attributed where necessary
;;
;;  Put this in your .emacs file to enable autoloading of rpm-mode,
;;  and auto-recognition of ".spec" files:
;;
;;  (autoload 'rpm-mode "rpm-mode.el" "RPM spec mode." t)
;;  (setq auto-mode-alist (append '(("\\.spec" . rpm-mode))
;;                                auto-mode-alist))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; TODO use auto-insert mode to initiliaze the spec ;;
 ;; (add-hook 'find-file-hook 'auto-insert)'         ;;
 ;;                                                  ;;
 ;; or use yassnippet templates                      ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defvar rpm-mode-hook nil)


(defvar rpm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-lf" 'rpm-lang-files)
    (define-key map "\C-c\C-fd" 'rpm-fdupes)
    (define-key map "\C-c\C-br" 'rpm-br-split)
    (define-key map "\C-c\C-ld" 'rpm-ldconfig)
    map)
  "Keymap for `rpm-mode'.")

;; modify the keymap so comment-dwim is remapped
(define-key rpm-mode-map [remap comment-dwim] 'rpm-comment-dwim)


(defvar rpm-sections
   '("preamble" "description" "patch" "package" "prep" "configure" "setup" "build" "install" "check" "clean" "changelog" "files")
   "Partial list of section names.")

(defvar rpm-tags-list
  ;; From RPM 4.4.9 sources, file build/parsePreamble.c: preambleList[], and
  ;; a few macros that aren't tags, but useful here.
  ;; TODO Also add openSUSE related macros
  ;; TODO remove unneeded ones for opensuse and OBS

  '( ("BuildArch")
     ("BuildArchitectures")
     ("BuildConflicts")
     ("BuildEnhances")
     ("BuildPlatforms")
     ("BuildPreReq")
     ("BuildRequires")
     ("#!BuildIgnore")
     ("BuildRoot")
     ("BuildSuggests")
     ("Conflicts")
     ("%description")
     ("Enhances")
     ("%files")
     ("Group")
     ("%ifarch")
     ("%if 0%{?suse_version}")
     ("Keyword")
     ("Keywords")
     ("License")
     ("Name")
     ("NoPatch")
     ("NoSource")
     ("Obsoletes")
     ("%package")
     ("%py_requires")
     ("Patch")
     ("Prefix")
     ("Prefixes")
     ("PreReq")
     ("Provides")
     ("Release")
     ("Requires")
     ("Source")
     ("Suggests")
     ("Summary")
     ("%suse_version")
     ("Url")
     ("Variant")
     ("Variants")
     ("Version")
     )
  "List of elements that are valid tags.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO                                                       ;;
;;                                                            ;;
;; Now we have the tags and groups defined how do we use them ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar rpm-group-tags-list
  ;; From http://en.opensuse.org/openSUSE:Package_group_guidelines

  '(("Amusements/Games/3D/Other")
    ("Amusements/Games/3D/Race")
    ("Amusements/Games/3D/Shoot")
    ("Amusements/Games/3D/Simulation")
    ("Amusements/Games/Action/Arcade")
    ("Amusements/Games/Action/Breakout")
    ("Amusements/Games/Action/Other")
    ("Amusements/Games/Action/Race")
    ("Amusements/Games/Action/Shoot")
    ("Amusements/Games/Board/Card")
    ("Amusements/Games/Board/Chess")
    ("Amusements/Games/Board/Other")
    ("Amusements/Games/Board/Pool")
    ("Amusements/Games/Board/Puzzle")
    ("Amusements/Games/Logic")
    ("Amusements/Games/Other")
    ("Amusements/Games/RPG")
    ("Amusements/Games/Strategy/Other")
    ("Amusements/Games/Strategy/Real Time")
    ("Amusements/Games/Strategy/Turn Based")
    ("Amusements/Teaching/Language")
    ("Amusements/Teaching/Mathematics")
    ("Amusements/Teaching/Other")
    ("Amusements/Toys/Background")
    ("Amusements/Toys/Clocks")
    ("Amusements/Toys/Graphics")
    ("Amusements/Toys/Other")
    ("Amusements/Toys/Screensavers")
    ("Development/Languages/C and C++")
    ("Development/Languages/Fortran")
    ("Development/Languages/Java")
    ("Development/Languages/Other")
    ("Development/Languages/Perl")
    ("Development/Languages/Python")
    ("Development/Languages/Ruby")
    ("Development/Languages/Scheme")
    ("Development/Languages/Tcl")
    ("Development/Libraries/C and C++")
    ("Development/Libraries/Cross")
    ("Development/Libraries/GNOME")
    ("Development/Libraries/Java")
    ("Development/Libraries/KDE")
    ("Development/Libraries/Other")
    ("Development/Libraries/Parallel")
    ("Development/Libraries/Perl")
    ("Development/Libraries/Python")
    ("Development/Libraries/Tcl")
    ("Development/Libraries/X11")
    ("Development/Libraries/YaST")
    ("Development/Sources")
    ("Development/Tools/Building")
    ("Development/Tools/Debuggers")
    ("Development/Tools/Doc Generators")
    ("Development/Tools/GUI Builders")
    ("Development/Tools/IDE")
    ("Development/Tools/Navigators")
    ("Development/Tools/Other")
    ("Development/Tools/Version Control")
    ("Documentation/Howto")
    ("Documentation/HTML")
    ("Documentation/Man")
    ("Documentation/Other")
    ("Documentation/SuSE")
    ("Hardware/Camera")
    ("Hardware/Fax")
    ("Hardware/ISDN")
    ("Hardware/Joystick")
    ("Hardware/Mobile")
    ("Hardware/Modem")
    ("Hardware/Other")
    ("Hardware/Palm")
    ("Hardware/Printing")
    ("Hardware/Psion")
    ("Hardware/Radio")
    ("Hardware/Scanner")
    ("Hardware/TV")
    ("Hardware/UPS")
    ("Hardware/Wifi")
    ("Multimedia/Graphics")
    ("Multimedia/Graphics/Vector Editors")
    ("Productivity/Archiving/Backup")
    ("Productivity/Archiving/Compression")
    ("Productivity/Clustering/Computing")
    ("Productivity/Clustering/HA")
    ("Productivity/Databases/Clients")
    ("Productivity/Databases/Servers")
    ("Productivity/Databases/Tools")
    ("Productivity/File utilities")
    ("Productivity/Graphics/3D Editors")
    ("Productivity/Graphics/Bitmap Editors")
    ("Productivity/Graphics/CAD")
    ("Productivity/Graphics/Convertors")
    ("Productivity/Graphics/Other")
    ("Productivity/Graphics/Vector Editors")
    ("Productivity/Graphics/Viewers")
    ("Productivity/Graphics/Visualization/Graph")
    ("Productivity/Graphics/Visualization/Other")
    ("Productivity/Graphics/Visualization/Raytracers")
    ("Productivity/Hamradio/Fax")
    ("Productivity/Hamradio/Logging")
    ("Productivity/Hamradio/Morse")
    ("Productivity/Hamradio/Other")
    ("Productivity/Hamradio/Packet")
    ("Productivity/Hamradio/Psk31")
    ("Productivity/Hamradio/Satellite")
    ("Productivity/Multimedia/CD/Grabbers")
    ("Productivity/Multimedia/CD/Players")
    ("Productivity/Multimedia/CD/Record")
    ("Productivity/Multimedia/Other")
    ("Productivity/Multimedia/Sound/Editors and Convertors")
    ("Productivity/Multimedia/Sound/Midi")
    ("Productivity/Multimedia/Sound/Mixers")
    ("Productivity/Multimedia/Sound/Players")
    ("Productivity/Multimedia/Sound/Utilities")
    ("Productivity/Multimedia/Sound/Visualization")
    ("Productivity/Multimedia/Video/Editors and Convertors")
    ("Productivity/Multimedia/Video/Players")
    ("Productivity/Networking/AOLInstantMessenger")
    ("Productivity/Networking/Archie")
    ("Productivity/Networking/Boot/Clients")
    ("Productivity/Networking/Boot/Servers")
    ("Productivity/Networking/Boot/Utilities")
    ("Productivity/Networking/Diagnostic")
    ("Productivity/Networking/DNS/Servers")
    ("Productivity/Networking/DNS/Utilities")
    ("Productivity/Networking/Email/Clients")
    ("Productivity/Networking/Email/Mailinglists")
    ("Productivity/Networking/Email/Servers")
    ("Productivity/Networking/Email/Utilities")
    ("Productivity/Networking/File-Sharing")
    ("Productivity/Networking/Ftp/Clients")
    ("Productivity/Networking/Ftp/Servers")
    ("Productivity/Networking/ICQ")
    ("Productivity/Networking/Instant Messenger")
    ("Productivity/Networking/IRC")
    ("Productivity/Networking/LDAP/Clients")
    ("Productivity/Networking/LDAP/Servers")
    ("Productivity/Networking/LDAP/Utilities")
    ("Productivity/Networking/Napster")
    ("Productivity/Networking/News/Clients")
    ("Productivity/Networking/News/Servers")
    ("Productivity/Networking/News/Utilities")
    ("Productivity/Networking/NFS")
    ("Productivity/Networking/NIS")
    ("Productivity/Networking/Novell")
    ("Productivity/Networking/Other")
    ("Productivity/Networking/PPP")
    ("Productivity/Networking/Radius/Clients")
    ("Productivity/Networking/Radius/Servers")
    ("Productivity/Networking/Routing")
    ("Productivity/Networking/Samba")
    ("Productivity/Networking/Security")
    ("Productivity/Networking/SSH")
    ("Productivity/Networking/System")
    ("Productivity/Networking/Talk/Clients")
    ("Productivity/Networking/Talk/Servers")
    ("Productivity/Networking/Web/Browsers")
    ("Productivity/Networking/Web/Frontends")
    ("Productivity/Networking/Web/Proxy")
    ("Productivity/Networking/Web/Servers")
    ("Productivity/Networking/Web/Utilities")
    ("Productivity/Office/Dictionary")
    ("Productivity/Office/Finance")
    ("Productivity/Office/Management")
    ("Productivity/Office/Organizers")
    ("Productivity/Office/Other")
    ("Productivity/Office/Spreadsheets")
    ("Productivity/Office/Suite")
    ("Productivity/Office/Word Processor")
    ("Productivity/Other")
    ("Productivity/Publishing/DocBook")
    ("Productivity/Publishing/HTML/Editors")
    ("Productivity/Publishing/HTML/Tools")
    ("Productivity/Publishing/Other")
    ("Productivity/Publishing/PDF")
    ("Productivity/Publishing/Presentation")
    ("Productivity/Publishing/PS")
    ("Productivity/Publishing/SGML")
    ("Productivity/Publishing/TeX/Base")
    ("Productivity/Publishing/TeX/Fonts")
    ("Productivity/Publishing/TeX/Frontends")
    ("Productivity/Publishing/TeX/Utilities")
    ("Productivity/Publishing/Texinfo")
    ("Productivity/Publishing/Troff")
    ("Productivity/Publishing/Word")
    ("Productivity/Publishing/XML")
    ("Productivity/Scientific/Astronomy")
    ("Productivity/Scientific/Chemistry")
    ("Productivity/Scientific/Electronics")
    ("Productivity/Scientific/Math")
    ("Productivity/Scientific/Other")
    ("Productivity/Scientific/Physics")
    ("Productivity/Security")
    ("Productivity/Telephony/Clients")
    ("Productivity/Telephony/H323/Clients")
    ("Productivity/Telephony/H323/Servers")
    ("Productivity/Telephony/H323/Utilities")
    ("Productivity/Telephony/Servers")
    ("Productivity/Telephony/SIP/Clients")
    ("Productivity/Telephony/SIP/Servers")
    ("Productivity/Telephony/SIP/Utilities")
    ("Productivity/Telephony/Utilities")
    ("Productivity/Text/Convertors")
    ("Productivity/Text/Editors")
    ("Productivity/Text/Spell")
    ("Productivity/Text/Utilities")
    ("System/Base")
    ("System/Benchmark")
    ("System/Boot")
    ("System/Console")
    ("System/Daemons")
    ("System/Emulators/Other")
    ("System/Emulators/PC")
    ("System/Fhs")
    ("System/Filesystems")
    ("System/GUI/GNOME")
    ("System/GUI/KDE")
    ("System/GUI/LXDE")
    ("System/GUI/Other")
    ("System/GUI/XFCE")
    ("System/I18n/Chinese")
    ("System/I18n/Japanese")
    ("System/I18n/Korean")
    ("System/Kernel")
    ("System/Libraries")
    ("System/Localization")
    ("System/Management")
    ("System/Monitoring")
    ("System/Packages")
    ("System/Shells")
    ("System/Sound Daemons")
    ("System/X11/Displaymanagers")
    ("System/X11/Fonts")
    ("System/X11/Icons")
    ("System/X11/Servers/XF86_3")
    ("System/X11/Servers/XF86_4")
    ("System/X11/Terminals")
    ("System/X11/Utilities")
    ("System/Yast")
    )
  "List of elements that are valid group tags.")




(defvar rpm-tags-regexp
  (concat "\\(\\<" (regexp-opt (mapcar 'car rpm-tags-list))
	  "\\|\\(Patch\\|Source\\)[0-9]+\\>\\)")
  "Regular expression for matching valid tags.")


(defvar rpm-mode-syntax-table nil
  "Syntax table in use in `rpm-mode' buffers.")
(unless rpm-mode-syntax-table
  (setq rpm-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" rpm-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " rpm-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " rpm-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " rpm-mode-syntax-table)
  (modify-syntax-entry ?/ "." rpm-mode-syntax-table)
  (modify-syntax-entry ?* "." rpm-mode-syntax-table)
  (modify-syntax-entry ?+ "." rpm-mode-syntax-table)
  (modify-syntax-entry ?- "." rpm-mode-syntax-table)
  (modify-syntax-entry ?= "." rpm-mode-syntax-table)
  (modify-syntax-entry ?% "_" rpm-mode-syntax-table)
  (modify-syntax-entry ?< "." rpm-mode-syntax-table)
  (modify-syntax-entry ?> "." rpm-mode-syntax-table)
  (modify-syntax-entry ?& "." rpm-mode-syntax-table)
  (modify-syntax-entry ?| "." rpm-mode-syntax-table)
  (modify-syntax-entry ?\' "." rpm-mode-syntax-table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functions for rpm mode  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rpm-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

(defun rpm-ldconfig ()
  "Replace `%run_ldconfig', which is deprecated, with direct calls to `/sbin/ldconfig'."
  (interactive)          
  (let ((old-pnt (point-marker)))
    (beginning-of-buffer)
    (replace-string "%run_ldconfig" "/sbin/ldconfig")
    (goto-char old-pnt)))


(defun rpm-lang-files ()
  "Insert typical lang subpackage text."
  (interactive)
  (insert (format "%%files lang -f %%{name}.lang\n")))

(defun rpm-fdupes ()
  "Insert typical %fdupes text."
  (interactive)
  (insert (format "%%fdupes %buildroot\n")))


(defun rpm-br-split ()
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
      (message "You can only run rpm-br-split on a line that starts with BuildRequires:"))))



;;;###autoload
(add-to-list 'auto-mode-alist '("\\.spec\\'" . rpm-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO                                                           ;;
;;                                                                ;;
;; need to define syntax table and follow the sample mode example ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'derived)

(define-derived-mode rpm-mode c-mode "RPM"
   "Major mode for editing RPM spec files.
Special commands:
\\{rpm-mode-map}"

   (make-local-variable 'paragraph-start)
   (setq paragraph-start (concat "$\\|" page-delimiter))
   (make-local-variable 'paragraph-separate)
   (setq paragraph-separate paragraph-start)
   (make-local-variable 'paragraph-ignore-fill-prefix)
   (setq paragraph-ignore-fill-prefix t)
   (make-local-variable 'require-final-newline)
   (setq require-final-newline t)
   (make-local-variable 'comment-start)
   (setq comment-start "# ")
   (make-local-variable 'comment-end)
   (setq comment-end "")
   (make-local-variable 'comment-column)
   (setq comment-column 32)
   (make-local-variable 'comment-start-skip)
   (setq comment-start-skip "#+ *")
   ;;Initialize font lock for GNU emacs.
   (make-local-variable 'font-lock-defaults)
   (setq font-lock-defaults '(rpm-font-lock-keywords nil t))
   (run-hooks 'rpm--mode-hook))


(defconst rpm-mode-version "0.0.1" "Version of `rpm-mode'.")

(defun rpm-about-rpm-mode (&optional arg)
  "About `rpm-mode'."
  (interactive "p")
  (message
   (concat "rpm-mode version "
           rpm-mode-version
           " by Togan Muftuoglu <toganm@opensuse.org> ")))

(provide 'rpm-mode)
;;; rpm-mode.el ends here


