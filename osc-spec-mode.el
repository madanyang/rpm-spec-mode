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
;;  project and does not replace osc commands. derived from on sh-mode

;;  Put this in your .emacs file to enable autoloading
;;  of osc-spec-mode, and auto-recognition of ".spec" files:
;;
;;  (autoload 'osc-spec-mode "osc-spec-mode.el" "RPM spec mode." t)
;;  (setq auto-mode-alist (append '(("\\.spec" . osc-spec-mode))
;;                                auto-mode-alist))

;; TODO use auto-insert mode to initiliaze the spec
;;  (add-hook 'find-file-hook 'auto-insert)'

;;; Code:

(define-derived-mode osc-spec-mode sh-mode "osc-spec mode"
"Major mode for rpm spec files.
          \\{osc-spec-mode-map}"
(setq case-fold-search nil))

(defvar osc-spec-mode-syntax-table nil
  "Syntax table in use in `osc-spec-mode' buffers.")
(unless osc-spec-mode-syntax-table
  (setq osc-spec-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\\ "\\" osc-spec-mode-syntax-table)
  (modify-syntax-entry ?\n ">   " osc-spec-mode-syntax-table)
  (modify-syntax-entry ?\f ">   " osc-spec-mode-syntax-table)
  (modify-syntax-entry ?\# "<   " osc-spec-mode-syntax-table)
  (modify-syntax-entry ?/ "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?* "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?+ "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?- "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?= "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?% "_" osc-spec-mode-syntax-table)
  (modify-syntax-entry ?< "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?> "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?& "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?| "." osc-spec-mode-syntax-table)
  (modify-syntax-entry ?\' "." osc-spec-mode-syntax-table))



(defvar osc-spec-mode-map nil
  "Keymap used in `osc-spec-mode'.")
(unless osc-spec-mode-map
  (setq osc-spec-mode-map (make-sparse-keymap))
  (and (functionp 'set-keymap-name)
       (set-keymap-name osc-spec-mode-map 'osc-spec-mode-map))
  (define-key osc-spec-mode-map "\C-c\C-c"  'rpm-change-tag)
  (define-key osc-spec-mode-map "\C-c\C-e"  'rpm-add-change-log-entry)
  (define-key osc-spec-mode-map "\C-c\C-i"  'rpm-insert-tag)
  (define-key osc-spec-mode-map "\C-c\C-n"  'rpm-forward-section)
  (define-key osc-spec-mode-map "\C-c\C-o"  'rpm-goto-section)
  (define-key osc-spec-mode-map "\C-c\C-p"  'rpm-backward-section)
  (define-key osc-spec-mode-map "\C-c\C-r"  'rpm-increase-release-tag)
  (define-key osc-spec-mode-map "\C-c\C-u"  'rpm-insert-true-prefix)
  (define-key osc-spec-mode-map "\C-c\C-ba" 'rpm-build-all)
  (define-key osc-spec-mode-map "\C-c\C-bb" 'rpm-build-binary)
  (define-key osc-spec-mode-map "\C-c\C-bc" 'rpm-build-compile)
  (define-key osc-spec-mode-map "\C-c\C-bi" 'rpm-build-install)
  (define-key osc-spec-mode-map "\C-c\C-bl" 'rpm-list-check)
  (define-key osc-spec-mode-map "\C-c\C-bp" 'rpm-build-prepare)
  (define-key osc-spec-mode-map "\C-c\C-bs" 'rpm-build-source)
  (define-key osc-spec-mode-map "\C-c\C-dd" 'rpm-insert-dir)
  (define-key osc-spec-mode-map "\C-c\C-do" 'rpm-insert-docdir)
  (define-key osc-spec-mode-map "\C-c\C-fc" 'rpm-insert-config)
  (define-key osc-spec-mode-map "\C-c\C-fd" 'rpm-insert-doc)
  (define-key osc-spec-mode-map "\C-c\C-ff" 'rpm-insert-file)
  (define-key osc-spec-mode-map "\C-c\C-fg" 'rpm-insert-ghost)
  (define-key osc-spec-mode-map "\C-c\C-xa" 'rpm-toggle-add-attr)
  (define-key osc-spec-mode-map "\C-c\C-xb" 'rpm-change-buildroot-option)
  (define-key osc-spec-mode-map "\C-c\C-xc" 'rpm-toggle-clean)
  (define-key osc-spec-mode-map "\C-c\C-xd" 'rpm-toggle-nodeps)
  (define-key osc-spec-mode-map "\C-c\C-xf" 'rpm-files-group)
  (define-key osc-spec-mode-map "\C-c\C-xg" 'rpm-toggle-sign-gpg)
  (define-key osc-spec-mode-map "\C-c\C-xi" 'rpm-change-timecheck-option)
  (define-key osc-spec-mode-map "\C-c\C-xn" 'rpm-toggle-nobuild)
  (define-key osc-spec-mode-map "\C-c\C-xo" 'rpm-files-owner)
  (define-key osc-spec-mode-map "\C-c\C-xr" 'rpm-toggle-rmsource)
  (define-key osc-spec-mode-map "\C-c\C-xq" 'rpm-toggle-quiet)
  (define-key osc-spec-mode-map "\C-c\C-xs" 'rpm-toggle-short-circuit)
  (define-key osc-spec-mode-map "\C-c\C-xt" 'rpm-change-target-option)
  (define-key osc-spec-mode-map "\C-c\C-xu" 'rpm-files-umask)
  ;;(define-key osc-spec-mode-map "\C-q" 'indent-spec-exp)
  ;;(define-key rpm-spec-mode-map "\t" 'sh-indent-line)
  )

;;------------------------------------------------------------
;; variables used by navigation functions.

(defconst rpm-sections
  '("preamble" "description" "prep" "setup" "build" "install" "check" "clean"
    "changelog" "files")
  "Partial list of section names.")
(defvar rpm-section-list
  '(("preamble") ("description") ("prep") ("setup") ("build") ("install")
    ("check") ("clean") ("changelog") ("files"))
  "Partial list of section names.")
(defconst rpm-scripts
  '("pre" "post" "preun" "postun"
    "trigger" "triggerin" "triggerprein" "triggerun" "triggerpostun"
    "pretrans" "posttrans")
  "List of rpm scripts.")
(defconst rpm-section-seperate "^%\\(\\w+\\)\\s-")
(defconst rpm-section-regexp
  (eval-when-compile
    (concat "^%"
            (regexp-opt
             ;; From RPM 4.6.0 sources, file build/parseSpec.c: partList[].
             '("build" "changelog" "check" "clean" "description" "files"
               "install" "package" "post" "postun" "pretrans" "posttrans"
               "pre" "prep" "preun" "trigger" "triggerin" "triggerpostun"
               "triggerprein" "triggerun" "verifyscript") t)
            "\\b"))
  "Regular expression to match beginning of a section.")

;;------------------------------------------------------------






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

(defvar rpm-tags-regexp
  (concat "\\(\\<" (regexp-opt (mapcar 'car rpm-tags-list))
	  "\\|\\(Patch\\|Source\\)[0-9]+\\>\\)")
  "Regular expression for matching valid tags.")

(defvar rpm-obsolete-tags-list
  ;; From RPM sources, file build/parsePreamble.c: preambleList[].
  '(("Copyright")    ;; 4.4.2
    ("RHNPlatform")  ;; 4.4.2, 4.4.9
    ("Serial")       ;; 4.4.2, 4.4.9
    )
  "List of elements that are obsolete tags in some versions of rpm.")

(defvar rpm-obsolete-tags-regexp
  (regexp-opt (mapcar 'car rpm-obsolete-tags-list) 'words)
  "Regular expression for matching obsolete tags.")




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

;;------------------------------------------------------------

(defface osc-spec-tag-face
  '(( ((class color) (background light)) (:foreground "blue3") )
    ( ((class color) (background dark)) (:foreground "blue") ))
  "*Face for tags."
  :group 'osc-spec-faces)

(defface osc-spec-obsolete-tag-face
  '(( ((class color)) (:foreground "white" :background "red") ))
  "*Face for obsolete tags."
  :group 'osc-spec-faces)

(defface osc-spec-macro-face
  '(( ((class color) (background light)) (:foreground "purple") )
    ( ((class color) (background dark)) (:foreground "yellow") ))
  "*Face for OSC macros and variables."
  :group 'osc-spec-faces)

(defface osc-spec-var-face
  '(( ((class color) (background light)) (:foreground "maroon") )
    ( ((class color) (background dark)) (:foreground "maroon") ))
  "*Face for environment variables."
  :group 'osc-spec-faces)

(defface osc-spec-doc-face
  '(( ((class color) (background light)) (:foreground "magenta3") )
    ( ((class color) (background dark)) (:foreground "magenta") ))
  "*Face for %doc entries in %files."
  :group 'osc-spec-faces)

(defface osc-spec-dir-face
  '(( ((class color) (background light)) (:foreground "green4") )
    ( ((class color) (background dark)) (:foreground "green") ))
  "*Face for %dir entries in %files."
  :group 'osc-spec-faces)

(defface osc-spec-package-face
  '(( ((class color) (background light)) (:foreground "red3") )
    ( ((class color) (background dark)) (:foreground "red") ))
  "*Face for package tag."
  :group 'osc-spec-faces)

(defface osc-spec-ghost-face
  '(( ((class color) (background light)) (:foreground "gray50") )
    ( ((class color) (background dark)) (:foreground "red") ))
  "*Face for %ghost and %config entries in %files."
  :group 'osc-spec-faces)

(defface osc-spec-section-face
  '(( ((class color) (background light)) (:foreground "purple" :underline t) )
    ( ((class color) (background dark)) (:foreground "yellow" :underline t) ))
  "*Face for section markers."
  :group 'osc-spec-faces)

;;; GNU emacs font-lock needs these...
(defvar osc-spec-macro-face
  'osc-spec-macro-face "*Face for RPM macros and variables.")
(defvar osc-spec-var-face
  'osc-spec-var-face "*Face for environment variables.")
(defvar osc-spec-tag-face
  'osc-spec-tag-face "*Face for tags.")
(defvar osc-spec-obsolete-tag-face
  'osc-spec-tag-face "*Face for obsolete tags.")
(defvar osc-spec-package-face
  'osc-spec-package-face "*Face for package tag.")
(defvar osc-spec-dir-face
  'osc-spec-dir-face "*Face for %dir entries in %files.")
(defvar osc-spec-doc-face
  'osc-spec-doc-face "*Face for %doc entries in %files.")
(defvar osc-spec-ghost-face
  'osc-spec-ghost-face "*Face for %ghost and %config entries in %files.")
(defvar osc-spec-section-face
  'osc-spec-section-face "*Face for section markers.")




(defvar osc-spec-font-lock-keywords
  (list
   (cons rpm-section-regexp osc-spec-section-face)
   '("%[a-zA-Z0-9_]+" 0 osc-spec-macro-face)
   (cons (concat "^" rpm-obsolete-tags-regexp "\\(\([a-zA-Z0-9,_]+\)\\)[ \t]*:")
         '((1 'osc-spec-obsolete-tag-face)
           (2 'osc-spec-ghost-face)))
   (cons (concat "^" rpm-tags-regexp "\\(\([a-zA-Z0-9,_]+\)\\)[ \t]*:")
         '((1 'osc-spec-tag-face)
           (3 'osc-spec-ghost-face)))
   (cons (concat "^" rpm-obsolete-tags-regexp "[ \t]*:")
         '(1 'osc-spec-obsolete-tag-face))
   (cons (concat "^" rpm-tags-regexp "[ \t]*:")
         '(1 'osc-spec-tag-face))
   '("%\\(de\\(fine\\|scription\\)\\|files\\|global\\|package\\)[ \t]+\\([^-][^ \t\n]*\\)"
     (3 osc-spec-package-face))
   '("%p\\(ost\\|re\\)\\(un\\|trans\\)?[ \t]+\\([^-][^ \t\n]*\\)"
     (3 osc-spec-package-face))
   '("%configure " 0 osc-spec-macro-face)
   '("%dir[ \t]+\\([^ \t\n]+\\)[ \t]*" 1 osc-spec-dir-face)
   '("%doc\\(dir\\)?[ \t]+\\(.*\\)\n" 2 osc-spec-doc-face)
   '("%\\(ghost\\|config\\([ \t]*(.*)\\)?\\)[ \t]+\\(.*\\)\n"
     3 osc-spec-ghost-face)
   '("^%.+-[a-zA-Z][ \t]+\\([a-zA-Z0-9\.-]+\\)" 1 osc-spec-doc-face)
   '("^\\(.+\\)(\\([a-zA-Z]\\{2,2\\}\\)):"
     (1 osc-spec-tag-face)
     (2 osc-spec-doc-face))
   '("^\\*\\(.*[0-9] \\)\\(.*\\)<\\(.*\\)>\\(.*\\)\n"
     (1 osc-spec-dir-face)
     (2 osc-spec-package-face)
     (3 osc-spec-tag-face)
     (4 osc-spec-ghost-face))
   '("%{[^{}]*}" 0 osc-spec-macro-face)
   '("$[a-zA-Z0-9_]+" 0 osc-spec-var-face)
   '("${[a-zA-Z0-9_]+}" 0 osc-spec-var-face)
   )
  "Additional expressions to highlight in `osc-spec-mode'.")

;;Initialize font lock for xemacs
(put 'osc-spec-mode 'font-lock-defaults '(osc-spec-font-lock-keywords))

(defvar osc-spec-mode-abbrev-table nil
  "Abbrev table in use in `osc-spec-mode' buffers.")
(define-abbrev-table 'osc-spec-mode-abbrev-table ())




;; (require 'cl)
;; (require 'dired)
;; (defconst osc-spec-mode-version "0.0.1" "Version of `osc-spec-mode'.")

;; (defgroup osc-spec nil
;;   "RPM spec mode with Emacs/XEmacs enhancements."
;;   :prefix "osc-spec-"
;;   :group 'languages)

;; (defcustom osc-owner ""
;;   "This variable holds your email address, to be used in changelogs."
;;   :type 'string
;;   :group 'osc-spec)




;; (defun mark-line ()
;;   (interactive)
;;   (beginning-of-line)
;;   (set-mark (point-marker))
;;   (end-of-line))

;; (defun mark-to-end ()
;;   (interactive)
;;   (set-mark (point-marker))
;;   (end-of-line))

;; ;; create the auto-insert thingys read auto-insert and auto-insert-alist functions

;; ;; (("\\.spec" . "rpm spec skeleton ")
;; ;; "#\n# spec file for" (file-name-nondirectory (buffer-file-name) "\n"
;; ;; "# Copyright (C) " (substring (current-time-string) -4) "   "
;; ;;   (getenv "ORGANIZATION")
;; ;;   |
;; ;;   (progn user-full-name)
;; ;;   "\n\n#"
;; ;;   (user-full-name)
;; ;;   '(if
;; ;;        (search-backward "&"
;; ;; 			(line-beginning-position)
;; ;; 			t)
;; ;;        (replace-match
;; ;; 	(capitalize
;; ;; 	 (user-login-name))
;; ;; 	t t))
;; ;;   '(end-of-line 1)
;; ;;   " <"
;; ;;   (progn user-mail-address) 





;; ;; borrowed from 
;; ;; http://wiki.mandriva.com/en/Development/Tasks/Packaging/Tools/Emacs
;; ;; 
;; ;; modified to fit needs

;; ;; (require 'ffap)
;; ;; (defun my-rpm-ffap (name)
;; ;;   (ffap-locate-file name '("" ".gz" ".bz2") 
;; ;;                '("./" "../SOURCES")))              
;; ;; (add-to-list 'ffap-alist 
;; ;;           '(rpm-spec-mode . my-rpm-ffap))

;; ;; borrowed from 
;; ;; http://wiki.mandriva.com/en/Development/Tasks/Packaging/Tools/Emacs
;; ;; 
;; ;; modified to fit needs


;; ;; (defun osc-insert-patch ()
;; ;;   (interactive)
;; ;;   (goto-char (point-min))
;; ;;   (let* ((file 
;; ;;      (completing-read 
;; ;;       "Patch: "
;; ;;       (mapcar (function (lambda (rule) (list rule)))
;; ;;             (directory-files "../SOURCES/" nil "^\\([_-0-9a-zA-Z]+\\).''\\.patch.'''"))) )
;; ;;        (max (search-forward-regexp rpm-section-regexp))
;; ;;        (count 0)
;; ;;        )
;; ;;    (goto-char (point-min))
;; ;;    (while (search-forward-regexp "^Patch?\\([0-9]+\\)?" max t) 
;; ;;      (if (> (string-to-int (match-string 1)) count)
;; ;;         (setq count (string-to-int (match-string 1)))
;; ;;       )
;; ;;      )
;; ;;    (if (eq count 0) (while (search-forward-regexp "^Source?\\([0-9]+\\)?" max t)()))   
;; ;;    (setq count (1+ count))
;; ;;    (end-of-line)
;; ;;    (insert (format "\n%s%d%s%s" "Patch" count ": " file))
;; ;;    (goto-char (point-min))
;; ;;    (if (search-forward-regexp "^%patch?\\([0-9]+\\)?" nil t)
;; ;;       (progn
;; ;;         (beginning-of-line)
;; ;;         (while (search-forward-regexp "^%patch?\\([0-9]+\\)?" nil t) ())
;; ;;         )
;; ;;      (search-forward-regexp "^%setup" nil t))

;; ;;    (end-of-line)
;; ;;    (insert (format "\n%s%d%s" "%patch" count " -p1 "))
;; ;;    (let ((name (rpm-spec-field-value "name" nil))
;; ;;         (version (rpm-spec-field-value "version" nil))
;; ;;         (string)
;; ;;         )
;; ;;      (cond ((string-match 
;; ;;            (concat "^" (regexp-quote (format "%s-%s-" name version))
;; ;;                  "\\([^.]*\\)\\(\\.patch\\.bz2\\)") file)
;; ;;           (setq string (format "%s%s" "-b ." 
;; ;;                           (substring file (match-beginning 1) (match-end 1)))))
;; ;;          ((string-match 
;; ;;            (concat "^" (regexp-quote (format "%s-" name version))
;; ;;                  ".''[0-9]+-" "\\([^.]'''\\)\\(\\.patch\\.bz2\\)") file)
;; ;;           (setq string 
;; ;;                (format "%s%s" "-b ." 
;; ;;                      (substring file (match-beginning 1) (match-end 1)))))
;; ;;          )
;; ;;      (if string
;; ;;         (insert string))
;; ;;      )))



;; (defun osc-pwd ()
;;   "Returns the current working directory, sans parent directories similar to pwd"
;;   (file-name-nondirectory (directory-file-name (expand-file-name default-directory))))



;; (defun osc-view-readonly (filename)
;;   "view a file readonly"
;;   (let* ((buffer-name (format "*%s*" filename)))
;;     (if (get-buffer buffer-name)
;;         (kill-buffer buffer-name))
;;     (let* ((output-buffer (get-buffer-create buffer-name))
;;            (outwin (display-buffer output-buffer nil t)))
;;       (save-selected-window
;;         (select-window outwin)
;;         (insert-file-contents filename)
;;         (setq buffer-read-only t)
;;         (goto-char (point-min))))))

;; (defun osc-view-changelog ()
;;   "something like \"less foo.changes\""  
;;   (interactive)

;;   (let ((filename (format "%s.changes" (osc-pwd))))
;;     (osc-view-readonly filename)))


;; (defun osc-view-spec ()
;;   "Open the .spec file in the current working directory in read-only mode."
;;   (interactive)

;;   ;;"something like \"less foo.spec\""
;;   (let ((filename (format "%s.spec" (osc-pwd))))
;;     (osc-view-readonly filename)))


;; (defun osc-edit-spec ()
;;   "Edit the .spec file found in the current working directory."
;;   (interactive)
;;   (let* ((filename (format "%s.spec" (osc-pwd)))
;;          (buf (find-file filename)))
;;     (switch-to-buffer buf))


;; (defun osc-remove-leading-plusses ()
;;   "Strip leading `+' characters from the working buffer.  This is
;; useful when copying and pasting diff output directly."
;;   (interactive)
;;   (let ((old-pnt (point-marker)))
;;     (beginning-of-buffer)
;;     (replace-regexp "^+" "")
;;     (goto-char old-pnt)
;;     (message "I am feeling nonplussed.")))

;; (defun osc-intltoolize-spec ()
;;   "Add intltoolize --force to the spec"
;;   (interactive)
;;   (let ((old-pnt (point-marker)))
;;     (beginning-of-buffer)
;;     (replace-regexp "^autoreconf.*$" "\\& intltoolize --force")

;;     (message "Now inspect what you've done, since it might not be
;;     correct.  If you're happy with the change(s) made, save this
;;     buffer, run osc-intltoolize-changelog, and save that buffer
;;     too.")))

;; (defun osc-intltoolize-changelog ()
;;   "Document what I did after intltoolizing a spec."
;;   (interactive)
;;   (osc-changelog)
;;   (insert (format "Fix the build when against newer versions of intltool.")))

;; ;;; Similar to vc on suse boxen.
;; ;;; FIXME: Make this smarter in the face of a) oddly named working directories,
;; ;;; b) "packages" with multiple .spec and .changes files, c) everything
;; ;;; else.

;; (defun osc-changelog ()

;;   "Edit the .changes file in the current working directory,
;; inserting the correct boilerplate text and positioning the cursor
;; for immediate typing.  This is similar to the 'vc' command in the
;; internal SUSE buildsystem. "

;;   (interactive)
;;   (if (string-equal "" osc-owner)
;;       (message "You need to customize the variable osc-owner before using this functionality.  To do so: M-x customize-group RET ab RET, then set `osc-owner' to your email address and save.")
;;     (let* ((filename (format "%s.changes" (osc-pwd)))
;;            (time-format "%a %b %e %H:%M:%S %Z %Y")
;;            (buf (find-file filename)))
;;       (switch-to-buffer buf)
;;       (goto-char (point-min))
;;       (insert (format "-------------------------------------------------------------------\n"))
;;       (insert (format "%s - %s\n" (format-time-string time-format) osc-owner))
;;       (insert (format "\n"))
;;       (insert (format "%s " "-"))
;;       (let ((old-pnt (point-marker)))
;;         (insert (format "\n\n"))
;;         (goto-char old-pnt)))))




;; (defun osc-br-split ()
;;   "Split a BuildRequires: line like this \"BuildRequires:  foo bar baz\" into
;; BuildRequires:  foo
;; BuildRequires:  bar
;; BuildRequires:  baz"
;;   (interactive)
;;   (save-excursion
;;     (beginning-of-line)
;;     (if (string-equal "BuildRequires" (word-at-point))
;;         (progn (beginning-of-line)
;;                (forward-word)
;;                (forward-word)
;;                (backward-char)
;;                (let ((here (point-marker)))
;;                  (progn (end-of-line)
;;                         (let ((there (point-marker)))
;;                           (progn (goto-char here)
;;                                  (let ((from " ")
;;                                        (to "\nBuildRequires:  "))
;;                                    (replace-string from to nil here there)))))))
;;       (message "You can only run osc-br-split on a line that starts with BuildRequires:"))))


;; (defun osc-ldconfig ()
;;   "Replace `%run_ldconfig', which is deprecated, with direct calls to `/sbin/ldconfig'."
;;   (interactive)          
;;   (let ((old-pnt (point-marker)))
;;     (beginning-of-buffer)
;;     (replace-string "%run_ldconfig" "/sbin/ldconfig")
;;     (goto-char old-pnt)))


;; (defun osc-lang-files ()
;;   "Insert typical lang subpackage text."
;;   (interactive)
;;   (insert (format "%%files lang -f %%{name}.lang\n")))

;; (defun osc-fdupes ()
;;   "Insert typical %fdupes text."
;;   (interactive)
;;   (insert (format "%%fdupes %buildroot\n")))


;; (defun osc-open-rej ()
;;   "When a patch fails to apply, it will leave behind files with
;; the suffix `.rej'. To use this command effectively, open the file
;; that was not successfully patched, and then run this command,
;; which will split the window and open the `.rej' file in one
;; window, and the unpatched or partially unpatched file in the
;; other."
;;   (interactive)
;;   (let* ((this-buffer (buffer-file-name))
;;          (rej-buffer (concat this-buffer ".rej")))
;;     (delete-other-windows)
;;     (split-window)
;;     (find-file rej-buffer)
;;     (revert-buffer t t)))

;; (defun osc-about-osc-spec-mode (&optional arg)
;;   "About `osc-spec-mode'."
;;   (interactive "p")
;;   (message
;;    (concat "osc-spec-mode version "
;;            osc-spec-mode-version
;;            " by Togan Muftuoglu <toganm@opensuse.org> ")))

;; ;;;###autoload
;; (add-to-list 'auto-mode-alist '("\\.spec\\(\\.in\\)?$" . osc-spec-mode))

;; (provide 'osc-spec-mode)

;; ;;; osc-spec-mode.el ends here
