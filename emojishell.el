;;; emojishell.el --- EmojiShell: emoji prompt for eshell -*- lexical-binding: t -*-
;;; Documentation:

;;; For simple usage:
;; load and `(setf eshell-prompt-function #'emojishell-emoji-prompt)'.

;;; For develop memo:
;; Package Variables:
;; + `emojishell-{normal|error|remote}-emoji-sets'
;;   Random emoji prompt sets for {normal|error|remote} usage.
;;
;; + `emojishell-path-name-shorten-trigger-length'
;;   `emojishell-path-name-maximum-length'
;;   Used to custom how the path name is reduced.
;;
;; + `emojishell-emoji-{normal|error}-face'
;;   `emojishell-{remote|path|git-branch-prompt}-face'
;;   Custom faces for emoji prompt and corresponding components.
;;
;; Package Functions:
;; + `emojishell-emoji-prompt'
;;   Eshell emoji prompt
;;
;; + `emojishell-shorten-path-name'
;;   `emojishell-path'
;;   Naive path name shorten method.
;;
;; + `eshell-previous-matching-input-from-input'
;;   *NOTE*: overwrite the function defined in eshell.el.
;;   Just to make sure eshell could update its prompt each time
;;   when the prompt is changed.
;;
;; misc functions
;; + `emojishell-luck-in'
;;   `emojishell-pick'
;;   Random pick functions.
;;
;; + `emojishell-with-face'
;;   Bind string with face
;;
;; + `emojishell-remote-p'
;; + `emojishell-git-branch'
;;

;; Credits:
;; + 絵文字大集合♪
;;   http://www.measure.jp/meal/report/others/emoji.htm

;;; Code:

;; require vc-git for `emojishell'
(autoload 'vc-git-branches "vc-git")

;; cusom variables
(defcustom emojishell-normal-emoji-sets
  '("[´･ᴗ･`]" "[´･ω･`]" "[ •_•]" "[•_• ]" "[੧ᐛ੭]" "[ง˙o˙]ว" "૧[●´৺`●]૭" "[ﾟ∀ﾟ*]"
    "♪[●^o^●]" "[∂_∂]" "ﾍ[∩_∩]ﾉ" "[|:3P]" "[=^.^=]" "[^_-]-☆")
  "A set of char displayed for normal prompt."
  :type 'list
  :group 'emojishell)

(defcustom emojishell-error-emoji-sets
  '("ﾍ[´Д`]ﾉ" "[;◔౪◔]" "ε=ε=ヾ[;ﾟдﾟ]/" "[ﾟДﾟ≡ﾟдﾟ]" "[||ﾟДﾟ]" "[▼皿▼]"
    "[;¬_¬]" "ﾍ[`⌒´]ﾉ" "[>_<;;]" "[-ι-;;]" "[ToT]" "[T^T]")
  "A set of char displayed for error prompt."
  :type 'list
  :group 'emojishell)

(defcustom emojishell-remote-emoji-sets
  '("|ω・]")
  "A set of char displayed for remote status."
  :type 'list
  :group 'emojishell)

(defcustom emojishell-path-name-shorten-trigger-length 30
  "The minimum path length to trigger `emojishell-shorten-path-name'."
  :type 'list
  :group 'emojishell)

(defcustom emojishell-path-name-maximum-length 80
  "The maximum path length after `emojishell-shorten-path-name'."
  :type 'integer
  :group 'emojishell)

;; custom faces
(defface emojishell-emoji-normal-face
  '((t :background "gold" :foreground "black"))
  "Eshell normal emoji prompt face."
  :group 'emojishell)

(defface emojishell-emoji-error-face
  '((t :background "red" :foreground "white"))
  "Eshell error emoji prompt face."
  :group 'emojishell)

(defface emojishell-remote-face
  '((t :foreground "red"))
  "Eshell remote face."
  :group 'emojishell)

(defface emojishell-path-face
  '((t :foreground "dim gray"))
  "Eshell path face."
  :group 'emojishell)

(defface emojishell-git-branch-face
  '((t :foreground "gray"))
  "Eshell git branch face."
  :group 'emojishell)

(defface emojishell-prompt-face
  '((t :foreground "gold"))
  "Eshell prompt face."
  :group 'emojishell)

;; custom functions
(defun emojishell-remote-p ()
  "Check if work remotely."
  (tramp-tramp-file-p default-directory))

(defun emojishell-git-branch ()
  "Return current git branch, if no, return `nil'."
  (let ((branch (car (vc-git-branches))))
    (if branch branch nil)))

(defun emojishell-shorten-path-name (path-name)
  "Try to make the `path-name' shorten than
  `emojishell-path-name-shorten-trigger-length'. "
  (let* ((path (split-string path-name "/"))
         (cnt  (1- (length path)))
         (len  (length path-name)))
    (string-join
     (cl-loop for rest = path then (cdr rest)
              for elem = (car rest)
              for i below cnt
              for name = (string-join
                          (mapcar (lambda (s) (substring s 0 1))
                                  (string-split elem "[-_\\. ]+" t)))
              while (> len emojishell-path-name-shorten-trigger-length)
              do (setf len (- len (length name)))
              collect name into shortened
              finally (return (append shortened rest)))
     "/")))

(defun emojishell-path ()
  "Return path of current working directory.
  If the length of path name is longer `emojishell-path-name-shorten-trigger-length',
  try to shorten the path name; and if the shortened path name is still longer than
  `emojishell-path-name-maximum-length', try to cutoff the path name directly."
  (let ((path-name (abbreviate-file-name (eshell/pwd))))
    (if (length> path-name emojishell-path-name-shorten-trigger-length)
        (let ((shortened (emojishell-shorten-path-name path-name)))
          (if (length> shortened emojishell-path-name-maximum-length)
              (concat "..."
                      (substring path-name
                                 (- (length shortened)
                                    emojishell-path-name-maximum-length)
                                 (1- (length shortened))))
            shortened))
      path-name)))

(defun emojishell-luck-in (&optional posibility limit)
  "Return `t' or `nil' at `possibility' chance."
  (let ((p (or posibility 0.5))
        (lim (or limit 100)))
    (if (< (/ (random lim) (float lim)) p) t nil)))

(defun emojishell-pick (lst)
  "Pick random element from `lst'."
  (cl-loop for elem in lst
           for count from (length lst) downto 1
           if (emojishell-luck-in (/ 1.0 count)) return elem))

(defmacro emojishell-with-face (string face)
  "Bind `string' with `face'."
  `(propertize ,string 'face ,face))

(defun emojishell-emoji-prompt ()
  "Eshell emoji prompt."
  (setf eshell-prompt-regexp "^[^#> ]* [#>] ")
  (concat
   ;; first line: remote path git
   (when (emojishell-remote-p)
     (concat (emojishell-with-face (emojishell-pick emojishell-remote-emoji-sets)
                            'emojishell-remote-face)
             " "))
   (emojishell-with-face (emojishell-path) 'emojishell-path-face)
   (let ((branch (emojishell-git-branch)))
     (when branch
       (concat " " (emojishell-with-face branch 'emojishell-git-branch-face))))
   "\n"

   ;; second line: status-emoji prompt
   (if (zerop eshell-last-command-status)
       (emojishell-with-face (emojishell-pick emojishell-normal-emoji-sets)
                      'emojishell-emoji-normal-face)
     (emojishell-with-face (emojishell-pick emojishell-error-emoji-sets)
                    'emojishell-emoji-error-face))

   (emojishell-with-face (if (= (user-uid) 0) " #" " >") 'emojishell-prompt-face)
   " "))

(provide 'emojishell)
;; emojishell.el ends here
