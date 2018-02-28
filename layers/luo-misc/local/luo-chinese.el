(defvar emacs-english-font "Monaco"
  "The font name of English.")

(defvar emacs-cjk-font "PingFang SC"
  "The font name for CJK.")

(defvar emacs-font-size-pair '(12 . 14)
  "Default font size pair for (english . chinese)")

(defun font-exist-p (fontname)
  "Test if this font is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname)) nil t)))

(defun set-font (english chinese size-pair)
  "Setup emacs English and Chinese font on x window-system."

  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t))

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font (frame-parameter nil 'font) charset
                          (font-spec :family chinese :size (cdr size-pair))))))

;; Setup font size based on emacs-font-size-pair
(set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)

;; {%org-mode%}
;; here are 20 hanzi and 40 english chars, see if they are the same width
;; 你你你你你你你你你你你你你你你你你你你你
;; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
;; /aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa/
;; {%/org-mode%}

;; Local Variables:
;; eval: (mmm-mode 1)
;; End:
