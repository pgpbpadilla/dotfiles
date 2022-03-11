(setq 
 ;; Use Unix file-encoding
 default-buffer-file-coding-system 'utf-8-unix
 buffer-file-coding-system 'utf-8-unix
)

;; Use native font
(set-face-attribute 'default nil 
                    :family "Consolas"
                    :height 100
                    :width 'normal)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Windows-Files.html
;; avoid extra system calls to determine file attribute accurately on Windows
(setq w32-get-true-file-attributes nil)
