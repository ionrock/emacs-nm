(require 's)

(defun nm/run (cmd)
  "Run CMD and return the output as a chomp'd string."
  (s-chomp (shell-command-to-string cmd)))


(defconst nm-buffer-name "*nm*"
  "Name of NetworkManager mode buffer.")

(defvar nm-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'nm-connect)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "p") 'previous-line)
    map)
  "Keymap for `nm-mode'.")

(defun nm-connect ()
  "connect to the network at point."
  (interactive)
  (nm/run (concat "nmcli device wifi connect " (tabulated-list-get-id))))


(defun nm/filter-by-vpn (line)
  (let ((entry (split-string line ":")))
    (if (string-equal (nth 1 entry) "vpn")
	(car entry))))

(defun nm/vpns ()
  (-map
   (lambda (line)
     (car (split-string line ":")))
   (-filter 'nm/filter-by-vpn
	    (split-string (nm/run "nmcli -t -f name,type c list") "\n"))))

(defun nm/vpn-connected (ap)
  (not (s-starts-with? "Error" (nm/run (format "nmcli c status id %s" ap)))))

;;;###autoload
(defun nm-vpn (ap)
  "Connect to a configured VPN."
  (interactive
   (list
    (let ((vpns (nm/vpns)))
      (if (> (length vpns) 1)
	  (completing-read "What VPN? " vpns nil t))
      (car vpns))))
  (if (not (nm/vpn-connected ap))
      (gnomenm-connect ap)))

(defvar nm-wifi-list-format
  [("SSID" 23 t)
   ("MODE" 20 t)
   ("SIGNAL" 8 t)
   ("SECURITY" 10 t)
   ("ACTIVE" 6)])

(defun nm-wifi-list-entries ()
  (-map
   (lambda (line)
     (let ((cols (split-string line ":")))
       (list
	(car cols)
	(vconcat
	 (-map
	  (lambda (field)
	    (s-chop-prefix "'" (s-chop-suffix "'" field)))
	  (split-string line ":"))))))
   (split-string
    (nm/run "nmcli -t -f SSID,MODE,SIGNAL,SECURITY,ACTIVE device wifi")
    "\n")))

(defun nm-buffer ()
  (get-buffer "*nm*"))

;;;###autoload
(define-derived-mode nm-mode tabulated-list-mode "NetworkManager"
  "Special mode for network manager buffers."
  (buffer-disable-undo)
  (kill-all-local-variables)
  (setq truncate-lines t)
  (setq mode-name "NetworkManager")
  (setq major-mode 'nm-mode)
  (use-local-map nm-mode-map)
  (setq tabulated-list-format nm-wifi-list-format)
  (setq tabulated-list-entries 'nm-wifi-list-entries)
  (setq tabulated-list-sort-key '("SSID" . nil))
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode 1))

;;;###autoload
(defun nm ()
  "Manage external services from within Emacs."
  (interactive)
  (let ((buffer-p (nm-buffer))
        (buffer (get-buffer-create nm-buffer-name)))
    (pop-to-buffer buffer)
    (unless buffer-p
      (nm-mode))))

(provide 'nm)
