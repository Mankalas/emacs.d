;;; Setup powershop markets
(defconst powershop-markets '("psnz" "psau" "psuk" "merx"))
(defvar powershop-market-history nil)
(defun powershop-read-market ()
  (completing-read "Market: " powershop-markets nil t
                   nil
                   'powershop-market-history
                   (or (car powershop-market-history) "psau")
                   ))
(provide 'init-powershop-markets)
