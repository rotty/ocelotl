(package (ocelotl (0))
  (depends (srfi)
           (spells)
           (parscheme))
  (libraries
   (exclude ("net" "soup-httpd.sls"))
   (sls -> "ocelotl")
   (("net" "private") -> ("ocelotl" "net" "private"))))

(package (ocelotl-soup (0))
  (depends (srfi)
           (spells)
           (sbank)
           (ocelotl))
  (libraries
   (("net" "soup-httpd.sls") -> ("ocelotl" "net" "soup-httpd.sls"))))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
