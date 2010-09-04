(package (ocelotl (0))
  (depends (srfi)
           (spells)
           (wak-riastreams)
           (wak-foof-loop)
           (wak-parscheme)
           (wak-ssax))
  
  (synopsis "library collection centered around HTTP")
  (description
   "Ocelotl provides the following facilities:"
   " - An HTTP 1.0 client"
   " - RFC822-style header parsing"
   " - Percent-encoding and -decoding"
   " - An URI abstraction"
   " - An HTTP server skeleton"
   ""
   "An actual HTTP server implementation based on the skeleton"
   "can be found in the `ocelotl-soup' package.")
  
  (libraries
   (exclude ("net" "soup-httpd.sls"))
   (sls -> "ocelotl")
   (("net" "private") -> ("ocelotl" "net" "private"))))

(package (ocelotl-soup (0))
  (depends (srfi)
           (spells)
           (sbank)
           (ocelotl))
  
  (synopsis "HTTP server based on libsoup")
  (description
   "This package implements an HTTP server based on the skeleton"
   "provided by the `ocelotl' package.")
  
  (libraries
   (("net" "soup-httpd.sls") -> ("ocelotl" "net" "soup-httpd.sls"))))

;; Local Variables:
;; scheme-indent-styles: ((package 1))
;; End:
