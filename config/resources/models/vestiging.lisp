(define-resource vestiging ()
  :class (s-prefix "org:Site")
  :has-one `((contact-punt :via ,(s-prefix "schema:contactPoint")
                           :as "vestigingsadres"))
  :resource-base (s-url "http://data.lblod.info/id/vestiging/")
  :features '(include-uri)
  :on-path "vestigingen"
)