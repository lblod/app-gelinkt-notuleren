(define-resource positie ()
  :class (s-prefix "org:Post")
  :has-one `((contact-punt :via ,(s-prefix "schema:contactPoint")
                           :as "contactinfo")
             (rol :via ,(s-prefix "org:role")
                  :as "rol")
             (bestuurseenheid :via ,(s-prefix "org:hasPost")
                            :inverse t
                            :as "is-positie-in"))
  :resource-base (s-url "http://data.lblod.info/id/positie/")
  :features '(include-uri)
  :on-path "posities"
)