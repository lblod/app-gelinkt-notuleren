;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORGANISATIE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; this is a shared domain file, maintained in https://github.com/lblod/domain-files
(define-resource vestiging ()
  :class (s-prefix "org:Site")
  :has-one `((contact-punt :via ,(s-prefix "schema:contactPoint")
                           :as "vestigingsadres"))
  :resource-base (s-url "http://data.lblod.info/id/vestiging/")
  :features '(include-uri)
  :on-path "vestigingen"
)

(define-resource contact-punt ()
  :class (s-prefix "schema:PostalAddress")
  :properties `((:land :string ,(s-prefix "schema:addressCountry"))
                (:gemeente :string ,(s-prefix "schema:addressLocality"))
                (:adres :string ,(s-prefix "schema:streetAddress"))
                (:postcode :string ,(s-prefix "schema:postalCode"))
                (:email :string ,(s-prefix "schema:email"))
                (:telephone :string ,(s-prefix "schema:telephone"))
                (:fax :string ,(s-prefix "schema:faxNumber"))
                (:website :string ,(s-prefix "schema:url")))
  :resource-base (s-url "http://data.lblod.info/id/contactpunt/")
  :features '(include-uri)
  :on-path "contact-punten"
  )

(define-resource functionaris ()
  ;;Functionaris is considered subclass of org:Membership as of now. TODO: publish this
  :class (s-prefix "lblod:Functionaris")
  :has-one `((tijdsinterval :via ,(s-prefix "org:memberDuring")
                            :as "lid-van-tot")
             (persoon :via ,(s-prefix "org:member")
                      :as "lid")
             (rol :via ,(s-prefix "org:role")
                  :as "rol")
             (bestuurseenheid :via ,(s-prefix "org:organization")
                              :as "is-lidmaatschap-bij")
             (functionaris-status-code :via ,(s-prefix "lblod:functionarisStatusCode")
                                       :as "status"))

  :resource-base (s-url "http://data.lblod.info/id/functionaris/")
  :features '(include-uri)
  :on-path "functionarissen"
  )

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

(define-resource rol ()
  :class (s-prefix "org:Role")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/concept/functionarisRol/")
  :features '(include-uri)
  :on-path "rollen"
)

(define-resource functionaris-status-code ()
  ;;TODO: publish this somewhere
  :class (s-prefix "lblod:FunctionarisStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/functionarisStatusCode/")
  :features '(include-uri)
  :on-path "functionaris-status-codes")
