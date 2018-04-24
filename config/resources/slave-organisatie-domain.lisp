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

(define-resource positie ()
  :class (s-prefix "org:Post")
  :has-one `((rol :via ,(s-prefix "org:role")
                  :as "rol")
             (organisatie :via ,(s-prefix "org:hasPost")
                            :inverse t
                            :as "is-positie-in"))
  :has-many `((persoon :via ,(s-prefix "org:heldBy")
                       :as "wordt-ingevuld-door"))
  :resource-base (s-url "http://data.lblod.info/id/positie/")
  :features '(include-uri)
  :on-path '"posities"
)

(define-resource rol ()
  :class (s-prefix "org:Role")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/concept/bestuurseenheidRollen/")
  :features '(include-uri)
  :on-path "rollen"
)

(define-resource organisatie ()
  :class (s-prefix "org:Organization")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel")))
  :has-one `((vestiging :via ,(s-prefix "org:hasPrimarySite")
                        :as "primaire-site")
             )
  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contactinfo")
              (positie :via ,(s-prefix "org:hasPost")
                       :as "posities"))
  :resource-base (s-url "http://data.lblod.info/id/organisaties/")
  :features '(include-uri)
  :on-path "organisaties"
)

