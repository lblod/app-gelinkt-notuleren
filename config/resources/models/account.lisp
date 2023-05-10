(define-resource account ()
  :class (s-prefix "foaf:OnlineAccount")
  :resource-base (s-url "http://data.lblod.info/id/account/")
  :properties `((:provider :via ,(s-prefix "foaf:accountServiceHomepage"))
                (:vo-id :via ,(s-prefix "dct:identifier")))
  :has-one `((gebruiker :via ,(s-prefix "foaf:account")
                         :inverse t
                         :as "gebruiker"))
  :on-path "accounts"
)