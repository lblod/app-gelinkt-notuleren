(define-resource versioned-behandeling ()
  :class (s-prefix "ext:VersionedBehandeling")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsBehandeling")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesBehandeling")
                                 :inverse t
                                 :as "published-resource")
             (zitting :via ,(s-prefix "ext:hasVersionedBehandeling")
                                 :inverse t
                                 :as "zitting")
             (behandeling-van-agendapunt :via ,(s-prefix "ext:behandeling")
                                         :as "behandeling"))
  :resource-base (s-url "http://data.lblod.info/prepublished-behandeling/")
  :features '(include-uri)
  :on-path "versioned-behandelingen")