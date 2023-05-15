(in-package :mu-cl-resources)

(defparameter *cache-count-queries* nil)
(defparameter *supply-cache-headers-p* t
  "when non-nil, cache headers are supplied.  this works together with mu-cache.")
;;(setf *cache-model-properties-p* t)
(defparameter *include-count-in-paginated-responses* t
  "when non-nil, all paginated listings will contain the number
   of responses in the result object's meta.")
(defparameter *max-group-sorted-properties* nil)
(setf sparql:*experimental-no-application-graph-for-sudo-select-queries* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource file ()
  :class (s-prefix "nfo:FileDataObject")
  :properties `((:name :string ,(s-prefix "nfo:fileName"))
                (:format :string ,(s-prefix "dct:format"))
                (:size :number ,(s-prefix "nfo:fileSize"))
                (:extension :string ,(s-prefix "dbpedia:fileExtension"))
                (:created :datetime ,(s-prefix "nfo:fileCreated")))
  :has-one `((file :via ,(s-prefix "nie:dataSource")
                   :inverse t
                   :as "download"))
  :resource-base (s-url "http://data.example.com/files/")
  :features `(include-uri)
  :on-path "files"
)

(define-resource concept ()
  :class (s-prefix "skos:Concept")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:notation :string ,(s-prefix "skos:notation"))
                (:search-label :string ,(s-prefix "ext:searchLabel")))
  :has-many `((concept-scheme :via ,(s-prefix "skos:inScheme")
                              :as "concept-schemes")
              (concept-scheme :via ,(s-prefix "skos:topConceptOf")
                              :as "top-concept-schemes"))
  :resource-base (s-url "http://lblod.data.gift/concepts/")
  :features `(include-uri)
  :on-path "concepts"
)

(define-resource concept-scheme ()
  :class (s-prefix "skos:ConceptScheme")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((concept :via ,(s-prefix "skos:inScheme")
                       :inverse t
                       :as "concepts")
              (concept :via ,(s-prefix "skos:topConceptOf")
                       :inverse t
                       :as "top-concepts"))
  :resource-base (s-url "http://lblod.data.gift/concept-schemes/")
  :features `(include-uri)
  :on-path "concept-schemes"
)

(define-resource identificator ()
  :class (s-prefix "adms:Identifier")
  :properties `((:identificator :string ,(s-prefix "skos:notation"))) ;; TODO: should have a specific type
  :resource-base (s-url "http://data.lblod.info/id/identificatoren/")
  :features '(include-uri)
  :on-path "identificatoren"
)

(define-resource rdfs-class ()
  :class (s-prefix "rdfs:Class")
  :properties `((:label :string ,(s-prefix "rdfs:label"))
                (:description :string ,(s-prefix "rdfs:comment"))
                (:api-path :string ,(s-prefix "ext:apiPath"))
                (:display-properties :string ,(s-prefix "ext:displayProperties"))
                (:base-uri :string ,(s-prefix "ext:baseUri"))
                (:api-filter :string ,(s-prefix "ext:apiFilter"))
                (:is-primitive :bool ,(s-prefix "ext:isPrimitive"))
                (:rdfa-type :uri ,(s-prefix "ext:rdfaType"))
                (:json-api-type :string ,(s-prefix "ext:jsonApiType")))

  :has-many `((rdfs-property :via ,(s-prefix "ext:rdfsClassProperties")
                        :as "properties"))

  :resource-base (s-url "http://data.lblod.info/id/rdfs-classes/")
  :features '(include-uri)
  :on-path "rdfs-classes"
)

(define-resource rdfs-property ()
  :class (s-prefix "rdfs:Property")
  :properties `((:label :string ,(s-prefix "rdfs:label"))
                (:rdfa-type :uri ,(s-prefix "ext:rdfaType")))
  :has-many `((rdfs-class :via ,(s-prefix "ext:rdfsClassProperties")
                     :inverse t
                     :as "domain"))
  :has-one `((rdfs-class :via ,(s-prefix "rdfs:range")
                     :as "range"))
  :resource-base (s-url "http://data.lblod.info/id/rdfs-properties/")
  :features '(include-uri)
  :on-path "rdfs-properties"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AUTH MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define-resource gebruiker ()
  :class (s-prefix "foaf:Person")
  :resource-base (s-url "http://data.lblod.info/id/gebruiker/")
  :properties `((:voornaam :string ,(s-prefix "foaf:firstName"))
                (:achternaam :string ,(s-prefix "foaf:familyName"))
                (:rijksregister-nummer :string ,(s-prefix "dct:identifier")))
  :has-many `((account :via ,(s-prefix "foaf:account")
                       :as "account")
              (bestuurseenheid :via ,(s-prefix "foaf:member")
                              :as "bestuurseenheden"))
  :on-path "gebruikers"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITOR MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource attachment ()
  :class (s-prefix "ext:Attachment")
  :properties `((:decision :uri ,(s-prefix "dct:isPartOf")))
  :has-one `((document-container :via ,(s-prefix "ext:hasAttachments")
                            :inverse t
                            :as "document-container")
             (file :via ,(s-prefix "ext:hasFile")
                            :as "file")
                   (concept :via ,(s-prefix "ext:attachmentType")
                            :as "type"))
  :resource-base (s-url "http://lblod.data.gift/attachment/")
  :features `(include-uri)
  :on-path "attachments"
)

(define-resource document-container ()
  :class (s-prefix "ext:DocumentContainer")
  :has-one `((editor-document :via ,(s-prefix "pav:hasCurrentVersion")
                              :as "current-version")
             (concept :via ,(s-prefix "ext:editorDocumentStatus")
                                     :as "status")
             (editor-document-folder :via ,(s-prefix "ext:editorDocumentFolder")
                                     :as "folder")
             (bestuurseenheid :via ,(s-prefix "dct:publisher")
                              :as "publisher"))
  :has-many `((editor-document :via ,(s-prefix "pav:hasVersion")
                               :as "revisions")
              (attachment :via ,(s-prefix "ext:hasAttachments")
                          :as "attachments")
              (editor-document :via ,(s-prefix "dct:isPartOf")
                  :as "is-part-of"))
  :resource-base (s-url "http://data.lblod.info/document-containers/")
  :features `(include-uri)
  :on-path "document-containers"
)

(define-resource editor-document ()
  :class (s-prefix "ext:EditorDocument")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:content :string ,(s-prefix "ext:editorDocumentContent"))
                (:context :string ,(s-prefix "ext:editorDocumentContext"))
                (:created-on :datetime ,(s-prefix "pav:createdOn"))
                (:updated-on :datetime ,(s-prefix "pav:lastUpdateOn"))
                (:starred :boolean ,(s-prefix "tmp:starred"))
                (:identifier :string ,(s-prefix "dct:identifier"))
                (:origin :string ,(s-prefix "pav:providedBy"))) ;;de gemeente Niel
  :has-one `((editor-document :via ,(s-prefix "pav:previousVersion")
                              :as "previous-version")
             (concept :via ,(s-prefix "dct:type")
                   :as "type")
             (concept :via ,(s-prefix "ext:editorDocumentStatus")
                      :as "status")
             (editor-document :via ,(s-prefix "pav:previousVersion")
                              :inverse t
                              :as "next-version")
             (document-container :via ,(s-prefix "pav:hasVersion")
                                 :inverse t
                                 :as "document-container"))
  :has-many `((tasklist-solution :via ,(s-prefix "ext:editorDocumentTasklistSolution")
                                 :as "tasklist-solutions")
              (document-container :via ,(s-prefix "dct:isPartOf")
                                         :inverse t
                                         :as "parts"))
  :resource-base (s-url "http://data.lblod.info/editor-documents/")
  :features `(no-pagination-defaults)
  :on-path "editor-documents"
)

(define-resource editor-document-folder ()
  :class (s-prefix "ext:EditorDocumentFolder")
  :properties `((:name :string ,(s-prefix "ext:EditorDocumentFolderName")))
  :resource-base (s-url "http://data.lblod.info/editor-document-folders/")
  :features `(no-pagination-defaults)
  :on-path "editor-document-folders"
)

(define-resource editor-document-status ()
  :class (s-prefix "ext:EditorDocumentStatus")
  :properties `((:name :string ,(s-prefix "ext:EditorDocumentStatusName")))
  :resource-base (s-url "http://data.lblod.info/editor-document-statuses/")
  :features `(no-pagination-defaults)
  :on-path "editor-document-statuses"
)

(define-resource template ()
  :class (s-prefix "ext:Template")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:description :string ,(s-prefix "dct:description"))
                (:matches :string-set ,(s-prefix "ext:templateMatches"))
                (:body :string ,(s-prefix "ext:templateContent"))
                (:contexts :uri-set ,(s-prefix "ext:activeInContext"))
                (:disabled-in-contexts :uri-set ,(s-prefix "ext:disabledInContext")))
  :resource-base (s-url "http://data.lblod.info/templates/")
  :features `(no-pagination-defaults)
  :on-path "templates"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MOBILITY MODELS ;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource verkeersbordcombinatie ()
  :class (s-prefix "lblodmow:Verkeersbordcombinatie")
  :properties `((:identifier :string ,(s-prefix "dct:identifier"))) ; identifier from irg
  :has-many `((maatregelconcept :via ,(s-prefix "dct:hasPart")
                                 :as "maatregelconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordcombinaties/")
  :on-path "verkeersbordcombinaties"
)

(define-resource maatregelconcept ()
  :class (s-prefix "lblodmow:MaatregelConcept")
  :properties `((:beschrijving :string ,(s-prefix "dct:description")))
  :has-one `((verkeersbordcombinatie :via ,(s-prefix "dct:hasPart")
                                     :inverse t
                                     :as "combinaties")
             (verkeersbordconcept :via ,(s-prefix "lblodmow:verkeersbordconcept")
                                  :as "verkeersbordconcept"))
  :resource-base (s-url "http://data.lblod.info/maatregelconcepten/")
  :on-path "maatregelconcepten"
)

(define-resource verkeersbordconcept ()
  :class (s-prefix "mobiliteit:Verkeersbordconcept")
  :properties `((:afbeelding :url ,(s-prefix "mobiliteit:grafischeWeergave"))
                (:betekenis :string ,(s-prefix "skos:scopeNote"))
                (:verkeersbordcode :string ,(s-prefix "skos:prefLabel"))
                )
  :has-one `((verkeersbordconcept-status-code :via ,(s-url "http://www.w3.org/2003/06/sw-vocab-status/ns#term_status")
                                         :as "status"))
  :has-many `((verkeersbordcategorie :via ,(s-prefix "org:classification")
                                      :as "categorie")
              (maatregelconcept :via ,(s-prefix "lblodmow:verkeersbordconcept")
                                      :inverse t
                                      :as "maatregelconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordconcepten/")
  :on-path "verkeersbordconcepten"
)

(define-resource verkeersbordcategorie ()
  :class (s-prefix "mobiliteit:Verkeersbordcategorie")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((verkeersbordconcept :via ,(s-prefix "org:classification")
                                   :inverse t
                                   :as "verkeersbordconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordcategorieen/")
  :on-path "verkeersbordcategorieen"
)

(define-resource verkeersbordconcept-status-code ()
  :class (s-prefix "lblodmow:VerkeersbordconceptStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :has-many `((verkeersbordconcept :via ,(s-url "http://www.w3.org/2003/06/sw-vocab-status/ns#term_status")
                                   :inverse t
                                   :as "verkeersbordconcepten"))
  :resource-base (s-url "http://data.lblod.info/verkeersbordconcept-status-codes/")
  :on-path "verkeersbordconcept-status-codes"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ZITTING/MEETING MODELS ;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TODO how to relate to superclass 'Agent' for heeftAanwezige
(define-resource zitting ()
  :class (s-prefix "besluit:Zitting")
  :properties `((:geplande-start :datetime ,(s-prefix "besluit:geplandeStart"))
                (:gestart-op-tijdstip :datetime ,(s-prefix "prov:startedAtTime"))
                (:geeindigd-op-tijdstip :datetime ,(s-prefix "prov:endedAtTime"))
                (:op-locatie :string ,(s-prefix "prov:atLocation"))
                (:intro :string ,(s-prefix "notulen:intro"))
                (:outro :string ,(s-prefix "notulen:outro")))

  :has-many `((mandataris :via ,(s-prefix "besluit:heeftAanwezigeBijStart")
                          :as "aanwezigen-bij-start")
              (mandataris :via ,(s-prefix "ext:heeftAfwezigeBijStart")
                          :as "afwezigen-bij-start")
              (agendapunt :via ,(s-prefix "besluit:behandelt")
                          :as "agendapunten")
              (uittreksel :via ,(s-prefix "ext:uittreksel")
                          :as "uittreksels")
              (intermission :via ,(s-prefix "ext:hasIntermission")
                      :as "intermissions")
              (agenda :via ,(s-prefix "bv:isAgendaVoor")
                      :inverse t
                      :as "publicatie-agendas"))

  :has-one `((bestuursorgaan :via ,(s-prefix "besluit:isGehoudenDoor")
                             :as "bestuursorgaan")
             (functionaris :via ,(s-prefix "besluit:heeftSecretaris")
                         :as "secretaris")
             (mandataris :via ,(s-prefix "besluit:heeftVoorzitter")
                         :as "voorzitter")
             (notulen :via ,(s-prefix "besluit:heeftNotulen")
                      :as "notulen")
             (besluitenlijst :via ,(s-prefix "ext:besluitenlijst")
                             :as "besluitenlijst")
             (publication-status-code :via , (s-prefix "bibo:status")
                                  :as "publicatie-status"))
  :resource-base (s-url "http://data.lblod.info/id/zittingen/")
  :features '(include-uri)
  :on-path "zittingen"
)

(define-resource agendapunt ()
  :class (s-prefix "besluit:Agendapunt")
  :properties `((:beschrijving :string ,(s-prefix "dct:description"))
                (:gepland-openbaar :boolean ,(s-prefix "besluit:geplandOpenbaar"))
                (:heeft-ontwerpbesluit :url ,(s-prefix "besluit:heeftOntwerpbesluit"))
                (:titel :string ,(s-prefix "dct:title"))
                (:type :uri-set ,(s-prefix "besluit:Agendapunt.type"))
                (:position :int ,(s-prefix "schema:position")))
  :has-many `((agendapunt :via ,(s-prefix "dct:references")
                          :as "referenties")
              (published-resource :via ,(s-prefix "prov:wasDerivedFrom")
                                  :as "publications"))
  :has-one `((agendapunt :via ,(s-prefix "besluit:aangebrachtNa")
                         :as "vorige-agendapunt")
             (behandeling-van-agendapunt :via ,(s-prefix "dct:subject")
                                         :inverse t 
                                         :as "behandeling")
             (zitting :via ,(s-prefix "besluit:behandelt")
                      :inverse t
                      :as "zitting")
             (agenda :via ,(s-prefix "dct:isPartOf")
                     :inverse t
                     :as "agenda"
                     )
             )
  :resource-base (s-url "http://data.lblod.info/id/agendapunten/")
  :features '(include-uri)
  :on-path "agendapunten"
)

(define-resource besluit ()
  :class (s-prefix "besluit:Besluit")
  :properties `((:beschrijving :string ,(s-prefix "eli:description"))
                (:citeeropschrift :string ,(s-prefix "eli:title_short"))
                (:motivering :string ,(s-prefix "besluit:motivering"))
                (:publicatiedatum :date ,(s-prefix "eli:date_publication"))
                (:inhoud :string ,(s-prefix "prov:value"))
                (:taal :url ,(s-prefix "eli:language"))
                (:titel :string ,(s-prefix "eli:title"))
                (:score :float ,(s-prefix "nao:score")))
  :has-one `((rechtsgrond-besluit :via ,(s-prefix "eli:realizes")
                                  :as "realisatie")
             (behandeling-van-agendapunt :via ,(s-prefix "prov:generated")
                                         :inverse t
                                         :as "volgend-uit-behandeling-van-agendapunt")
             (besluitenlijst :via ,(s-prefix "ext:besluitenlijstBesluit")
                             :inverse t
                             :as "besluitenlijst"))
  :has-many `((published-resource :via ,(s-prefix "prov:wasDerivedFrom")
                                  :as "publications"))
  :resource-base (s-url "http://data.lblod.info/id/besluiten/")
  :features '(include-uri)
  :on-path "besluiten"
)

(define-resource agenda-position ()
  :class (s-prefix "ext:AgendaPosition")
  :has-one `((agendapunt :via ,(s-prefix "dct:related")
                         :as "agendapoint")
                (concept :via ,(s-prefix "ext:location")
                         :as "position"))
  :resource-base (s-url "http://data.lblod.info/id/agenda-positions/")
  :features '(include-uri)
  :on-path "agenda-positions"
)

;;TODO how to relate to superclass 'Agent' for heeftAanwezige
(define-resource behandeling-van-agendapunt ()
  :class (s-prefix "besluit:BehandelingVanAgendapunt")
  :properties `((:openbaar :boolean ,(s-prefix "besluit:openbaar"))
                (:gevolg :string ,(s-prefix "besluit:gevolg"))
                (:afgeleid-uit :string ,(s-prefix "pav:derivedFrom"))
                (:position :int ,(s-prefix "schema:position")))
  :has-many `((besluit :via ,(s-prefix "prov:generated")
                       :as "besluiten")
              (mandataris :via ,(s-prefix "besluit:heeftAanwezige")
                          :as "aanwezigen")
              (mandataris :via ,(s-prefix "ext:heeftAfwezige")
                          :as "afwezigen")
              (stemming :via ,(s-prefix "besluit:heeftStemming")
                          :as "stemmingen"))
  :has-one `((behandeling-van-agendapunt :via ,(s-prefix "besluit:gebeurtNa")
                                         :as "vorige-behandeling-van-agendapunt")
             (agendapunt :via ,(s-prefix "dct:subject")
                              :as "onderwerp")
             (functionaris :via ,(s-prefix "besluit:heeftSecretaris")
                         :as "secretaris")
             (mandataris :via ,(s-prefix "besluit:heeftVoorzitter")
                         :as "voorzitter")
             (document-container :via , (s-prefix "ext:hasDocumentContainer")
                                  :as "document-container")
             (versioned-behandeling :via ,(s-prefix "ext:behandeling")
                                 :inverse t
                                 :as "versioned-behandeling"))
  :resource-base (s-url "http://data.lblod.info/id/behandelingen-van-agendapunten/")
  :features '(include-uri)
  :on-path "behandelingen-van-agendapunten"
)

(define-resource stemming ()
  :class (s-prefix "besluit:Stemming")
  :properties `((:position :int ,(s-prefix "schema:position"))
                (:aantal-onthouders :number ,(s-prefix "besluit:aantalOnthouders"))
                (:aantal-tegenstanders :number ,(s-prefix "besluit:aantalTegenstanders"))
                (:aantal-voorstanders :number ,(s-prefix "besluit:aantalVoorstanders"))
                (:geheim :boolean ,(s-prefix "besluit:geheim"))
                (:title :string ,(s-prefix "dct:title"))
                (:gevolg :string ,(s-prefix "besluit:gevolg"))
                (:onderwerp :string ,(s-prefix "besluit:onderwerp")))
  :has-one  `((behandeling-van-agendapunt :via ,(s-prefix "besluit:heeftStemming")
                          :inverse t
                          :as "behandeling-van-agendapunt"))
  :has-many `((mandataris :via ,(s-prefix "besluit:heeftAanwezige")
                          :as "aanwezigen")
              (mandataris :via ,(s-prefix "besluit:heeftOnthouder")
                          :as "onthouders")
              (mandataris :via ,(s-prefix "besluit:heeftStemmer")
                          :as "stemmers")
              (mandataris :via ,(s-prefix "besluit:heeftTegenstander")
                          :as "tegenstanders")
              (mandataris :via ,(s-prefix "besluit:heeftVoorstander")
                          :as "voorstanders"))
  :resource-base (s-url "http://data.lblod.info/id/stemmingen/")
  :features '(include-uri)
  :on-path "stemmingen"
)

(define-resource intermission ()
  :class (s-prefix "ext:Intermission")
  :properties `((:started-at :datetime ,(s-prefix "prov:startedAtTime"))
                (:ended-at :datetime ,(s-prefix "prov:endedAtTime"))
                (:comment :string ,(s-prefix "rdfs:comment")))
  :has-one `((zitting :via ,(s-prefix "ext:hasIntermission")
                      :inverse t
                      :as "zitting")
              (agenda-position :via ,(s-prefix "ext:agendaPosition")
                      :as "agenda-position"))
  :resource-base (s-url "http://data.lblod.info/id/intermissions/")
  :features '(include-uri)
  :on-path "intermissions"
)

(define-resource artikel ()
  :class (s-prefix "besluit:Artikel")
  :properties `((:nummer :string ,(s-prefix "eli:number"))
                (:inhoud :string ,(s-prefix "prov:value"))
                (:taal :url ,(s-prefix "eli:language"))
                (:titel :string ,(s-prefix "eli:title"))
                (:page :url ,(s-prefix "foaf:page"))
                (:score :float ,(s-prefix "nao:score")))
  :has-one `((rechtsgrond-artikel :via ,(s-prefix "eli:realizes")
                                    :as "realisatie"))
  :resource-base (s-url "http://data.lblod.info/id/artikels/")
  :features '(include-uri)
  :on-path "artikels"
)

(define-resource rechtsgrond-artikel ()
  :class (s-prefix "eli:LegalResourceSubdivision")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:inwerkingtreding :date ,(s-prefix "eli:first_date_entry_in_force")))
  :has-one `((rechtsgrond-besluit :via ,(s-prefix "eli:has_part")
                                  :inverse t
                                  :as "rechtsgrond-besluit"))
  :resource-base (s-url "http://data.lblod.info/id/rechtsgronden-artikel/")
  :features '(include-uri)
  :on-path "rechtsgronden-artikel"
)

;;TODO how to relate to superclass 'Rechtsgrond' for citeert/corrigeert/gecorrigeerd door/verandert/...
(define-resource rechtsgrond-besluit ()
  :class (s-prefix "eli:LegalResource")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:inwerkingtreding :date ,(s-prefix "eli:first_date_entry_in_force")))
  :has-many `((rechtsgrond-artikel :via ,(s-prefix "eli:has_part")
                                   :as "rechtsgronden-artikel"))
  :has-one `((bestuursorgaan :via ,(s-prefix "eli:passed_by")
                             :as "bestuursorgaan"))
  :resource-base (s-url "http://data.lblod.info/id/rechtsgronden-besluit/")
  :features '(include-uri)
  :on-path "rechtsgronden-besluit"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PUBLICATION MODELS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource signed-resource ()
  :class (s-prefix "sign:SignedResource")
  :properties `((:content :string ,(s-prefix "sign:text"))
                (:hash-value :string ,(s-prefix "sign:hashValue"))
                (:created-on :datetime ,(s-prefix "dct:created"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-one `((blockchain-status :via ,(s-prefix "sign:status")
                                :as "status")
             (agenda :via ,(s-prefix "ext:signsAgenda")
                               :as "agenda")
             (versioned-besluiten-lijst :via ,(s-prefix "ext:signsBesluitenlijst")
                                        :as "versioned-besluiten-lijst")
             (versioned-notulen :via ,(s-prefix "ext:signsNotulen")
                                :as "versioned-notulen")
             (versioned-behandeling :via ,(s-prefix "ext:signsBehandeling")
                                    :as "versioned-behandeling")
             (gebruiker :via ,(s-prefix "sign:signatory")
                        :as "gebruiker"))
  :resource-base (s-url "http://data.lblod.info/signed-resources/")
  :features '(include-uri)
  :on-path "signed-resources"
)

(define-resource published-resource ()
  :class (s-prefix "sign:PublishedResource")
  :properties `((:content :string ,(s-prefix "sign:text"))
                (:hash-value :string ,(s-prefix "sign:hashValue"))
                (:created-on :datetime ,(s-prefix "dct:created"))
                (:submission-status :uri ,(s-prefix "ext:submissionStatus")))
  :has-one `((blockchain-status :via ,(s-prefix "sign:status")
                                :as "status")
             (agenda :via ,(s-prefix "ext:publishesAgenda")
                               :as "agenda")
             (versioned-besluiten-lijst :via ,(s-prefix "ext:publishesBesluitenlijst")
                                        :as "versioned-besluiten-lijst")
             (versioned-behandeling :via ,(s-prefix "ext:publishesBehandeling")
                                        :as "versioned-behandeling")
             (versioned-notulen :via ,(s-prefix "ext:publishesNotulen")
                                :as "versioned-notulen")
             (gebruiker :via ,(s-prefix "sign:signatory")
                        :as "gebruiker"))
  :resource-base (s-url "http://data.lblod.info/published-resources/")
  :features '(include-uri)
  :on-path "published-resources"
)

(define-resource versioned-agenda ()
  :class (s-prefix "ext:VersionedAgenda")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:kind :string ,(s-prefix "ext:agendaKind"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `(
              (signed-resource :via ,(s-prefix "ext:signsAgenda")
                               :inverse t
                               :as "signed-resources")
              )
  :has-one `(
             (published-resource :via ,(s-prefix "ext:publishesAgenda")
                                 :inverse t
                                 :as "published-resource")
             (editor-document :via ,(s-prefix "prov:wasDerivedFrom")
                              :as "editor-document")
             (document-container :via ,(s-prefix "ext:hasVersionedAgenda")
                                 :inverse t
                                 :as "document-container")
             )
  :resource-base (s-url "http://data.lblod.info/prepublished-agendas/")
  :features '(include-uri)
  :on-path "versioned-agendas"
)

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
  :on-path "versioned-behandelingen"
)

(define-resource versioned-besluiten-lijst ()
  :class (s-prefix "ext:VersionedBesluitenLijst")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsBesluitenlijst")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesBesluitenlijst")
                                 :inverse t
                                 :as "published-resource")
             (editor-document :via ,(s-prefix "prov:wasDerivedFrom")
                              :as "editor-document")
             (zitting :via ,(s-prefix "besluit:heeftBesluitenlijst")
                                 :inverse t
                                 :as "zitting"))
  :resource-base (s-url "http://data.lblod.info/prepublished-besluiten-lijsten/")
  :features '(include-uri)
  :on-path "versioned-besluiten-lijsten"
)

(define-resource versioned-notulen ()
  :class (s-prefix "ext:VersionedNotulen")
  :properties `((:state :string ,(s-prefix "ext:stateString"))
                (:content :string ,(s-prefix "ext:content"))
                (:public-content :string ,(s-prefix "ext:publicContent"))
                (:public-behandelingen :uri-set ,(s-prefix "ext:publicBehandeling"))
                (:kind :string ,(s-prefix "ext:notulenKind"))
                (:deleted :boolean ,(s-prefix "ext:deleted")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsNotulen")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesNotulen")
                                 :inverse t
                                 :as "published-resource")
             (editor-document :via ,(s-prefix "prov:wasDerivedFrom")
                              :as "editor-document")
             (zitting :via ,(s-prefix "ext:hasVersionedNotulen")
                                 :inverse t
                                 :as "zitting"))
  :resource-base (s-url "http://data.lblod.info/prepublished-notulen/")
  :features '(include-uri)
  :on-path "versioned-notulen"
)

(define-resource versioned-regulatory-statement ()
  :class (s-prefix "ext:VersionedRegulatoryStatement")
  :properties `((:state :string ,(s-prefix "ext:stateString")))
  :has-many `((signed-resource :via ,(s-prefix "ext:signsRegulatoryStatement")
                               :inverse t
                               :as "signed-resources"))
  :has-one `((published-resource :via ,(s-prefix "ext:publishesRegulatoryStatement")
                                 :inverse t
                                 :as "published-resource")
             (versioned-behandeling :via ,(s-prefix "ext:hasVersionedRegulatoryStatement")
                                 :inverse t
                                 :as "versioned-behandeling")
             (editor-document :via ,(s-prefix "ext:regulatoryStatement")
                                         :as "regulatory-statement")
             (file :via ,(s-prefix "prov:generated")
                            :as "file"))
  :resource-base (s-url "http://data.lblod.info/prepublished-regulatory-statement/")
  :features '(include-uri)
  :on-path "versioned-regulatory-statements"
)

(define-resource agenda ()
  :class (s-prefix "bv:Agenda")
  :properties `(
                (:inhoud :string ,(s-prefix "prov:value"))
                (:agenda-status :string ,(s-prefix "bv:agendaStatus"))
                (:agenda-type :string ,(s-prefix "bv:agendaType"))
                (:rendered-content :string ,(s-prefix "ext:renderedContent"))
                (:deleted :boolean ,(s-prefix "ext:deleted"))
                )
  :has-one `(
             (zitting :via ,(s-prefix "bv:isAgendaVoor")
                      :as "zitting")
             (published-resource :via ,(s-prefix "ext:publishesAgenda")
                                 :inverse t
                                 :as "published-resource")
             )
  :has-many `(
              (agendapunt :via ,(s-prefix "dct:isPartOf")
                          :inverse t
                          :as "agendapunten")
              (signed-resource :via ,(s-prefix "ext:signsAgenda")
                               :inverse t
                               :as "signed-resources")
              )
  :resource-base (s-url "http://data.lblod.info/id/agendas/")
  :features '(include-uri)
  :on-path "agendas"
)

(define-resource besluitenlijst ()
  :class (s-prefix "ext:Besluitenlijst")
  :properties `((:inhoud :string ,(s-prefix "prov:value"))
                (:publicatiedatum :date ,(s-prefix "eli:date_publication")))
  :has-one `((published-resource :via ,(s-prefix "prov:wasDerivedFrom")
                                 :as "publication")
             (zitting :via ,(s-prefix "ext:besluitenlijst")
                      :inverse t
                      :as "zitting"))
  :has-many `((besluit :via ,(s-prefix "ext:besluitenlijstBesluit")
                                          :as "besluiten"))
  :resource-base (s-url "http://data.lblod.info/id/besluitenlijsten/")
  :features '(include-uri)
  :on-path "besluitenlijsten"
)

(define-resource uittreksel ()
  :class (s-prefix "ext:Uittreksel")
  :properties `((:inhoud :string ,(s-prefix "prov:value")))
  :has-one `((published-resource :via ,(s-prefix "prov:wasDerivedFrom")
                                 :as "publication")
             (behandeling-van-agendapunt :via ,(s-prefix "ext:uittrekselBvap")
                                         :as "behandeling-van-agendapunt")
             (zitting :via ,(s-prefix "ext:uittreksel")
                      :inverse t
                      :as "zitting"))
  :resource-base (s-url "http://data.lblod.info/id/uittreksels/")
  :features '(include-uri)
  :on-path "uittreksels"
)

(define-resource notulen ()
  :class (s-prefix "ext:Notulen")
  :properties `((:inhoud :string ,(s-prefix "prov:value")))
  :has-one `((zitting :via ,(s-prefix "besluit:heeftNotulen")
                      :inverse t
                      :as "zitting"))
  :has-many `((published-resource :via ,(s-prefix "prov:wasDerivedFrom")
                                  :as "publications"))
  :resource-base (s-url "http://data.lblod.info/id/notulen/")
  :features '(include-uri)
  :on-path "notulen"
)

(define-resource publishing-log ()
  :class (s-prefix "ext:PublishingLog")
  :properties `((:action :string ,(s-prefix "ext:publishingAction"))
              (:date :datetime ,(s-prefix "pav:createdOn")))
  :has-one `((published-resource :via ,(s-prefix "ext:hasPublishedResource")
                               :as "published-resource")
             (signed-resource :via ,(s-prefix "ext:hasSignedResource")
                               :as "signed-resource")
              (gebruiker :via ,(s-prefix "ext:responsibleUser")
                                         :as "user")
              (zitting :via ,(s-prefix "ext:hasPublishingLog")
                                         :inverse t
                                         :as "zitting"))
  :resource-base (s-url "http://data.lblod.info/publishing-logs/")
  :features `(include-uri)
  :on-path "publishing-logs"
)

(define-resource publication-status-code ()
  :class (s-prefix "bibo:DocumentStatus")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/MandatarisStatusCode/")
  :features '(include-uri)
  :on-path "publication-status-codes"
)

(define-resource blockchain-status ()
  :class (s-prefix "sign:BlockchainStatus")
  :properties `((:title :string ,(s-prefix "dct:title"))
                (:description :string ,(s-prefix "dct:description")))
  :resource-base (s-url "http://data.lblod.info/blockchain-statuses/")
  :features '(include-uri)
  :on-path "blockchain-statuses"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BESTUUR/ADMINISTRATION MODELS ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource bestuurseenheid ()
  :class (s-prefix "besluit:Bestuurseenheid")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel"))
                (:alternatieve-naam :string-set ,(s-prefix "skos:altLabel"))
                (:wil-mail-ontvangen :boolean ,(s-prefix "ext:wilMailOntvangen")) ;;Voorkeur in berichtencentrum
                (:mail-adres :string ,(s-prefix "ext:mailAdresVoorNotificaties")))
  :has-one `((werkingsgebied :via ,(s-prefix "besluit:werkingsgebied")
                             :as "werkingsgebied")
             (werkingsgebied :via ,(s-prefix "ext:inProvincie")
                             :as "provincie")
             (bestuurseenheid-classificatie-code :via ,(s-prefix "besluit:classificatie")
                                                 :as "classificatie"))
  :has-many `((contact-punt :via ,(s-prefix "schema:contactPoint")
                            :as "contactinfo")
              (bestuursorgaan :via ,(s-prefix "besluit:bestuurt")
                              :inverse t
                              :as "bestuursorganen"))
  :resource-base (s-url "http://data.lblod.info/id/bestuurseenheden/")
  :features '(include-uri)
  :on-path "bestuurseenheden"
)

(define-resource bestuurseenheid-classificatie-code ()
  :class (s-prefix "ext:BestuurseenheidClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuurseenheidClassificatieCode/")
  :features '(include-uri)
  :on-path "bestuurseenheid-classificatie-codes"
)

(define-resource werkingsgebied ()
  :class (s-prefix "prov:Location")
  :properties `((:naam :string ,(s-prefix "rdfs:label"))
                (:niveau :string, (s-prefix "ext:werkingsgebiedNiveau")))

  :has-many `((bestuurseenheid :via ,(s-prefix "besluit:werkingsgebied")
                               :inverse t
                               :as "bestuurseenheid"))
  :resource-base (s-url "http://data.lblod.info/id/werkingsgebieden/")
  :features '(include-uri)
  :on-path "werkingsgebieden"
)

(define-resource bestuursorgaan ()
  :class (s-prefix "besluit:Bestuursorgaan")
  :properties `((:naam :string ,(s-prefix "skos:prefLabel"))
                (:binding-einde :date ,(s-prefix "mandaat:bindingEinde"))
                (:binding-start :date ,(s-prefix "mandaat:bindingStart")))
  :has-one `((bestuurseenheid :via ,(s-prefix "besluit:bestuurt")
                              :as "bestuurseenheid")
             (bestuursorgaan-classificatie-code :via ,(s-prefix "besluit:classificatie")
                                                :as "classificatie")
             (bestuursorgaan :via ,(s-prefix "mandaat:isTijdspecialisatieVan")
                             :as "is-tijdsspecialisatie-van")
             (rechtstreekse-verkiezing :via ,(s-prefix "mandaat:steltSamen")
                                      :inverse t
                                      :as "wordt-samengesteld-door"))
  :has-many `((bestuursorgaan :via ,(s-prefix "mandaat:isTijdspecialisatieVan")
                       :inverse t
                       :as "heeft-tijdsspecialisaties")
              (mandaat :via ,(s-prefix "org:hasPost")
                       :as "bevat")
              (bestuursfunctie :via ,(s-prefix "lblodlg:heeftBestuursfunctie")
                               :as "bevat-bestuursfunctie"))
  :resource-base (s-url "http://data.lblod.info/id/bestuursorganen/")
  :features '(include-uri)
  :on-path "bestuursorganen"
)

(define-resource bestuursorgaan-classificatie-code ()
  :class (s-prefix "ext:BestuursorgaanClassificatieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :has-many `((bestuursfunctie-code :via ,(s-prefix "ext:hasDefaultType")
                                    :as "standaard-type")
              (bestuursorgaan :via ,(s-prefix "besluit:classificatie")
                              :inverse t
                              :as "is-classificatie-van"))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuursorgaanClassificatieCode/")
  :features '(include-uri)
  :on-path "bestuursorgaan-classificatie-codes"
)

(define-resource bestuursfunctie ()
  :class (s-prefix "lblodlg:Bestuursfunctie")
  :has-one `((bestuursfunctie-code :via ,(s-prefix "org:role")
                                   :as "rol")
             (contact-punt :via ,(s-prefix "schema:contactPoint")
                           :as "contactinfo"))
  :has-many `((bestuursorgaan :via ,(s-prefix "lblodlg:heeftBestuursfunctie")
                              :inverse t
                              :as "bevat-in"))
  :resource-base (s-url "http://data.lblod.info/id/bestuursfuncties/")
  :features '(include-uri)
  :on-path "bestuursfuncties"
)

(define-resource bestuursfunctie-code ()
  :class (s-prefix "ext:BestuursfunctieCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :has-many `((bestuursorgaan-classificatie-code :via ,(s-prefix "ext:hasDefaultType")
                                                 :inverse t
                                                 :as "standaard-type-van"))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BestuursfunctieCode/")
  :features '(include-uri)
  :on-path "bestuursfunctie-codes"
)

(define-resource fractie ()
  :class (s-prefix "mandaat:Fractie")
  :properties `((:naam :string ,(s-prefix "regorg:legalName"))
                (:generated-from :uri-set ,(s-prefix "ext:generatedFrom"))) ;;if it e.g. comes from gelinkt-notuleren
  :resource-base (s-url "http://data.lblod.info/id/fracties/")
  :has-many `((bestuursorgaan :via ,(s-prefix "org:memberOf")
                              :as "bestuursorganen-in-tijd"))
  :has-one `((bestuurseenheid :via ,(s-prefix "org:linkedTo")
                              :as "bestuurseenheid")
             (fractietype :via ,(s-prefix "ext:isFractietype")
                          :as "fractietype"))
  :features '(include-uri)
  :on-path "fracties"
)

(define-resource fractietype ()
  :class (s-prefix "ext:Fractietype")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/Fractietype/")
  :features '(include-uri)
  :on-path "fractietypes"
)

(define-resource mandaat ()
  :class (s-prefix "mandaat:Mandaat")
  :properties `((:aantal-houders :number ,(s-prefix "mandaat:aantalHouders")))
  :has-one `((bestuursfunctie-code :via ,(s-prefix "org:role")
                                   :as "bestuursfunctie"))
  :has-many `((bestuursorgaan :via ,(s-prefix "org:hasPost")
                              :inverse t
                              :as "bevat-in"))
  :resource-base (s-url "http://data.lblod.info/id/mandaten/")
  :features '(include-uri)
  :on-path "mandaten"
)

(define-resource functionaris ()
  :class (s-prefix "lblodlg:Functionaris")
  :properties `((:start :datetime ,(s-prefix "mandaat:start"))
                (:einde :datetime ,(s-prefix "mandaat:einde")))
  :has-one `((bestuursfunctie :via ,(s-prefix "org:holds")
                              :as "bekleedt")
             (functionaris-status-code :via ,(s-prefix "mandaat:status")
                                       :as "status")
             (persoon :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                      :as "is-bestuurlijke-alias-van"))
  :resource-base (s-url "http://data.lblod.info/id/functionarissen/")
  :features '(include-uri)
  :on-path "functionarissen"
)

(define-resource functionaris-status-code ()
  :class (s-prefix "lblodlg:FunctionarisStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/functionarisStatusCode/")
  :features '(include-uri)
  :on-path "functionaris-status-codes"
)

(define-resource mandataris ()
  :class (s-prefix "mandaat:Mandataris")
  :properties `((:rangorde :language-string ,(s-prefix "mandaat:rangorde"))
                (:start :datetime ,(s-prefix "mandaat:start"))
                (:einde :datetime ,(s-prefix "mandaat:einde"))
                (:datum-eedaflegging :datetime ,(s-prefix "ext:datumEedaflegging"))
                (:datum-ministrieel-besluit :datetime ,(s-prefix "ext:datumMinistrieelBesluit"))
                (:generated-from :uri-set ,(s-prefix "ext:generatedFrom"))) ;;if it e.g. comes from gelinkt-notuleren
  :has-many `((rechtsgrond-aanstelling :via ,(s-prefix "mandaat:isAangesteldDoor")
                                       :as "rechtsgronden-aanstelling")
              (rechtsgrond-beeindiging :via ,(s-prefix "mandaat:isOntslagenDoor")
                                       :as "rechtsgronden-beeindiging")
              (mandataris :via ,(s-prefix "mandaat:isTijdelijkVervangenDoor")
                          :as "tijdelijke-vervangingen")
              (beleidsdomein-code :via ,(s-prefix "mandaat:beleidsdomein")
                                  :as "beleidsdomein")
              (behandeling-van-agendapunt :via ,(s-prefix "besluit:heeftAanwezige")
                           :inverse t
                           :as "aanwezig-bij-behandeling")
              (behandeling-van-agendapunt :via ,(s-prefix "ext:heeftAfwezige")
                           :inverse t
                           :as "afwezig-bij-behandeling")
              (zitting :via ,(s-prefix "besluit:heeftAanwezigeBijStart")
                           :inverse t
                           :as "aanwezig-bij-zitting")
              (zitting :via ,(s-prefix "ext:heeftAfwezigeBijStart")
                           :inverse t
                           :as "afwezig-bij-zitting"))
  :has-one `((mandaat :via ,(s-prefix "org:holds")
                      :as "bekleedt")
             (lidmaatschap :via ,(s-prefix "org:hasMembership")
                           :as "heeft-lidmaatschap")
             (persoon :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                      :as "is-bestuurlijke-alias-van")
             (mandataris-status-code :via ,(s-prefix "mandaat:status")
                                      :as "status"))
  :resource-base (s-url "http://data.lblod.info/id/mandatarissen/")
  :features '(include-uri)
  :on-path "mandatarissen"
)

(define-resource mandataris-status-code ()
  :class (s-prefix "ext:MandatarisStatusCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/MandatarisStatusCode/")
  :features '(include-uri)
  :on-path "mandataris-status-codes"
)

(define-resource beleidsdomein-code ()
  :class (s-prefix "ext:BeleidsdomeinCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :has-many `((mandataris :via ,(s-prefix "mandaat:beleidsdomein")
                          :inverse t
                          :as "mandatarissen"))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/BeleidsdomeinCode/")
  :features '(include-uri)
  :on-path "beleidsdomein-codes"
)

(define-resource lidmaatschap ()
  :class (s-prefix "org:Membership")
  :has-one `((fractie :via ,(s-prefix "org:organisation")
                      :as "binnen-fractie")
             (mandataris :via ,(s-prefix "org:hasMembership")
                         :inverse t
                         :as "lid")
             (tijdsinterval :via ,(s-prefix "org:memberDuring")
                            :as "lid-gedurende"))
  :resource-base (s-url "http://data.lblod.info/id/lidmaatschappen/")
  :features '(include-uri)
  :on-path "lidmaatschappen"
)

(define-resource tijdsinterval ()
  :class (s-prefix "dct:PeriodOfTime")
  :properties `((:begin :datetime ,(s-prefix "generiek:begin"))
                (:einde :datetime ,(s-prefix "generiek:einde")))
  :resource-base (s-url "http://data.lblod.info/id/tijdsintervallen/")
  :features '(include-uri)
  :on-path "tijdsintervallen"
)

(define-resource rechtsgrond-aanstelling ()
  :class (s-prefix "mandaat:RechtsgrondAanstelling")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:inwekingtreding :date ,(s-prefix "eli:first_date_entry_in_force"))
                (:type-document :uri-set ,(s-prefix "eli:type_document")))
  :has-many `((mandataris :via ,(s-prefix "mandaat:isAangesteldDoor")
                          :inverse t
                          :as "bekrachtigt-aanstellingen-van"))
  :resource-base (s-url "http://data.ldblod.info/id/rechtsgronden-aanstelling/")
  :features '(include-uri)
  :on-path "rechtsgronden-aanstelling"
)

(define-resource rechtsgrond-beeindiging ()
  :class (s-prefix "mandaat:RechtsgrondBeeindiging")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
              (:inwekingtreding :date ,(s-prefix "eli:first_date_entry_in_force"))
              (:type-document :uri-set ,(s-prefix "eli:type_document")))
  :has-many `((mandataris :via ,(s-prefix "mandaat:isOntslagenDoor")
                          :inverse t
                          :as "bekrachtigt-ontslagen-van"))
  :resource-base (s-url "http://data.lblod.info/id/rechtsgronden-beeindiging/")
  :features '(include-uri)
  :on-path "rechtsgronden-beeindiging"
)

(define-resource rechtsgrond ()
  :class (s-prefix "eli:LegalResource")
  :properties `((:buitenwerkingtreding :date ,(s-prefix "eli:date_no_longer_in_force"))
                (:inwekingtreding :date ,(s-prefix "eli:first_date_entry_in_force"))
                (:type-document :uri-set ,(s-prefix "eli:type_document"))) ;;TODO: what about predefined lists?
  :resource-base (s-url "https://data.lblod.info/id/rechtsgronden/")
  :features '(include-uri)
  :on-path "rechtsgronden"
)

(define-resource rechtstreekse-verkiezing ()
  :class (s-prefix "mandaat:RechtstreekseVerkiezing")
  :properties `((:datum :date ,(s-prefix "mandaat:datum"))
                (:geldigheid :date ,(s-prefix "dct:valid")))
  :has-one `((bestuursorgaan :via ,(s-prefix "mandaat:steltSamen")
                             :as "stelt-samen"))
  :has-many `((kandidatenlijst :via ,(s-prefix "mandaat:behoortTot")
                               :inverse t
                               :as "heeft-lijst"))
  :resource-base (s-url "http://data.lblod.info/id/rechtstreekse-verkiezingen/")
  :features '(include-uri)
  :on-path "rechtstreekse-verkiezingen"
)

(define-resource kandidatenlijst ()
  :class (s-prefix "mandaat:Kandidatenlijst")
  :properties `((:lijstnaam :string ,(s-prefix "skos:prefLabel"))
                (:lijstnummer :number ,(s-prefix "mandaat:lijstnummer")))
  :has-many `((persoon :via ,(s-prefix "mandaat:heeftKandidaat")
                       :as "kandidaten"))
  :has-one `((lijsttype :via ,(s-prefix "mandaat:lijsttype")
                        :as "lijsttype")
             (rechtstreekse-verkiezing :via ,(s-prefix "mandaat:behoortTot")
                                       :as "rechtstreekse-verkiezing"))
  :resource-base (s-url "http://data.lblod.info/id/kandidatenlijsten/")
  :features '(include-uri)
  :on-path "kandidatenlijsten"
)

(define-resource lijsttype ()
  :class (s-prefix "ext:KandidatenlijstLijsttype")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/KandidatenlijstLijsttype/")
  :features '(include-uri)
  :on-path "lijsttypes"
)

(define-resource verkiezingsresultaat ()
  :class (s-prefix "mandaat:Verkiezingsresultaat")
  :properties `((:aantal-naamstemmen :number ,(s-prefix "mandaat:aantalNaamstemmen"))
                (:plaats-rangorde :number ,(s-prefix "mandaat:plaatsRangorde")))
  :has-one `((persoon :via ,(s-prefix "mandaat:isResultaatVan")
                      :as "is-resultaat-van")
             (kandidatenlijst :via ,(s-prefix "mandaat:isResultaatVoor")
                              :as "is-resultaat-voor")
             (verkiezingsresultaat-gevolg-code :via ,(s-prefix "mandaat:gevolg")
                                               :as "gevolg"))
  :resource-base (s-url "http://data.lblod.info/id/verkiezingsresultaten/")
  :features '(include-uri)
  :on-path "verkiezingsresultaten"
)

(define-resource verkiezingsresultaat-gevolg-code ()
  :class (s-prefix "ext:VerkiezingsresultaatGevolgCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/VerkiezingsresultaatGevolgCode/")
  :features '(include-uri)
  :on-path "verkiezingsresultaat-gevolg-codes"
)

;;TODO: is this model still in use/relevant?
(define-resource rol ()
  :class (s-prefix "org:Role")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/concept/functionarisRol/")
  :features '(include-uri)
  :on-path "rollen"
)

;;TODO: is this model still in use/relevant?
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONTACT MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource contact-punt ()
  :class (s-prefix "schema:ContactPoint")
  :properties `((:aanschrijfprefix :language-string-set ,(s-prefix "vcard:honorific-prefix"))
                (:email :string ,(s-prefix "schema:email"))
                (:fax :string ,(s-prefix "schema:faxNumber"))
                (:naam :string ,(s-prefix "foaf:name"))
                (:website :url ,(s-prefix "foaf:page"))
                (:telefoon :string ,(s-prefix "schema:telephone")))
  :has-one `((adres :via ,(s-prefix "locn:address")
                    :as "adres"))
  :features '(include-uri)
  :resource-base (s-url "http://data.lblod.info/id/contact-punten/")
  :on-path "contact-punten"
)

(define-resource adres ()
  :class (s-prefix "locn:Address")
  :properties `((:busnummer :string ,(s-prefix "adres:Adresvoorstelling.busnummer"))
                (:huisnummer :string ,(s-prefix "adres:AdresVoorstelling.huisnummer"))
                (:straatnaam :string ,(s-prefix "locn:thoroughfare"))
                (:postcode :string ,(s-prefix "locn:postCode"))
                (:gemeentenaam :string ,(s-prefix "adres:gemeentenaam"))
                (:land :language-string-set ,(s-prefix "adres:land"))
                (:locatieaanduiding :string ,(s-prefix "locn:locatorDesignator"))
                (:locatienaam :language-string-set ,(s-prefix "locn:locatorName"))
                (:postbus :string ,(s-prefix "locn:poBox"))
                (:postnaam :string ,(s-prefix "locn:postName"))
                (:volledig-adres :string ,(s-prefix "locn:fullAddress"))
                (:adres-register-id :number ,(s-prefix "lblodlg:adresRegisterId"))
                (:adres-register-uri :url ,(s-prefix "adres:verwijstNaar")))
  :features '(include-uri)
  :resource-base (s-url "http://data.lblod.info/id/adressen/")
  :on-path "adressen"
)

;;TODO: is this model still in use/relevant?
(define-resource vestiging ()
  :class (s-prefix "org:Site")
  :has-one `((contact-punt :via ,(s-prefix "schema:contactPoint")
                           :as "vestigingsadres"))
  :resource-base (s-url "http://data.lblod.info/id/vestiging/")
  :features '(include-uri)
  :on-path "vestigingen"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSON-RELATED DATA MODELS ;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource persoon ()
  :class (s-prefix "person:Person")
  :properties `((:achternaam :string ,(s-prefix "foaf:familyName"))
                (:alternatieve-naam :string ,(s-prefix "foaf:name"))
                (:gebruikte-voornaam :string ,(s-prefix "persoon:gebruikteVoornaam")))
  :has-many `((mandataris :via ,(s-prefix "mandaat:isBestuurlijkeAliasVan")
                          :inverse t
                          :as "is-aangesteld-als")
              (kandidatenlijst :via ,(s-prefix "mandaat:heeftKandidaat")
                               :inverse t
                               :as "is-kandidaat-voor")
              (verkiezingsresultaat :via ,(s-prefix "mandaat:isResultaatVan")
                        :inverse t
                        :as "verkiezingsresultaten"))
  :has-one `((geboorte :via ,(s-prefix "persoon:heeftGeboorte")
                       :as "geboorte")
             (identificator :via ,(s-prefix "adms:identifier")
                            :as "identificator")
             (geslacht-code :via ,(s-prefix "persoon:geslacht")
                            :as "geslacht"))
  :resource-base (s-url "http://data.lblod.info/id/personen/")
  :features '(include-uri)
  :on-path "personen"
)

(define-resource geslacht-code ()
  :class (s-prefix "ext:GeslachtCode")
  :properties `((:label :string ,(s-prefix "skos:prefLabel"))
                (:scope-note :string ,(s-prefix "skos:scopeNote")))
  :resource-base (s-url "http://data.vlaanderen.be/id/concept/GeslachtCode/")
  :features '(include-uri)
  :on-path "geslacht-codes"
)

(define-resource geboorte ()
  :class (s-prefix "persoon:Geboorte")
  :properties `((:datum :date ,(s-prefix "persoon:datum")))
  :resource-base (s-url "http://data.lblod.info/id/geboortes/")
  :features '(include-uri)
  :on-path "geboortes"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPORT MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource report ()
  :class (s-url "http://lblod.data.gift/vocabularies/reporting/Report")
  :properties `((:title :string ,(s-prefix "dct:title"))
    (:description :string ,(s-prefix "dct:description"))
    (:created :string ,(s-prefix "dct:created"))
  )
  :has-one `((file :via ,(s-prefix "prov:generated ")
                    :as "file"))
  :resource-base (s-url "http://data.lblod.info/id/reports/")
  :features '(include-uri)
  :on-path "reports"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JOBS MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource job ()
  :class (s-prefix "cogs:Job")
  :properties `((:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:creator :url ,(s-prefix "dct:creator")) ;;Later consider using proper relation in domain.lisp
                (:status :url ,(s-prefix "adms:status")) ;;Later consider using proper relation in domain.lisp
                (:operation :url ,(s-prefix "task:operation"))) ;;Later consider using proper relation in domain.lisp

  :has-one `((job-error :via ,(s-prefix "task:error")
                        :as "error"))

  :has-many `((task :via ,(s-prefix "dct:isPartOf")
                    :inverse t
                    :as "tasks"))

  :resource-base (s-url "http://redpencil.data.gift/id/job/")
  :features '(include-uri)
  :on-path "jobs"
)

(define-resource job-error ()
  :class (s-prefix "oslc:Error")
  :properties `((:message :string ,(s-prefix "oslc:message")))
  :resource-base (s-url "http://redpencil.data.gift/id/jobs/error/")
  :features '(include-uri)
  :on-path "job-errors"
)

(define-resource task ()
  :class (s-prefix "task:Task")
  :properties `((:created :datetime ,(s-prefix "dct:created"))
                (:modified :datetime ,(s-prefix "dct:modified"))
                (:status :url ,(s-prefix "adms:status")) ;;Later consider using proper relation in domain.lisp
                (:operation :url ,(s-prefix "task:operation")) ;;Later consider using proper relation in domain.lisp
                (:index :string ,(s-prefix "task:index")))

  :has-one `((job-error :via ,(s-prefix "task:error")
                    :as "error")
             (job :via ,(s-prefix "dct:isPartOf")
                    :as "job"))

  :has-many `((task :via ,(s-prefix "cogs:dependsOn")
                    :as "parent-tasks")
              (data-container :via ,(s-prefix "task:resultsContainer")
                    :as "results-containers")
              (data-container :via ,(s-prefix "task:inputContainer")
                    :as "input-containers")
              )

  :resource-base (s-url "http://redpencil.data.gift/id/task/")
  :features '(include-uri)
  :on-path "tasks"
)

(define-resource data-container ()
  :class (s-prefix "nfo:DataContainer")
  :properties `((:has-graph :url ,(s-prefix "task:hasGraph")))
  :has-many `((file :via ,(s-prefix "task:hasFile") ;;subProperty of dct:hasPart because mu-resource does not like the same predicate linked to multiple types
                    :as "files")
              (harvesting-collection :via ,(s-prefix "task:hasHarvestingCollection")
                                     :as "harvesting-collections")
              (task :via ,(s-prefix "task:resultsContainer")
                    :inverse t
                    :as "result-from-tasks")
              (task :via ,(s-prefix "task:inputContainer")
                    :inverse t
                    :as "input-from-tasks")
              )
  :resource-base (s-url "http://redpencil.data.gift/id/dataContainers/")
  :features '(include-uri)
  :on-path "data-containers"
)

;;TODO: is this model still in use/relevant?
(define-resource tasklist-solution ()
  :class (s-prefix "ext:TasklistSolution")

  :properties `((:name :string ,(s-prefix "ext:tasklistSolutionName"))
                (:index :number ,(s-prefix "ext:tasklistSolutionIndex")))

  :has-one `((tasklist :via ,(s-prefix "ext:tasklistSolutionTasklist")
                        :as "tasklist"))

  :has-many `((task-solution :via ,(s-prefix "ext:tasklistSolutionTaskSolution")
                   :as "task-solutions"))

  :resource-base (s-url "http://data.lblod.info/id/tasklist-solutions/")
  :features '(include-uri)
  :on-path "tasklist-solutions"
)

;;TODO: is this model still in use/relevant?
(define-resource tasklist ()
  :class (s-prefix "ext:Tasklist")

  :properties `((:name :string ,(s-prefix "ext:tasklistName")))

  :has-many `((task :via ,(s-prefix "ext:tasklistTask")
                   :as "tasks"))

  :resource-base (s-url "http://data.lblod.info/id/tasklists/")
  :features '(include-uri)
  :on-path "tasklists"
)

;;TODO: is this model still in use/relevant?
(define-resource task-solution ()
  :class (s-prefix "ext:TaskSolution")
  :properties `((:status :bool ,(s-prefix "ext:taskSolutionStatus")))

  :has-one `((task :via ,(s-prefix "ext:taskSolutionTask")
                   :as "task"))

  :resource-base (s-url "http://data.lblod.info/id/task-solutions/")
  :features '(include-uri)
  :on-path "task-solutions"
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOG MODELS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-resource log-entry ()
  :class (s-prefix "rlog:Entry")
  :properties `((:class-name :url ,(s-prefix "rlog:className"))
                (:message :string ,(s-prefix "rlog:message"))
                (:specific-information :string ,(s-prefix "ext:specificInformation"))
                (:datetime :datetime ,(s-prefix "rlog:date")))
  :has-one `((log-level :via ,(s-prefix "rlog:level")
                   :as "log-level")
              (status-code :via ,(s-prefix "rlog:hasCode")
                   :as "status-code")
              (log-source :via ,(s-prefix "dct:source")
                   :as "log-source"))
  :has-many `((report-content :via ,(s-prefix "ext:belongsToReport")
                   :as "report-content"))
  :resource-base (s-url "http://data.lblod.info/id/log-entries/")
  :features `(include-uri)
  :on-path "log-entries"
)

(define-resource log-level ()
  :class (s-prefix "rlog:Level")
  :properties `((:priority :number ,(s-prefix "rlog:priority"))
                (:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/log-levels/")
  :features `(include-uri)
  :on-path "log-levels"
)

(define-resource log-source ()
  :class (s-prefix "ext:LogSource")
  :properties `((:label :string ,(s-prefix "skos:prefLabel")))
  :resource-base (s-url "http://data.lblod.info/id/log-sources/")
  :features `(include-uri)
  :on-path "log-sources"
)

(define-resource status-code ()
  :class (s-prefix "rlog:StatusCode")
  :properties `((:code :number ,(s-prefix "rlog:codeId")))
  :resource-base (s-url "http://data.lblod.info/id/status-codes/")
  :features `(include-uri)
  :on-path "status-codes"
)