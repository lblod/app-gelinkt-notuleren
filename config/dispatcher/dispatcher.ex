defmodule Dispatcher do
  use Matcher
  define_accept_types []

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   forward conn, path, "http://resource/themes/"
  # end
  #
  get "/sync/files/*path" do
    forward conn, path, "http://published-resource-producer/files/"
  end
  match "/files/*path" do
    forward conn, path, "http://file/files/"
  end

  match "/blockchain/*path" do
    forward conn, path, "http://blockchain/"
  end

  match "/codex/sparql/*path" do
    forward conn, path, "http://codex-proxy/sparql/"
  end

  match "/decisionservice/*path" do
    forward conn, path, "http://decisionservice/"
  end

  match "/agendas/*path" do
    forward conn, path, "http://resource/agendas/"
  end

  match "/besluitenlijsten/*path" do
    forward conn, path, "http://resource/besluitenlijsten/"
  end

  match "/uittreksels/*path" do
    forward conn, path, "http://resource/uittreksels/"
  end

  match "/prepublished-agendas/*path" do
    forward conn, path, "http://resource/prepublished-agendas/"
  end

  match "/agendapunten/*path" do
    forward conn, path, "http://cache/agendapunten/"
  end

  match "/behandelingen-van-agendapunten/*path" do
    forward conn, path, "http://cache/behandelingen-van-agendapunten/"
  end

  match "/concepts/*path" do
    forward conn, path, "http://cache/concepts/"
  end

  match "/concept-schemes/*path" do
    forward conn, path, "http://cache/concept-schemes/"
  end

  match "/bestuurseenheden/*path" do
    forward conn, path, "http://cache/bestuurseenheden/"
  end

  match "/werkingsgebieden/*path" do
    forward conn, path, "http://cache/werkingsgebieden/"
  end

  match "/bestuurseenheid-classificatie-codes/*path" do
    forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end

  match "/bestuursorganen/*path" do
    forward conn, path, "http://cache/bestuursorganen/"
  end

  match "/bestuursorgaan-classificatie-codes/*path" do
    forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end

  match "/stemmingen/*path" do
    forward conn, path, "http://cache/stemmingen/"
  end

  match "/zittingen/*path" do
    forward conn, path, "http://cache/zittingen/"
  end

  match "/notulen/*path" do
    forward conn, path, "http://resource/notulen/"
  end

  match "/fracties/*path" do
    forward conn, path, "http://resource/fracties/"
  end

  match "/fractietypes/*path" do
    forward conn, path, "http://cache/fractietypes/"
  end

  match "/geboortes/*path" do
    forward conn, path, "http://cache/geboortes/"
  end

  match "/lijsttypes/*path" do
    forward conn, path, "http://cache/lijsttypes/"
  end

  match "/kandidatenlijsten/*path" do
    forward conn, path, "http://cache/kandidatenlijsten/"
  end

  match "/lidmaatschappen/*path" do
    forward conn, path, "http://resource/lidmaatschappen/"
  end

  match "/mandaten/*path" do
    forward conn, path, "http://resource/mandaten/"
  end

  match "/bestuursfunctie-codes/*path" do
    forward conn, path, "http://cache/bestuursfunctie-codes/"
  end

  match "/mandatarissen/*path" do
    forward conn, path, "http://cache/mandatarissen/"
  end

  match "/mandataris-status-codes/*path" do
    forward conn, path, "http://cache/mandataris-status-codes/"
  end

  match "/publication-status-codes/*path" do
    forward conn, path, "http://cache/publication-status-codes/"
  end

  match "/beleidsdomein-codes/*path" do
    forward conn, path, "http://cache/beleidsdomein-codes/"
  end

  match "/personen/*path" do
    forward conn, path, "http://cache/personen/"
  end

  match "/geslacht-codes/*path" do
    forward conn, path, "http://cache/geslacht-codes/"
  end

  match "/identificatoren/*path" do
    forward conn, path, "http://resource/identificatoren/"
  end

  match "/rechtsgronden-aanstelling/*path" do
    forward conn, path, "http://resource/rechtsgronden-aanstelling/"
  end

  match "/rechtsgronden-beeindiging/*path" do
    forward conn, path, "http://resource/rechtsgronden-beeindiging/"
  end

  match "/rechtstreekse-verkiezingen/*path" do
    forward conn, path, "http://cache/rechtstreekse-verkiezingen/"
  end

  match "/rechtsgronden/*path" do
    forward conn, path, "http://resource/rechtsgronden/"
  end

  match "/tijdsgebonden-entiteiten/*path" do
    forward conn, path, "http://cache/tijdsgebonden-entiteiten/"
  end

  match "/tijdsintervallen/*path" do
    forward conn, path, "http://resource/tijdsintervallen/"
  end

  match "/verkiezingsresultaten/*path" do
    forward conn, path, "http://cache/verkiezingsresultaten/"
  end

  match "/verkiezingsresultaat-gevolg-codes/*path" do
    forward conn, path, "http://cache/verkiezingsresultaat-gevolg-codes/"
  end

  match "/templates/*path" do
    forward conn, path, "http://cache/templates/"
  end

  match "/editor-documents/*path" do
    forward conn, path, "http://cache/editor-documents/"
  end

  match "/document-containers/*path" do
    forward conn, path,  "http://resource/document-containers/"
  end

  match "/editor-document-statuses/*path" do
    forward conn, path, "http://cache/editor-document-statuses/"
  end

  match "/editor-document-folders/*path" do
    forward conn, path, "http://cache/editor-document-folders/"
  end

  match "/rdfs-classes/*path" do
    forward conn, path, "http://cache/rdfs-classes/"
  end

  match "/rdfs-properties/*path" do
    forward conn, path, "http://cache/rdfs-properties/"
  end

  #################################################################
  # Adressenregister
  #################################################################
  match "/adressenregister/*path" do
    forward conn, path, "http://adressenregister/"
  end

  match "/prepublish/*path" do
    forward conn, path, "http://preimporter/prepublish/"
  end

  match "/signing/*path" do
    forward conn, path, "http://preimporter/signing/"
  end

  match "/signed-resources/*path" do
    forward conn, path, "http://resource/signed-resources/"
  end

  match "/published-resources/*path" do
    forward conn, path, "http://resource/published-resources/"
  end

  match "/versioned-agendas/*path" do
    forward conn, path, "http://resource/versioned-agendas/"
  end

  match "/versioned-besluiten-lijsten/*path" do
    forward conn, path, "http://resource/versioned-besluiten-lijsten/"
  end

  match "/versioned-behandelingen/*path" do
    forward conn, path, "http://resource/versioned-behandelingen/"
  end

  match "/versioned-notulen/*path" do
    forward conn, path, "http://resource/versioned-notulen/"
  end

  match "/blockchain-statuses/*path" do
    forward conn, path, "http://resource/blockchain-statuses/"
  end

  match "/verkeersbordcombinaties/*path" do
    forward conn, path, "http://resource/verkeersbordcombinaties/"
  end

  match "/maatregelconcepten/*path" do
    forward conn, path, "http://resource/maatregelconcepten/"
  end

  match "/verkeersbordconcepten/*path" do
    forward conn, path, "http://resource/verkeersbordconcepten/"
  end

  match "/verkeersbordcategorieen/*path" do
    forward conn, path, "http://resource/verkeersbordcategorieen/"
  end

  match "/verkeersbordconcept-status-codes/*path" do
    forward conn, path, "http://resource/verkeersbordconcept-status-codes/"
  end

  match "/intermissions/*path" do
    forward conn, path, "http://resource/intermissions/"
  end

  match "/agenda-positions/*path" do
    forward conn, path, "http://resource/agenda-positions/"
  end

  match "/files/*path" do
    forward conn, path, "http://file/files/"
  end
  #######
  # Tasks
  #######

  match "/tasklists/*path" do
    forward conn, path, "http://cache/tasklists/"
  end

  match "/tasks/*path" do
    forward conn, path, "http://cache/tasks/"
  end

  match "/tasklist-solutions/*path" do
    forward conn, path, "http://cache/tasklist-solutions/"
  end

  match "/task-solutions/*path" do
    forward conn, path, "http://cache/task-solutions/"
  end

  #################################################################
  # slave leidinggevenden
  #################################################################
  match "/bestuursfuncties/*path" do
    forward conn, path, "http://cache/bestuursfuncties/"
  end

  match "/functionarissen/*path" do
    forward conn, path, "http://cache/functionarissen/"
  end

  match "/contact-punten/*path" do
    forward conn, path, "http://cache/contact-punten/"
  end

  match "/adressen/*path" do
    forward conn, path, "http://cache/adressen/"
  end

  match "/functionaris-status-codes/*path" do
    forward conn, path, "http://cache/functionaris-status-codes/"
  end

  match "/agendapoint-service/*path" do
    forward conn, path, "http://agendapoint-service/"
  end

  #########
  # login
  ########
  match "/mock/sessions/*path" do
    forward conn, path, "http://mocklogin/sessions/"
  end

  match "/sessions/*path" do
    forward conn, path, "http://login/sessions/"
  end

  match "/gebruikers/*path" do
    forward conn, path, "http://cache/gebruikers/"
  end

  match "/accounts/*path" do
    forward conn, path, "http://cache/accounts/"
  end

  post "/remote-login/*path" do
    forward conn, [], "http://remotelogin/remote-login"
  end

  match "/query/*path" do
    forward conn, path, "http://yasgui/"
  end
  # match _ do
  #   send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  # end
end
