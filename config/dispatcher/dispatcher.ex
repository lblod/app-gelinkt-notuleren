defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    any: [ "*/*" ]
  ]

  def dispatch_frontend( conn ) do
    has_publicatie =
      conn
        |> Plug.Conn.get_req_header( "host" )
        |> hd # assume only one host header
        |> String.contains?("publicatie")

    if has_publicatie do
      "publicatie"
    else
      "editor"
    end
  end

  @html %{ accept: %{ html: true } }
  @json %{ accept: %{ json: true } }
  @any %{ accept: %{ any: true } }

  match "/blockchain/*path", @json do
    forward conn, path, "http://blockchain/"
  end

   match "/decisionservice/*path", @json do
    forward conn, path, "http://decisionservice/"
  end

  match "/agendas/*path", @json do
    forward conn, path, "http://resource/agendas/"
  end

  match "/besluitenlijsten/*path", @json do
    forward conn, path, "http://resource/besluitenlijsten/"
  end

  match "/uittreksels/*path", @json do
    forward conn, path, "http://resource/uittreksels/"
  end

  match "/prepublished-agendas/*path", @json do
    forward conn, path, "http://resource/prepublished-agendas/"
  end

  match "/agendapunten/*path", @json do
    forward conn, path, "http://resource/agendapunten/"
  end

  match "/artikels/*path", @json do
    forward conn, path, "http://resource/artikels/"
  end

  match "/behandelingen-van-agendapunten/*path", @json do
    forward conn, path, "http://resource/behandelingen-van-agendapunten/"
  end

  match "/besluiten/*path", @json do
    forward conn, path, "http://resource/besluiten/"
  end

  match "/bestuurseenheden/*path", @json do
    forward conn, path, "http://cache/bestuurseenheden/"
  end

  match "/werkingsgebieden/*path", @json do
    forward conn, path, "http://cache/werkingsgebieden/"
  end

  match "/bestuurseenheid-classificatie-codes/*path", @json do
    forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end

  match "/bestuursorganen/*path", @json do
    forward conn, path, "http://resource/bestuursorganen/"
  end

  match "/bestuursorgaan-classificatie-codes/*path", @json do
    forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end

  match "/rechtsgronden-besluit/*path", @json do
    forward conn, path, "http://resource/rechtsgronden-besluit/"
  end

  match "/rechtsgronden-artikel/*path", @json do
    forward conn, path, "http://resource/rechtsgronden-artikel/"
  end

  match "/stemmingen/*path", @json do
    forward conn, path, "http://resource/stemmingen/"
  end

  match "/zittingen/*path", @json do
    forward conn, path, "http://resource/zittingen/"
  end

  match "/notulen/*path", @json do
    forward conn, path, "http://resource/notulen/"
  end

  match "/fracties/*path", @json do
    forward conn, path, "http://resource/fracties/"
  end

  match "/fractietypes/*path", @json do
    forward conn, path, "http://cache/fractietypes/"
  end

  match "/geboortes/*path", @json do
    forward conn, path, "http://cache/geboortes/"
  end

  match "/lijsttypes/*path", @json do
    forward conn, path, "http://cache/lijsttypes/"
  end

  match "/kandidatenlijsten/*path", @json do
    forward conn, path, "http://cache/kandidatenlijsten/"
  end

  match "/lidmaatschappen/*path", @json do
    forward conn, path, "http://resource/lidmaatschappen/"
  end

  match "/mandaten/*path", @json do
    forward conn, path, "http://resource/mandaten/"
  end

  match "/bestuursfunctie-codes/*path", @json do
    forward conn, path, "http://cache/bestuursfunctie-codes/"
  end

  match "/mandatarissen/*path", @json do
    forward conn, path, "http://resource/mandatarissen/"
  end

  match "/mandataris-status-codes/*path", @json do
    forward conn, path, "http://cache/mandataris-status-codes/"
  end

  match "/beleidsdomein-codes/*path", @json do
    forward conn, path, "http://cache/beleidsdomein-codes/"
  end

  match "/personen/*path", @json do
    forward conn, path, "http://cache/personen/"
  end

  match "/geslacht-codes/*path", @json do
    forward conn, path, "http://cache/geslacht-codes/"
  end

  match "/identificatoren/*path", @json do
    forward conn, path, "http://resource/identificatoren/"
  end

  match "/rechtsgronden-aanstelling/*path", @json do
    forward conn, path, "http://resource/rechtsgronden-aanstelling/"
  end

  match "/rechtsgronden-beeindiging/*path", @json do
    forward conn, path, "http://resource/rechtsgronden-beeindiging/"
  end

  match "/rechtstreekse-verkiezingen/*path", @json do
    forward conn, path, "http://cache/rechtstreekse-verkiezingen/"
  end

  match "/rechtsgronden/*path", @json do
    forward conn, path, "http://resource/rechtsgronden/"
  end

  match "/tijdsgebonden-entiteiten/*path", @json do
    forward conn, path, "http://cache/tijdsgebonden-entiteiten/"
  end

  match "/tijdsintervallen/*path", @json do
    forward conn, path, "http://resource/tijdsintervallen/"
  end

  match "/verkiezingsresultaten/*path", @json do
    forward conn, path, "http://cache/verkiezingsresultaten/"
  end

  match "/verkiezingsresultaat-gevolg-codes/*path", @json do
    forward conn, path, "http://cache/verkiezingsresultaat-gevolg-codes/"
  end

  match "/templates/*path", @json do
    forward conn, path, "http://cache/templates/"
  end

  match "/editor-documents/*path", @json do
    forward conn, path, "http://cache/editor-documents/"
  end

  match "/document-containers/*path", @json do
    forward conn, path,  "http://resource/document-containers/"
  end

  match "/editor-document-statuses/*path", @json do
    forward conn, path, "http://cache/editor-document-statuses/"
  end

  match "/rdfs-classes/*path", @json do
    forward conn, path, "http://cache/rdfs-classes/"
  end

  match "/rdfs-properties/*path", @json do
    forward conn, path, "http://cache/rdfs-properties/"
  end

  post "/sync/*path", @json do
    forward conn, path, "http://sync/sync"
  end

  #################################################################
  # Adressenregister
  #################################################################
  match "/adressenregister/*path", @json do
    forward conn, path, "http://adressenregister/"
  end

  ############
  # Blockchain
  ############
  match "/prepublish/*path", @json do
    forward conn, path, "http://preimporter/prepublish/"
  end

  match "/signing/*path", @json do
    forward conn, path, "http://preimporter/signing/"
  end

  match "/signed-resources/*path", @json do
    forward conn, path, "http://resource/signed-resources/"
  end

  match "/published-resources/*path", @json do
    forward conn, path, "http://resource/published-resources/"
  end

  match "/versioned-agendas/*path", @json do
    forward conn, path, "http://resource/versioned-agendas/"
  end

  match "/versioned-besluiten-lijsten/*path", @json do
    forward conn, path, "http://resource/versioned-besluiten-lijsten/"
  end

  match "/versioned-behandelingen/*path", @json do
    forward conn, path, "http://resource/versioned-behandelingen/"
  end

  match "/versioned-notulen/*path", @json do
    forward conn, path, "http://resource/versioned-notulen/"
  end

  match "/blockchain-statuses/*path", @json do
    forward conn, path, "http://resource/blockchain-statuses/"
  end

  match "/verkeersbordcombinaties/*path", @json do
    forward conn, path, "http://resource/verkeersbordcombinaties/"
  end

  match "/maatregelconcepten/*path", @json do
    forward conn, path, "http://resource/maatregelconcepten/"
  end

  match "/verkeersbordconcepten/*path", @json do
    forward conn, path, "http://resource/verkeersbordconcepten/"
  end

  match "/verkeersbordcategorieen/*path", @json do
    forward conn, path, "http://resource/verkeersbordcategorieen/"
  end

  match "/verkeersbordconcept-status-codes/*path", @json do
    forward conn, path, "http://resource/verkeersbordconcept-status-codes/"
  end

  #######
  # Tasks
  #######

  match "/tasklists/*path", @json do
    forward conn, path, "http://cache/tasklists/"
  end

  match "/tasks/*path", @json do
    forward conn, path, "http://cache/tasks/"
  end

  match "/tasklist-solutions/*path", @json do
    forward conn, path, "http://cache/tasklist-solutions/"
  end

  match "/task-solutions/*path", @json do
    forward conn, path, "http://cache/task-solutions/"
  end

  #################################################################
  # slave leidinggevenden
  #################################################################
  match "/bestuursfuncties/*path", @json do
    forward conn, path, "http://cache/bestuursfuncties/"
  end

  match "/functionarissen/*path", @json do
    forward conn, path, "http://cache/functionarissen/"
  end

  match "/contact-punten/*path", @json do
    forward conn, path, "http://cache/contact-punten/"
  end

  match "/adressen/*path", @json do
    forward conn, path, "http://cache/adressen/"
  end

  match "/functionaris-status-codes/*path", @json do
    forward conn, path, "http://cache/functionaris-status-codes/"
  end

  #########
  # login
  ########
  match "/mock/sessions/*path", @json do
    forward conn, path, "http://mocklogin/sessions/"
  end

  match "/sessions/*path", @json do
    forward conn, path, "http://login/sessions/"
  end

  match "/gebruikers/*path", @json do
    forward conn, path, "http://cache/gebruikers/"
  end

  match "/accounts/*path", @json do
    forward conn, path, "http://cache/accounts/"
  end

  post "/remote-login/*_path", @json do
    forward conn, [], "http://remotelogin/remote-login"
  end

  match "/assets/*path", @any do
    frontend = dispatch_frontend(conn)
    if frontend == "editor" do
      forward conn, path, "http://editor/assets/"
    else
      forward conn, path, "http://publicatie/assets/"
    end
  end

  match "/*path", @html do
    frontend = dispatch_frontend(conn)
    if frontend == "editor" do
      forward conn, [], "http://editor/index.html"
    else
       forward conn, path, "http://publicatie/"
    end
  end

  match "_", %{ last_call: true, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  last_match
end
