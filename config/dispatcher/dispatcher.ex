defmodule Dispatcher do
  use Plug.Router

  def start(_argv) do
    port = 80
    IO.puts "Starting Plug with Cowboy on port #{port}"
    Plug.Adapters.Cowboy.http __MODULE__, [], port: port
    :timer.sleep(:infinity)
  end

  plug Plug.Logger
  plug :match
  plug :dispatch

  # In order to forward the 'themes' resource to the
  # resource service, use the following forward rule.
  #
  # docker-compose stop; docker-compose rm; docker-compose up
  # after altering this file.
  #
  # match "/themes/*path" do
  #   Proxy.forward conn, path, "http://resource/themes/"
  # end

  match "/agendas/*path" do
    Proxy.forward conn, path, "http://resource/agendas/"
  end
  match "/agendapunten/*path" do
    Proxy.forward conn, path, "http://resource/agendapunten/"
  end
  match "/artikels/*path" do
    Proxy.forward conn, path, "http://resource/artikels/"
  end
  match "/behandelingen-van-agendapunten/*path" do
    Proxy.forward conn, path, "http://resource/behandelingen-van-agendapunten/"
  end
  match "/besluiten/*path" do
    Proxy.forward conn, path, "http://resource/besluiten/"
  end
  match "/bestuurseenheden/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheden/"
  end
  match "/werkingsgebieden/*path" do
    Proxy.forward conn, path, "http://cache/werkingsgebieden/"
  end
  match "/bestuurseenheid-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuurseenheid-classificatie-codes/"
  end
  match "/bestuursorganen/*path" do
    Proxy.forward conn, path, "http://resource/bestuursorganen/"
  end
  match "/bestuursorgaan-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuursorgaan-classificatie-codes/"
  end
  match "/rechtsgronden-besluit/*path" do
    Proxy.forward conn, path, "http://resource/rechtsgronden-besluit/"
  end
  match "/rechtsgronden-artikel/*path" do
    Proxy.forward conn, path, "http://resource/rechtsgronden-artikel/"
  end
  match "/stemmingen/*path" do
    Proxy.forward conn, path, "http://resource/stemmingen/"
  end
  match "/zittingen/*path" do
    Proxy.forward conn, path, "http://resource/zittingen/"
  end
  match "/entiteiten/*path" do
    Proxy.forward conn, path, "http://resource/entiteiten/"
  end
  match "/fracties/*path" do
    Proxy.forward conn, path, "http://resource/fracties/"
  end
  match "/fractietypes/*path" do
    Proxy.forward conn, path, "http://cache/fractietypes/"
  end
  match "/geboortes/*path" do
    Proxy.forward conn, path, "http://cache/geboortes/"
  end
  match "/lijsttypes/*path" do
    Proxy.forward conn, path, "http://cache/lijsttypes/"
  end
  match "/kandidatenlijsten/*path" do
    Proxy.forward conn, path, "http://cache/kandidatenlijsten/"
  end
  match "/lidmaatschappen/*path" do
    Proxy.forward conn, path, "http://resource/lidmaatschappen/"
  end
  match "/mandaten/*path" do
    Proxy.forward conn, path, "http://resource/mandaten/"
  end
  match "/bestuursfunctie-codes/*path" do
    Proxy.forward conn, path, "http://cache/bestuursfunctie-codes/"
  end
  match "/mandatarissen/*path" do
    Proxy.forward conn, path, "http://resource/mandatarissen/"
  end
  match "/mandataris-status-codes/*path" do
    Proxy.forward conn, path, "http://cache/mandataris-status-codes/"
  end
  match "/beleidsdomein-codes/*path" do
    Proxy.forward conn, path, "http://cache/beleidsdomein-codes/"
  end
  match "/personen/*path" do
    Proxy.forward conn, path, "http://cache/personen/"
  end
  match "/geslacht-codes/*path" do
    Proxy.forward conn, path, "http://cache/geslacht-codes/"
  end
  match "/identificatoren/*path" do
    Proxy.forward conn, path, "http://resource/identificatoren/"
  end
  match "/rechtsgronden-aanstelling/*path" do
    Proxy.forward conn, path, "http://resource/rechtsgronden-aanstelling/"
  end
  match "/rechtsgronden-beeindiging/*path" do
    Proxy.forward conn, path, "http://resource/rechtsgronden-beeindiging/"
  end
  match "/rechtstreekse-verkiezingen/*path" do
    Proxy.forward conn, path, "http://cache/rechtstreekse-verkiezingen/"
  end
  match "/rechtsgronden/*path" do
    Proxy.forward conn, path, "http://resource/rechtsgronden/"
  end
  match "/tijdsgebonden-entiteiten/*path" do
    Proxy.forward conn, path, "http://cache/tijdsgebonden-entiteiten/"
  end
  match "/tijdsintervallen/*path" do
    Proxy.forward conn, path, "http://resource/tijdsintervallen/"
  end
  match "/verkiezingsresultaten/*path" do
    Proxy.forward conn, path, "http://cache/verkiezingsresultaten/"
  end
  match "/verkiezingsresultaat-gevolg-codes/*path" do
    Proxy.forward conn, path, "http://cache/verkiezingsresultaat-gevolg-codes/"
  end
  match "/vestigingen/*path" do
    Proxy.forward conn, path, "http://cache/vestigingen/"
  end
  match "/contact-punten/*path" do
    Proxy.forward conn, path, "http://cache/contact-punten/"
  end
  match "/posities/*path" do
    Proxy.forward conn, path, "http://cache/posities/"
  end
  match "/rollen/*path" do
    Proxy.forward conn, path, "http://cache/rollen/"
  end
  match "/organisaties/*path" do
    Proxy.forward conn, path, "http://cache/organisaties/"
  end
  match "/templates/*path" do
    Proxy.forward conn, path, "http://cache/templates/"
  end
  match "/editor-documents/*path" do
    Proxy.forward conn, path, "http://cache/editor-documents/"
  end
  match "/document-containers/*path" do
    Proxy.forward conn, path,  "http://cache/document-containers/"
  end
  match "/editor-document-statuses/*path" do
    Proxy.forward conn, path, "http://cache/editor-document-statuses/"
  end

  match "/publish/*path" do
    Proxy.forward conn, path, "http://notulenimporter/publish/"
  end

  match "/rdfs-classes/*path" do
    Proxy.forward conn, path, "http://cache/rdfs-classes/"
  end

  match "/rdfs-properties/*path" do
    Proxy.forward conn, path, "http://cache/rdfs-properties/"
  end

  #######
  # Tasks
  #######

  match "/tasklists/*path" do
    Proxy.forward conn, path, "http://cache/tasklists/"
  end

  match "/tasks/*path" do
    Proxy.forward conn, path, "http://cache/tasks/"
  end

  match "/tasklist-solutions/*path" do
    Proxy.forward conn, path, "http://cache/tasklist-solutions/"
  end

  match "/task-solutions/*path" do
    Proxy.forward conn, path, "http://cache/task-solutions/"
  end

  #########
  # login
  ########
  match "/mock/sessions/*path" do
    Proxy.forward conn, path, "http://mocklogin/sessions/"
  end
  match "/sessions/*path" do
    Proxy.forward conn, path, "http://login/sessions/"
  end  
  match "/gebruikers/*path" do
    Proxy.forward conn, path, "http://cache/gebruikers/"
  end
  match "/accounts/*path" do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
