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
    Proxy.forward conn, path, "http://resource/bestuurseenheden/"
  end

  match "/bestuurseenheid-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://resource/bestuurseenheid-classificatie-codes/"
  end

  match "/bestuursorganen/*path" do
    Proxy.forward conn, path, "http://resource/bestuursorganen/"
  end

  match "/bestuursorgaan-classificatie-codes/*path" do
    Proxy.forward conn, path, "http://resource/bestuursorgaan-classificatie-codes/"
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

  match "/geboortes/*path" do
    Proxy.forward conn, path, "http://resource/geboortes/"
  end

  match "/lijsttypes/*path" do
    Proxy.forward conn, path, "http://resource/lijsttypes/"
  end

  match "/kandidatenlijsten/*path" do
    Proxy.forward conn, path, "http://resource/kandidatenlijsten/"
  end

  match "/lidmaatschappen/*path" do
    Proxy.forward conn, path, "http://resource/lidmaatschappen/"
  end

  match "/mandaten/*path" do
    Proxy.forward conn, path, "http://resource/mandaten/"
  end

  match "/bestuursfunctie-codes/*path" do
    Proxy.forward conn, path, "http://resource/bestuursfunctie-codes/"
  end

  match "/mandatarissen/*path" do
    Proxy.forward conn, path, "http://resource/mandatarissen/"
  end

  match "/mandataris-status-codes/*path" do
    Proxy.forward conn, path, "http://resource/mandataris-status-codes/"
  end

  match "/beleidsdomein-codes/*path" do
    Proxy.forward conn, path, "http://resource/beleidsdomein-codes/"
  end

  match "/personen/*path" do
    Proxy.forward conn, path, "http://resource/personen/"
  end

  match "/geslacht-codes/*path" do
    Proxy.forward conn, path, "http://resource/geslacht-codes/"
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
    Proxy.forward conn, path, "http://resource/rechtstreekse-verkiezingen/"
  end

  match "/tijdsgebonden-entiteiten/*path" do
    Proxy.forward conn, path, "http://resource/tijdsgebonden-entiteiten/"
  end

  match "/tijdsintervallen/*path" do
    Proxy.forward conn, path, "http://resource/tijdsintervallen/"
  end

  match "/verkiezingsresultaten/*path" do
    Proxy.forward conn, path, "http://resource/verkiezingsresultaten/"
  end

  match "/verkiezingsresultaat-gevolg-codes/*path" do
    Proxy.forward conn, path, "http://resource/verkiezingsresultaat-gevolg-codes/"
  end

  match "/werkingsgebieden/*path" do
    Proxy.forward conn, path, "http://resource/werkingsgebieden/"
  end

  match "/templates/*path" do
    Proxy.forward conn, path, "http://resource/templates/"
  end

  match "/editor-documents/*path" do
    Proxy.forward conn, path, "http://resource/editor-documents/"
  end
  match "/editor-document-statuses/*path" do
    Proxy.forward conn, path, "http://resource/editor-document-statuses/"
  end

  match "/publish/*path" do
    Proxy.forward conn, path, "http://notulenimporter/publish/"
  end

  match _ do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
