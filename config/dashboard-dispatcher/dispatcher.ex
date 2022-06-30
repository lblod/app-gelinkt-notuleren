defmodule Dispatcher do
  use Matcher
  define_accept_types [
    html: ["text/html", "application/xhtml+html"],
    json: ["application/json", "application/vnd.api+json"],
    any: [ "*/*" ],
  ]

  define_layers [ :api, :frontend, :not_found ]
  
  get "/files/:id/download", %{ accept: %{any: true}, layer: :api} do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  get "/files/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/files/"
  end

  #########
  # login
  ########
  match "/mock/sessions/*path", %{ accept: %{json: true}, layer: :api} do
    forward conn, path, "http://mocklogin/sessions/"
  end

  match "/gebruikers/*path", %{ accept: %{json: true}, layer: :api} do
    forward conn, path, "http://cache/gebruikers/"
  end

  match "/accounts/*path", %{ accept: %{json: true}, layer: :api} do
    forward conn, path, "http://cache/accounts/"
  end

  
  #################################################################
  # Dashboard routes
  #################################################################

  # Reports
  get "/reports/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/reports/"
  end

  # Logs
  get "/log-entries/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/log-entries/"
  end

  get "/log-levels/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/log-levels/"
  end

  get "/status-codes/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/status-codes/"
  end

  get "/log-sources/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/log-sources/"
  end

  get "/status-codes/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/acm-idm-service-log-entries/"
  end
  
  # Jobs
  get "/jobs/*path", %{ accept: %{json: true}, layer: :api} do
    Proxy.forward conn, path, "http://resource/jobs/"
  end

  ###############################################################
  # dashboard frontend layer
  ###############################################################
  match "/assets/*path", %{ accept: %{any: true}, layer: :frontend } do
    Proxy.forward conn, path, "http://dashboard/assets/"
  end

  match "/@appuniversum/*path", %{ accept: %{any: true}, layer: :frontend } do
    Proxy.forward conn, path, "http://dashboard/@appuniversum/"
  end

  match "/*path", %{ accept: %{html: true}, layer: :frontend } do
    Proxy.forward conn, [], "http://dashboard/index.html"
  end

  match "/*_path", %{ accept: %{html: true}, layer: :frontend } do
    Proxy.forward conn, [], "http://dashboard/index.html"
  end

  ###############################################################
  # Not found
  ###############################################################
  match "/*_", %{ accept: %{any: true}, layer: :not_found} do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end
end
