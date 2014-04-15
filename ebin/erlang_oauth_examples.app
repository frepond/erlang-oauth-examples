{application, erlang_oauth_examples, [
  {description, "Client and example modules for working with various sources"},
  {vsn, "0.1.0"},
  {modules, [oauth_gcal, oauth_google, oauth_mochiweb_client,
             oauth_termie, oauth_client, oauth_gcontacts, 
             oauth_mochiweb, oauth_mochiweb_server, oauth_twitter]},
  {registered, []},
  {applications, [kernel, stdlib, crypto, public_key, inets]}
]}.
