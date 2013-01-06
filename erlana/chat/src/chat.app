{application, chat,
 [{description, "chat"},
  {vsn, "0.01"},
  {modules, [
    chat,
    chat_app,
    chat_sup,
    chat_web,
    chat_deps
  ]},
  {registered, []},
  {mod, {chat_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
