%% file: mysql_test.erl
%% author: Yariv Sadan (yarivvv@gmail.com)
%% for license see COPYING

-module(contacts_mig).
-compile(export_all).

update_contacts([[ID, DispName]|T]) ->
    mysql:prepare(update_contacts, <<"UPDATE SYNC_Contacts SET sort_name=? WHERE __id=?">>),
    SortName = mbcs:encode(mbcs:decode(DispName, utf8), gbk, [{error, ignore}] ),
    io:format("SortName: ~p~n", [SortName]),
    io:format("ID: ~p - DispName: ~p~n", [ID, DispName]),
    Result = mysql:execute(p1, update_contacts, [SortName, ID]),
    %mysql:prepare(find, <<"select * from SYNC_Contacts where __id=?">>),
    %Result = mysql:execute(p1, find, [ID]),
    io:format("mysql:execute result: ~p~n", [Result]),
    update_contacts(T),
    ok;
update_contacts([]) ->
    ok.

testzh() ->
    Zh = "Erlang的Unicode支持",
    io:format("~ts~n",[Zh]).

start() ->
    code:add_patha("/home/qujian/project/erlang/src/mbcs"),
    RetMbcs = mbcs:start(),
    io:format("RetMbcs: ~p~n", [RetMbcs]),
    io:format("mbcs:encode(\"世界，你好\", gbk, [{error, ignore}] ) = ~p~n", [mbcs:encode("世界，你好", gbk, [{error, ignore}] ) ] ),
    %compile:file("/usr/local/lib/erlang/lib/mysql/mysql.erl"),
    %compile:file("/usr/local/lib/erlang/lib/mysql/mysql_conn.erl"),
    
    %% Start the MySQL dispatcher and create the first connection
    %% to the database. 'p1' is the connection pool identifier.
    mysql:start_link(p1, "10.8.10.216", undefined, "dbwrite", "dbwrite", "synchronizedb", undefined, utf8),
    %% mysql -h10.8.10.216 -udbwrite -pdbwrite -Dsynchronizedb

    %% Add 2 more connections to the connection pool
    %mysql:connect(p1, "localhost", undefined, "root", "password", "test",
						%		  true),
    %mysql:connect(p1, "localhost", undefined, "root", "password", "test",
						%		  true),
    
    %mysql:fetch(p1, <<"DELETE FROM developer">>),

    %mysql:fetch(p1, <<"INSERT INTO developer(name, country) VALUES "
%		     "('Claes (Klacke) Wikstrom', 'Sweden'),"
%		     "('Ulf Wiger', 'USA')">>),

    %% Execute a query (using a binary)
    Result1 = mysql:fetch(p1, <<"SELECT __id, data1 FROM SYNC_ContactsData WHERE mime='vnd.android.cursor.item/name'">>),
    %io:format("Result1: ~p~n", [Result1]),
    {data, {mysql_result, _, Rowset, _, _} } = Result1,
    %io:format("Rowset: ~p~n", [Rowset]),
    update_contacts(Rowset),

    %% Register a prepared statement
    %mysql:prepare(update_developer_country,
%		  <<"UPDATE developer SET country=? where name like ?">>),
    
    %% Execute the prepared statement
    %mysql:execute(p1, update_developer_country, [<<"Sweden">>, <<"%Wiger">>]),
    
    %Result2 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    %io:format("Result2: ~p~n", [Result2]),
    
    %mysql:transaction(
    %  p1,
    %  fun() -> mysql:fetch(<<"INSERT INTO developer(name, country) VALUES "
%			    "('Joe Armstrong', 'USA')">>),
%	       mysql:fetch(<<"DELETE FROM developer WHERE name like "
%			    "'Claes%'">>)
%      end),

    %Result3 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    %io:format("Result3: ~p~n", [Result3]),
    
    %mysql:prepare(delete_all, <<"DELETE FROM developer">>),

%    {error, foo} = mysql:transaction(
%		     p1,
%		     fun() -> mysql:execute(delete_all),
%			      throw({error, foo})
%		     end),

    %Result4 = mysql:fetch(p1, <<"SELECT * FROM developer">>),
    %io:format("Result4: ~p~n", [Result4]),
    ok.
