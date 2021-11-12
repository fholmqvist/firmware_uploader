%%%-------------------------------------------------------------------
%% @doc firmware_uploader public API
%%
%%  Rewrite of a small work utility "FirmwareUploader",
%%  which gets called once a day by network cronjob
%%  to upload new firmwares to later be automatically
%%  downloaded and installed by remote devices the morning after.
%%
%%  Program preemptively terminates if any step fails or isn't needed.
%%
%%  1. Ping server.
%%  2. Get firmware files from folder.
%%  3. Determine the set of customers from files.
%%  4. Fetch existing firmwares for provided 
%%     customers from server.
%%  5. Determine the set of new firmwares by 
%%     diffing files with server response.
%%  6. Upload firmwares that aren't already present.
%%
%% @end
%%%-------------------------------------------------------------------

-module(firmware_uploader).

-export([main/1]).

-define(SERVER, "http://localhost:8007/").
-define(FIRMWARE_POST, "api/firmware/").
-define(GET_CUSTOMER, "api/firmware/for/").
-define(FOLDER, "./example_files/").

main(_args) ->
  inets:start(),
  ssl:start(),
  
  io:format("~nConnecting to server.~n"),
  
  case ping_server() of
    error ->
      io:format("Could not connect to server.~nAborting.~n"), error;
    ok ->
      io:format("Server responded ok.~n~n"),
      
      LocalFiles = files_in_folder(?FOLDER),
      CustomerNames = customer_names_from_files(LocalFiles),
      StorageFiles = storage_files_for_many(CustomerNames),
      NewFiles = lists:subtract(LocalFiles, StorageFiles),
      
      display_new_files(NewFiles),
      io:format("~nUploading firmwares:~n"),
      
      upload_new_files(NewFiles),
      io:format("~nDone.~n")
  end.

ping_server() ->
  {Result, _} = httpc:request(get, {?SERVER, []}, [], []),
  Result.

files_in_folder(Folder) ->
  {_, Filenames} = file:list_dir(Folder),
  lists:filter(fun (F) -> is_firmware_file(F) end, Filenames).

display_new_files([]) -> 
  io:format("~nNo new firmwares to upload.~nAborting.~n");

display_new_files(NewFiles) ->
  io:format("~nNew firmwares:~n"),
  lists:foreach(fun (F) -> 
    io:format("\t~p~n", [F]) end, 
  NewFiles).

storage_files_for_many(Customers) ->
    lists:foldl(fun (Customer, List) -> 
        lists:append(storage_files_for_single(Customer), List) end, 
    [], Customers).

storage_files_for_single(Customer) ->
    Request = {string:join([?SERVER, ?GET_CUSTOMER, Customer], ""), []},
    {Result, {_, _, Data}} = httpc:request(get, Request, [], []),
    case Result of
      ok -> string:split(Data, ",", all);
      _ -> []
    end.

upload_new_files(Firmwares) ->
    lists:foreach(fun(F) -> upload_file(F) end, Firmwares).

upload_file(File) ->
    {_, Data} = file:read_file(string:join([?FOLDER, File], "")),
    Request = {string:join([?SERVER, ?FIRMWARE_POST, File], ""), [], "application/zip", Data},
    {Result, _} = httpc:request(post, Request, [], []),
    io:format("\t~p: ~p.~n", [File, Result]).

customer_names_from_files(Files) ->
    Customers = lists:map(fun (F) -> customer_from_file(F) end, Files),
    Set = sets:from_list(Customers),
    sets:to_list(Set).

customer_from_file(File) ->
    [Customer, _, _, _] = split_on_underscores(File),
    Customer.

is_firmware_file(File) ->
    (string:str(File, ".zip") > 0) and
      (length(split_on_underscores(File)) == 4).

split_on_underscores(String) ->
    string:split(String, "_", all).
