%%%-------------------------------------------------------------------
%% @doc firmware_uploader public API
%%
%%  Rewrite of a small work utility "FirmwareUploader",
%%  which gets called once a day by network cronjob
%%  to upload new firmwares to later be automatically
%%  downloaded and installed by remote devices the morning after.
%%
%%  1. Ping server.
%%  2. Get firmware files from folder.
%%  3. Find set of customers from files.
%%  4. Fetch existing firmwares for provided customers.
%%  5. Upload firmwares that aren't already present.
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
    
    io:format("\nAttempting to contact server.\n"),
    
    case ping_server() of
      ok ->
        io:format("Server responded ok.\n\n"),
        get_firmware_files_in_folder(?FOLDER);
      error ->
        io:format("Could not connect to server.\nAborting.\n"), error
    end.

ping_server() ->
    {Result, _} = httpc:request(get, {?SERVER, []}, [], []),
    Result.

get_firmware_files_in_folder(Folder) ->
    {_, Filenames} = file:list_dir(Folder),
    FirmwaresInFolder = lists:filter(fun (F) -> is_firmware_file(F) end, Filenames),
    get_firmwares_already_in_storage(FirmwaresInFolder).

get_firmwares_already_in_storage([]) ->
    io:format("Found no firmwares in folder ~p.\nAborting.\n", [?FOLDER]);

get_firmwares_already_in_storage(FirmwaresInFolder) ->
    io:format("Found firmwares in folder:\n"),
    print_firmwares(FirmwaresInFolder),
    
    Customers = customers_from_files(FirmwaresInFolder),
    FirmwaresFromStorage = existing_firmwares_for_customers(Customers),
    io:format("\nStorage already has:\n"),
    print_firmwares(FirmwaresFromStorage),
    
    NewFirmwares = get_unique_firmwares(FirmwaresInFolder,FirmwaresFromStorage),
    upload_firmware_set(NewFirmwares).

upload_firmware_set([]) -> io:format("\nNo new firmwares to upload.\nAborting.\n");
upload_firmware_set(NewFirmwares) ->
    io:format("\nNew firmwares:\n"),
    print_firmwares(NewFirmwares),
    upload_new_firmwares(NewFirmwares).

get_unique_firmwares(FirmwaresInFolder, FirmwaresFromStorage) ->
    lists:subtract(FirmwaresInFolder, FirmwaresFromStorage).

existing_firmwares_for_customers(Customers) ->
    lists:foldl(fun (F, List) -> 
        lists:append(existing_firmwares_for_customer(F), List) end, 
    [], Customers).

existing_firmwares_for_customer(Customer) ->
    Request = {string:join([?SERVER, ?GET_CUSTOMER, Customer], ""), []},
    {Result, {_, _, Data}} = httpc:request(get, Request, [], []),
    case Result of
      ok -> string:split(Data, ",", all);
      _ -> []
    end.

upload_new_firmwares(Firmwares) ->
    io:format("\nUploading firmwares:\n"),
    lists:foreach(fun(F) -> upload_file(F) end, Firmwares),
    io:format("\nDone.\n").

upload_file(File) ->
    {_, Data} = file:read_file(string:join([?FOLDER, File], "")),
    Request = {string:join([?SERVER, ?FIRMWARE_POST, File], ""), [], "application/zip", Data},
    {Result, _} = httpc:request(post, Request, [], []),
    io:format("\t~p: ~p.\n", [File, Result]).

customers_from_files(Files) ->
    Customers = lists:map(fun (F) -> customer_from_file(F) end, Files),
    Set = sets:from_list(Customers),
    sets:to_list(Set).

customer_from_file(File) ->
    [Customer, _, _, _] = split_on_underscores(File),
    Customer.

print_firmwares(Filenames) ->
    lists:foreach(fun (F) -> io:format("\t~p\n", [F]) end,
		  Filenames).

is_firmware_file(File) ->
    (string:str(File, ".zip") > 0) and
      (length(split_on_underscores(File)) == 4).

split_on_underscores(String) ->
    string:split(String, "_", all).
