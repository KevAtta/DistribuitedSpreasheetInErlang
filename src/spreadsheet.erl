-module(spreadsheet).
-include("record.hrl").
-export([
    new/1, new/4,
    start_mnesia/0,
    start_table/0,
    stop/0,
    loop/0,
    init/1,
    get/4,
    set/5, set/6,
    get/5,
    share/2
]).

% Valori di default
defaults() -> #{row => 3, col => 2, tabs => 2}.

% Inizializza il database Mnesia
start_mnesia() ->
    mnesia:create_schema([node() | nodes()]),
    mnesia:start().

start_table() ->
    mnesia:create_table(spreadsheet, [
        {attributes, record_info(fields, spreadsheet)}, {disc_copies, [node() | nodes()]}
    ]).

loop() ->
    receive
        {new_default, From, SpreadsheetName} ->
            From ! new(SpreadsheetName),
            loop();
        {new, From, SpreadsheetName, NumRow, NumCol, NumTab} ->
            From ! new(SpreadsheetName, NumRow, NumCol, NumTab),
            loop();
        {get, From, SpreadsheetName, TabIndex, RowIndex, ColIndex} ->
            From ! get(SpreadsheetName, TabIndex, RowIndex, ColIndex),
            loop();
        {get_timeout, From, SpreadsheetName, TabIndex, RowIndex, ColIndex, Timeout} ->
            From ! get(SpreadsheetName, TabIndex, RowIndex, ColIndex, Timeout),
            loop();
        {set, From, SpreadsheetName, TabIndex, RowIndex, ColIndex, Value} ->
            From ! set(SpreadsheetName, TabIndex, RowIndex, ColIndex, Value),
            loop();
        {set_timeout, From, SpreadsheetName, TabIndex, RowIndex, ColIndex, Value, Timeout} ->
            From ! set(SpreadsheetName, TabIndex, RowIndex, ColIndex, Value, Timeout),
            loop();
        {share, From, SpreadsheetName, AccessPolicies} ->
            From ! share(SpreadsheetName, AccessPolicies),
            loop();
        {info, From, SpreadsheetName} ->
            From ! info:info(SpreadsheetName), 
            loop();
        {to_csv, From, SpreadsheetName, FileName} ->
            From ! csv_manage:to_csv(SpreadsheetName, FileName),
            loop();
        {to_csv_timeout, From, SpreadsheetName, FileName, Timeout} -> 
            From ! csv_manage:to_csv_timeout(SpreadsheetName, FileName, Timeout),
            loop();
        {from_csv, From, FileName} ->
            From ! csv_manage:from_csv(FileName),
            loop();
        {from_csv_timeout, From, FileName, Timeout} -> 
            From ! csv_manage:from_csv_timeout(FileName, Timeout),
            loop();
        {delete_row, From, SpreadsheetName, TabNumber, RowNumber} ->
            From ! modifie_row:del_row(SpreadsheetName, TabNumber, RowNumber),
            loop();
        {add_row, From, SpreadsheetName, TabNumber} -> 
            From ! modifie_row:add_row(SpreadsheetName, TabNumber),
            loop();
        {stop} ->
            stop()
    end.

init(RegisterName) ->
    Pid = spawn(spreadsheet, loop, []),
    register(RegisterName, Pid),
    Pid.

% Ferma il database Mnesia
stop() ->
    mnesia:stop().

% Crea un nuovo foglio con nome e dimensioni di default
new(SpreadsheetName) ->
    Defaults = defaults(),
    new(SpreadsheetName, maps:get(row, Defaults), maps:get(col, Defaults), maps:get(tabs, Defaults)).

% Crea un nuovo foglio con nome e dimensioni specificate (il foglio non sara' sovrascrivibile)
new(SpreadsheetName, NumRow, NumCol, NumTab) ->
    try
        Tabs = lists:map(fun(_) -> new_tab(NumRow, NumCol) end, lists:seq(1, NumTab)),
        AccessPolicies = [],
        Spreadsheet = #spreadsheet{
            name = SpreadsheetName, tabs = Tabs, owner = self(), access_policies = AccessPolicies
        },
        Result = write_unique(Spreadsheet),
        case Result of
            {atomic, ok} ->
                {ok, Spreadsheet};
            {atomic, {error, already_exists}} ->
                {error, "Spreadsheet already exists"};
            _ ->
                {error, "Failed to create spreadsheet"}
        end
    catch
        _:_ ->
            {error, "Failed to create spreadsheet"}
    end.

% Crea una nuova tabella di dimensioni NxM
new_tab(NumRow, NumCol) ->
    lists:map(fun(_) -> new_row(NumCol) end, lists:seq(1, NumRow)).

% Crea una nuova riga di lunghezza M
new_row(NumCol) ->
    lists:map(fun(_) -> empty_cell() end, lists:seq(1, NumCol)).

% Crea una cella vuota
empty_cell() -> undef.

write_unique(Record) ->
    F = fun() ->
        case mnesia:read({spreadsheet, Record#spreadsheet.name}) of
            [] ->
                mnesia:write(Record);
            _ ->
                {error, already_exists}
        end
    end,
    mnesia:transaction(F).

% Inserisce un valore in una posizione specifica di una tab specifica
set(SpreadsheetName, TabIndex, RowIndex, ColIndex, Value) ->
    Fun = fun() ->
        case mnesia:read(spreadsheet, SpreadsheetName, write) of
            [Spreadsheet] ->
                Tabs = Spreadsheet#spreadsheet.tabs,
                Owner = Spreadsheet#spreadsheet.owner,
                AccessPolicies = Spreadsheet#spreadsheet.access_policies,
                HasAccess = (lists:any(
                    fun({Pid, AP}) -> ((AP =:= write) andalso (Pid =:= self())) end,
                    AccessPolicies
                )),
                if
                    ((Owner =:= self()) or HasAccess) ->
                        try
                            % Tab contiene la tabella alla posizione TabIndex
                            Tab = lists:nth(TabIndex, Tabs),
                            % Row contiene la riga alla posizione RowIndex dalla tabella
                            Row = lists:nth(RowIndex, Tab),
                            % nthtail(N, List) -> Tail; prendo tutti i valori prima del valore da inserire, ci accodo il valore,
                            % e infine riaccodo tutti i valori presenti in precedenza
                            NewRow =
                                lists:sublist(Row, ColIndex - 1) ++ [Value] ++
                                    lists:nthtail(ColIndex, Row),
                            % Creiamo una nuova tabella sostituendo la riga alla posizione RowIndex con NewRow
                            NewTab =
                                lists:sublist(Tab, RowIndex - 1) ++ [NewRow] ++
                                    lists:nthtail(RowIndex, Tab),
                            % Creiamo una nuova lista di tabelle sostituendo la tabella alla posizione TabIndex con NewTab
                            NewTabs =
                                lists:sublist(Tabs, TabIndex - 1) ++ [NewTab] ++
                                    lists:nthtail(TabIndex, Tabs),
                            % Creiamo un nuovo foglio sostituendo le tabelle con NewTabs
                            NewSpreadsheet = Spreadsheet#spreadsheet{tabs = NewTabs},
                            mnesia:write(NewSpreadsheet),
                            true
                        catch
                            _:_ ->
                                {error, "Invalid index", false}
                        end;
                    true ->
                        {error, "Access denied", false}
                end;
            _ ->
                {error, "Spreadsheet not found", false}
        end
    end,
    mnesia:transaction(Fun).

% Inserisce un valore in una posizione specifica di una tab specifica
set(SpreadsheetName, TabIndex, RowIndex, ColIndex, Value, Timeout) ->
    {Time, Result} = timer:tc(fun() ->
        spreadsheet:set(SpreadsheetName, TabIndex, RowIndex, ColIndex, Value)
    end),
    if
        Time > Timeout * 1000 -> timeout;
        true -> Result
    end.

% Legge il valore della cella (i,j) che appartiene al tab del foglio name
get(SpreadsheetName, TabIndex, RowIndex, ColIndex) ->
    Fun = fun() ->
        case mnesia:read(spreadsheet, SpreadsheetName, write) of
            [Spreadsheet] ->
                Tabs = Spreadsheet#spreadsheet.tabs,
                Owner = Spreadsheet#spreadsheet.owner,
                AccessPolicies = Spreadsheet#spreadsheet.access_policies,
                HasAccess = lists:any(
                    fun({Pid, AP}) -> ((AP =:= read) andalso (Pid =:= self())) end,
                    AccessPolicies
                ),
                if
                    ((Owner =:= self()) or HasAccess) ->
                        try
                            Tab = lists:nth(TabIndex, Tabs),
                            Row = lists:nth(RowIndex, Tab),
                            Value = lists:nth(ColIndex, Row),
                            Value
                        catch
                            _:_ ->
                                {error, "Invalid index"}
                        end;
                    true ->
                        {error, "Access denied"}
                end;
            _ ->
                {error, "Spreadsheet not found"}
        end
    end,
    mnesia:transaction(Fun).


% Legge il valore della cella (i,j) che appartiene al tab del foglio name con un timeout
get(SpreadsheetName, TabIndex, RowIndex, ColIndex, Timeout) ->
    {Time, Result} = timer:tc(fun() -> spreadsheet:get(SpreadsheetName, TabIndex, RowIndex, ColIndex) end),
    if
        Time > Timeout * 1000 -> timeout;
        true -> Result
    end.

share(SpreadsheetName, NewAccessPolicies) ->
    Fun = fun() ->
        {Pid, Policy} = NewAccessPolicies,
        if
            is_pid(Pid) andalso ((Policy =:= write) or (Policy =:= read)) ->
                case mnesia:read(spreadsheet, SpreadsheetName, write) of
                    [Spreadsheet] ->
                        if
                            %% controllo se chi sta invocando la funzione e' anche il proprietario
                            (Spreadsheet#spreadsheet.owner =:= self()) ->
                                OldAccessPolicies = Spreadsheet#spreadsheet.access_policies,
                                case
                                    lists:any(fun({PID, _}) -> PID =:= Pid end, OldAccessPolicies)
                                of
                                    true ->
                                        case
                                            lists:any(
                                                fun({PID, POLICY}) ->
                                                    (PID =:= Pid) andalso (POLICY =:= Policy)
                                                end,
                                                OldAccessPolicies
                                            )
                                        of
                                            true ->
                                                {error, "PID already in access policies"};
                                            false ->
                                                UpdatedAccessPolicies = lists:keyreplace(
                                                    Pid, 1, OldAccessPolicies, {Pid, Policy}
                                                ),
                                                NewSpreadsheet = Spreadsheet#spreadsheet{
                                                    access_policies = UpdatedAccessPolicies
                                                },
                                                mnesia:write(NewSpreadsheet),
                                                true
                                        end;
                                    false ->
                                        UpdatedAccessPolicies =
                                            OldAccessPolicies ++ [NewAccessPolicies],
                                        NewSpreadsheet = Spreadsheet#spreadsheet{
                                            access_policies = UpdatedAccessPolicies
                                        },
                                        mnesia:write(NewSpreadsheet),
                                        true
                                end;
                            true ->
                                {error, "Something went wrong"}
                        end;
                    _ ->
                        {error, "Spreadsheet not found"}
                end;
            true ->
                {error, "Insert a valid Pid or a valid policy"}
        end
    end,
    mnesia:transaction(Fun).
