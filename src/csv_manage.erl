-module(csv_manage).
-include("record.hrl").
-export([to_csv/2, to_csv_timeout/3, from_csv/1, from_csv_timeout/2]).

% Esporta in CSV l'INTERO foglio
to_csv(SpreadsheetName, Filename) ->
    ActualFilename =
        case lists:suffix(".csv", atom_to_list(Filename)) of
            true -> Filename;
            false -> atom_to_list(Filename) ++ ".csv"
        end,
    case mnesia:transaction(fun() -> mnesia:read({spreadsheet, SpreadsheetName}) end) of
        {atomic, []} ->
            {error, "Spreadsheet not found"};
        {atomic, [Spreadsheet]} ->
            try
                {ok, File} = file:open(ActualFilename, [write]),
                file:write(File, "name;tabs;owner;access_policies\n"),
                Data = io_lib:format(
                    "~s;~s;~p;~p\n",
                    [
                        Spreadsheet#spreadsheet.name,
                        format_tabs(Spreadsheet#spreadsheet.tabs),
                        Spreadsheet#spreadsheet.owner,
                        Spreadsheet#spreadsheet.access_policies
                    ]
                ),
                file:write(File, Data),
                file:close(File),
                ok
            catch
                error:Reason ->
                    {error, io:format("Failed to export spreadsheet: ~p", [Reason])}
            end
    end.

% Esporta in CSV l'intero foglio aspettando un tempo 'Timeout' 
to_csv_timeout(SpreadsheetName, Filename, Timeout) ->
    {Time, Result} = timer:tc(fun() -> csv_manage:to_csv(SpreadsheetName, Filename) end),
    if
        Time > Timeout * 1000 -> timeout;
        true -> Result
    end.

% Prende una lista di schede (tabs) e le formatta come stringa.
% Per ogni scheda chiama la funzione format_tab/1 per convertire la scheda in una stringa.
% Unisce tutte le stringhe delle schede in una sola stringa con la funzione lists:join/2, separando ogni stringa di scheda con una virgola.
format_tabs(Tabs) ->
    "[" ++ lists:join(",", lists:map(fun(Tab) -> format_tab(Tab) end, Tabs)) ++ "]".
% Formatta i dati di una singola scheda.
% Prende una scheda (che è una lista di righe) e per ogni riga nella scheda, chiama la funzione format_row/1 per convertire la riga in una stringa.
% Quindi, unisce tutte le stringhe delle righe in una sola stringa con la funzione lists:join/2, separando ogni stringa di riga con una virgola.
format_tab(Tab) ->
    "[" ++ lists:join(",", lists:map(fun(Row) -> format_row(Row) end, Tab)) ++ "]".
% Formatta i dati di una singola riga.
% Prende una riga (che è una lista di celle) e per ogni cella nella riga, chiama la funzione format_cell/1 per convertire la cella in una stringa.
% Quindi, unisce tutte le stringhe delle celle in una sola stringa con la funzione lists:join/2, separando ogni stringa di cella con una virgola.
format_row(Row) ->
    "[" ++ lists:join(",", lists:map(fun(Cell) -> format_cell(Cell) end, Row)) ++ "]".
% Prende una cella e la converte in una stringa.
% Se il valore della cella è undef, restituisce la stringa “undef”. Altrimenti, converte il valore della cella in una stringa utilizzando la funzione atom_to_list/1
format_cell(Cell) ->
    case Cell of
        undef -> "undef";
        _ when is_integer(Cell) -> integer_to_list(Cell);
        _ when is_list(Cell) -> "\"" ++ lists:flatten(Cell) ++ "\"";
        _ -> atom_to_list(Cell)
    end.

from_csv(Filename) ->
    ActualFilename =
        case lists:suffix(".csv", atom_to_list(Filename)) of
            true -> Filename;
            false -> atom_to_list(Filename) ++ ".csv"
        end,
    case file:read_file(ActualFilename) of
        {ok, Binary} ->
            [_, Line | _] = binary:split(Binary, <<"\n">>, [global]),
            SplittedData = string:split(binary_to_list(Line), ";"),
            [Name, Rest | _] = SplittedData,
            [Tabs, RestPid | _] = string:split(Rest, ";"),
            [Pid, Policy | _] = string:split(RestPid, ";"),
            Spreadsheet = #spreadsheet{
                name = list_to_atom(Name),
                tabs = convert_tabs(Tabs),
                owner = list_to_pid(Pid),
                access_policies = convert_policy(Policy)
            },
            mnesia:transaction(fun() -> mnesia:write(Spreadsheet) end);
        {error, Reason} ->
            {error, Reason}
    end.

from_csv_timeout(Filename, Timeout) -> 
    {Time, Result} = timer:tc(fun() -> csv_manage:from_csv(Filename) end),
    if
        Time > Timeout * 1000 -> timeout;
        true -> Result
    end.

convert_tabs(Tabs) ->
    {ok, Ts, _} = erl_scan:string(Tabs),
    {ok, Result} = erl_parse:parse_term(Ts ++ [{dot, 1} || element(1, lists:last(Ts)) =/= dot]),
    Result.

convert_policy(Policy) ->
    case Policy of
        "[]" ->
            [];
        _ ->
            StrippedString = string:trim(Policy, both, "[]"),
            TupleStrings = string:split(StrippedString, "},{", all),
            Result = lists:map(
                fun(TupleString) ->
                    StrippedTupleString = string:trim(TupleString, both, "{}"),
                    [PidString, ActionString] = string:split(StrippedTupleString, ",", all),
                    Pid = list_to_pid("<" ++ string:trim(PidString, both, "<> ") ++ ">"),
                    Action = list_to_atom(string:trim(ActionString, both, "<> ")),
                    {Pid, Action}
                end,
                TupleStrings
            ),
            Result
    end.
