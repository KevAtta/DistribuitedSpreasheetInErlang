-module(modifie_row).
-include("record.hrl").
-export([add_row/2, del_row/3]).

% Aggiunge una nuova riga alla tabella specificata nel foglio di calcolo
add_row(SpreadsheetName, TabNumber) ->
    try
        Fun = fun() ->
            Spreadsheet = mnesia:read({spreadsheet, SpreadsheetName}),
            case Spreadsheet of
                [] ->
                    {error, "Spreadsheet not found"};
                [#spreadsheet{tabs = Tabs} = S] ->
                    if
                        TabNumber =< length(Tabs) ->
                            Tab = lists:nth(TabNumber, Tabs),
                            RowLength =
                                case Tab of
                                    % Prende la lunghezza della prima riga della prima scheda
                                    [] -> length(hd(hd(Tabs)));
                                    _ -> length(hd(Tab))
                                end,
                            NewRow = new_row(RowLength),
                            NewTab = Tab ++ [NewRow],
                            NewTabs =
                                lists:sublist(Tabs, TabNumber - 1) ++ [NewTab] ++
                                    lists:nthtail(TabNumber, Tabs),
                            mnesia:write(S#spreadsheet{tabs = NewTabs}),
                            {ok, S#spreadsheet{tabs = NewTabs}};
                        true ->
                            {error, "Tab not found"}
                    end
            end
        end,
        mnesia:transaction(Fun)
    catch
        _:_ ->
            {error, "Failed to add row"}
    end.

% Crea una nuova riga di lunghezza M
new_row(NumCol) ->
    lists:map(fun(_) -> empty_cell() end, lists:seq(1, NumCol)).

% Crea una cella vuota
empty_cell() -> undef.

% Rimuove una riga specificata da una scheda specificata nel foglio di calcolo
del_row(SpreadsheetName, TabNumber, RowNumber) ->
    try
        Fun = fun() ->
            Spreadsheet = mnesia:read({spreadsheet, SpreadsheetName}),
            case Spreadsheet of
                [] ->
                    {error, "Spreadsheet not found"};
                [#spreadsheet{tabs = Tabs} = S] ->
                    if
                        TabNumber =< length(Tabs) ->
                            case lists:nth(TabNumber, Tabs) of
                                [] ->
                                    {error, "Tab empty"};
                                Tab ->
                                    if
                                        RowNumber =< length(Tab) ->
                                            NewTab =
                                                lists:sublist(Tab, RowNumber - 1) ++
                                                    lists:nthtail(RowNumber, Tab),
                                            NewTabs =
                                                lists:sublist(Tabs, TabNumber - 1) ++ [NewTab] ++
                                                    lists:nthtail(TabNumber, Tabs),
                                            mnesia:write(S#spreadsheet{tabs = NewTabs}),
                                            {ok, S#spreadsheet{tabs = NewTabs}};
                                        true ->
                                            {error, "Row not found"}
                                    end
                            end;
                        true ->
                            {error, "Tab not found"}
                    end
            end
        end,
        mnesia:transaction(Fun)
    catch
        _:_ ->
            {error, "Failed to delete row"}
    end.