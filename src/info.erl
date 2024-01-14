-module(info).
-include("record.hrl").
-export([info/1]).

info(SpreadsheetName) ->
    try
        {atomic, [Spreadsheet]} = mnesia:transaction(fun() -> mnesia:read({spreadsheet, SpreadsheetName}) end),
        Name = Spreadsheet#spreadsheet.name,
        Tabs = Spreadsheet#spreadsheet.tabs,
        Owner = Spreadsheet#spreadsheet.owner,
        AccessPolicies = Spreadsheet#spreadsheet.access_policies,
        NumCellsPerTab = lists:map(fun(Tab) -> length(lists:flatten(Tab)) end, Tabs),
        {spreadsheet_info, Name, NumCellsPerTab, Owner, AccessPolicies}
    catch
        _:_ ->
            {error, "Failed to get spreadsheet info"}
    end.
