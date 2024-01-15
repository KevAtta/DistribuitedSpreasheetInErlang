# Distributed spreadsheet

## Distributed Applications and Cloud Computing Project

### Students

- [Kevin Attarantato](https://github.com/KevAtta)
- [Giorgia Roselli](https://github.com/GioRoss)
---
<div align="center">
    <h4><b>OBJECTIVE</b></h4>
</div>
The project aims to create a distributed spreadsheet. The sheet is
a matrix of size NxM of K tabs, also the cells can contain any
primitive data type plus the undef value.

<div align="center">
    <h4><b>PROJECT CHOICES</b></h4>
</div>
The distributed DB was chosen to store the data of the spreadsheet(s).
Mnesia for different reasons:

- The first of all is due to the requirements, in particular the resistance of the knots
case of bankruptcy of others. Using the Mnesia system already has this feature
being implemented natively in the same system;
- Beyond this it was chosen to use the Mnesia system as it is native to
erlang, has high scalability and fault tolerance (as mentioned above) and
tables can be replicated across different nodes.

In order to identify the owner of the sheet, it was also decided to implement the spreadsheet:init() function which will spawn() the process which will have the possibility of possibly creating the sheet and carrying out operations on them. The table that contains the various sheets is made up of the fields of a record called 'spreadsheet' composed as follows:
```erlang
-record(spreadsheet, {name, tabs, owner, access_policies = []}).
```
- <b>name</b>: is the name of the sheet;
- <b>tabs</b>: are the various tabs;
- <b>owner</b>: is the owner of the sheet;
- <b>access_policies</b>: is a list of {Proc, AP}, where Proc will be the pid of a process and AP is the policy, which can be read or write.

Finally, for better organization and reuse, the code has been divided into multiple modules.

<div align="center">
    <h4><b>STARTING THE CODE AND CREATING SHEETS</b></h4>
</div>
For the purposes of the report and the description of the various functions, the tests were carried out by creating 3 nodes.
At the first start, position yourself in the 'root' folder, with the shell, where all the modules are present and create more nodes as in the next figure.

```erlang
erl -sname nome_host@localhost -setcookies psw
```

Then fill in all the modules one at a time with the commands:
```erlang
c(spreadsheet).
c(info).
c(csv_manage).
c(modifie_row).
```

At this point, make known the nodes on the network using the ```net_adm:ping()``` command:
```erlang
(node1@localhost)>net_adm:ping(node2@localhost).
(node1@localhost)>net_adm:ping(node3@localhost).
```

In this way, having received 'pong' as a response, a connection will be established between the nodes. If you receive 'pang' it means that something has gone wrong. Furthermore, it is not necessary to execute this command on all nodes but only on one as the others will then connect automatically.
At this point it is possible to print the list of nodes to be able to see that the nodes have actually been connected correctly:
```erlang
(node1@localhost)>nodes(). 
[node2@localhost, node3@localhost]
```
At this point create the Mnesia schema in each node created using the following command:
```erlang
(node1@localhost)> spreadsheet:start_mnesia().
```
And then, in any node of your choice, start the ```start_table()``` function as follows:
```erlang
(node1@localhost)> spreadsheet:start_table().
```
At this point it is possible to verify that the inMnesia table has been correctly created in all nodes via the ```observer:start()``` function. We will therefore obtain a similar situation:
![observer mnesia tables](https://github.com/KevAtta/ProgettoADCC/assets/48328145/abb13fd5-c2f8-420a-b8e6-7f3d44f5453d)
Now to create a new spreadsheet, you need to start the ``spreadsheet:init()`` function from one of the nodes, the ``init()`` function is defined as in the next snippet.
```erlang
init(RegisterName) ->
    Pid = spawn(spreadsheet, loop, []),
    register(RegisterName, Pid),
    Pid.
```
At this point the process using the message passing method will remain waiting for a receive, this example shows the commands to execute:
```erlang
(node1@localhost)> Pid = spreadsheet:init(pid).
(node1@localhost)> Pid ! {new_default, self(), sheet1}.
(node2@localhost)> register(node2, self()).
(node2@localhost)> {Pid, nmode1@localhost} ! {new_default, {node2, node2@localhost}, sheet2}.
```
<div align="center">
    <h4><b>IMPLEMENTATION</b></h4>
</div>
The functions inserted and usable via the message passing method are now listed. Following execution, it is possible to view the response of the message using the flush() command.
<div align="center">
    <h5><b>Creating a sheet with default dimensions:</b></h5>
</div>

```erlang
{new_default, From, SpreadsheetName}
(node1@localhost)> Pid ! {new_default, self(), sheet}.
```
- Create a sheet named 'table';
- If the sheet named 'table' already exists, it is not created and an error is returned;
- By default, a sheet is created containing two tabs measuring 3 x 2;
- If the function is successful, the sheet will return.
<div align="center">
    <h5><b>Creating a sheet with variable dimensions:</b></h5>
</div>

```erlang
{new, From, SpreadsheetName, NumRow, NumCol, NumTab}
(node1@localhost)> Pid ! {new, self(), sheet, 2, 2, 3}.
```
- Create a sheet named table;
- If the sheet already exists, it is not created and an error returns;
- If they are inserted instead of the indices and the number of tabs no values numeric, or negative, the sheet is not created and an error returns;
- If the function is successful, the sheet will return.
<div align="center">
    <h5><b>Reading a value within a sheet:</b></h5>
</div>

```erlang
{get, From, Spreadsheet, TabIndex, RowIndex, ColIndex}
(node1@localhost)> Pid ! {get, self(), sheet, 1, 2, 2}.
```
- Reads the value of the cell (RowIndex, ColIndex) that belongs to the sheet tab table;
- If invalid indexes are inserted, as in the previous case, the is not read value and returns an error;
- If you do not have read access to that sheet, the value e will not be read will return an error (only the owner of the sheet and those who have the read permissions repruned to AP);
- If you enter the name of a sheet that does not exist, the e value will not be read an error will return;
- There is also a version of the same function with the addition of a <b>timeout</b>. This value must be an integer value and quantifies seconds;
- If the function is successful the value will be returned.
<div align="center">
    <h5><b>Writing a value within a sheet:</b></h5>
</div>

```erlang
{set, From, SpreadsheetName, TabIndex, RowIndex, ColIndex, Value}
(node1@localhost)> Pid ! {set, self(), sheet, 1, 2, 2, test}.
```
- Writes a cell value (RowIndex, ColIndex) that belongs to the sheet tab
table;
- Value can be any primitive and undef value;
- If you do not have write access to that sheet, the value e will not be written will return an error (only the owner of the sheet and those who have the write permissions repruned to AP);
- If you enter the name of a sheet that does not exist, the e value will not be read an error will return;
- If invalid indexes are inserted, an error returns;
- There is also a version of the same function with the addition of a <b>timeout</b>. This value must be an integer value and quantifies seconds;
- If the function is successful, a Boolean will return.
<div align="center">
    <h5><b>Changing the sheet policy:</b></h5>
</div>

```erlang
{share, From, SpreadsheetName, AccessPolicies}
(node1@localhost)> Pid ! {share, self(), sheet, {<0.854.0>, write}}.
```
- The owner of the sheet can share the sheet for reading or writing with others processes;
- If the person trying to change the policy is not the owner, an error will be returned;
- If you insert a sheet that does not exist, an error will return;
- AccessPolicies is a list of {Proc, AP} where, Proc is a Pid and AP is the policy that it can take on the values of 'write' or 'read';
- If you do not enter a valid Pid in the AccessPolicies list, an error will be returned;
- If you try to insert an already existing process into the policies, an error will return;
- If the function is successful, a Boolean will return.
<div align="center">
    <h5><b>Print information on one sheet:</b></h5>
</div>

```erlang
{info, From, SpreadsheetName}
(node1@localhost)> Pid ! {info, self(), sheet}.
```
- Information is printed on the sheet such as:
    - Sheet name;
    - The number of cells per table (Ex. [4,6] means that the first tab is composed with four cells and the second tab with six cells);
    - The owner of the sheet;
    - The sheet policies.
- If the sheet does not exist an error will be returned.
<div align="center">
    <h5><b>Exporting the sheet to a .csv file</b></h5>
</div>

```erlang
{to_csv, From, SpreadsheetName, FileName}
(node1@localhost)> Pid ! {to_csv, self(), sheet, first_csv}.
```
- Export the sheet with the name 'FileName.csv' to csv;
- If you insert a sheet that does not exist, an error returns;
- The file will be generated in the same folder;
- There is also a version of the same function with the addition of a <b>timeout</b>. This value must be an integer value and quantifies seconds.
<div align="center">
    <h5><b>Importing the sheet from a .csv file</b></h5>
</div>

```erlang
{from_csv, From, FileName}
(node1@localhost)> Pid ! {from_csv, self(), first_csv}.
```
- Import a sheet from a csv file;
- There is also a version of the same function with the addition of a <b>timeout</b>. This value must be an integer value and quantifies seconds.
<div align="center">
    <h5><b>Adding a row in a specific sheet tab:</b></h5>
</div>

```erlang
{add_row, From, SpreadsheetName, TabNumber}
(node1@localhost)> Pid ! {add_row, self(), sheet, 2}.
```
- Adds a row to the end of the 'TabNumber' tab of the 'SpreadsheetName' sheet;
- If you insert a sheet that does not exist, the function will return an error;
- If you insert an invalid tab an error will return;
- If the function is successful it will return 'ok'.
<div align="center">
    <h5><b>Removing a row in a specific sheet tab:</b></h5>
</div>

```erlang
{delete_row, From, SpreadsheetName, TabNumber, RowNumber}
(node1@localhost)> Pid ! {delete_row, self(), sheet, 2, 3}.
```
- Removes the 'RowNumber' row from the 'TabNumber' tab of the 'SpreadsheetName' sheet;
- If you insert a sheet that does not exist, the function will return an error;
- If you insert an invalid tab an error will return;
- If you remove all rows of a tab and then the tab is empty, it will return an error;
- If you choose to delete a row that does not exist, an error will be returned;
- If the function is successful it will return 'ok'.

Each operation that will modify the various sheets, these changes will be displayed in each node that belongs to the Mnesia cluster. Below is an example: (in this example 5 different sheets were created).

![mnesia tables](https://github.com/KevAtta/ProgettoADCC/assets/48328145/66018931-279c-4195-a004-67b1d77d2211)
<div align="center">
    <h4><b>MEASUREMENTS</b></h4>
</div>

The following graphs were obtained by executing the ``get()`` and ``set()`` functions without timeout and increasing the size of the table from time to time while keeping the number of tabs fixed. The phyton libraries ``numpy`` and ``matplotlib.pylot`` were used to plot the graph.

![plot set fx](https://github.com/KevAtta/ProgettoADCC/assets/48328145/03dac249-2725-4eb6-8221-29f8475baac7)

![plot get fx](https://github.com/KevAtta/ProgettoADCC/assets/48328145/b6c3f78e-cfa4-4e16-acda-37394803c6df)
