-module(app_tool).

-export([main/1]).

-include("app_tool.hrl").

main ([Option]) ->
    try
        run(list_to_integer(Option))
    catch
        _ : {parse_error, R} ->
            ?MSG("~p in File ~p, Line ~p.~n", [R, ?GDF(file), ?GDF(line)]);
        _ : {generate_error, R} ->
            ?MSG("~p~n", [R]);
        _ : E ->
            ?MSG("{~p, ~p}~n", [E, erlang:get_stacktrace()])
    end;
main (_) ->
    usage().

usage () ->
    ?MSG("usage: ...\n"),
    halt(1).
    
run (1) ->
    ?MSG("Generate Protocol Files...~n"),
    init(),
    check_dir(get_default_dir(protocol)),
    Doc = parse_doc(file_find(get_default_dir(protocol), ".erl")),
    save_to_file(Doc, "Protocol.txt", false),
    generate_doc_file(parse_doc_complete(Doc)),
    clean(),
    ok;
run (2) ->
    ?MSG("Generate Database Files...~n"),
    generate_db_file(init_database());
run (_) ->
    run(1),
    run(2).
    
save_to_file (_, _, false) ->
    ok;
save_to_file (Data, File, _) ->
    {ok, Fd} = ?FOPEN(File, [write]),
    io:format(Fd, "~p", [Data]),
    ?FCLOSE(Fd).

check_dir (Dir) ->
    case filelib:is_dir(Dir) of
        false -> ?EXIT(Dir ++ " not a directory");
        true -> ok
    end.
    
get_default_dir (T) ->
    {T, Path} = lists:keyfind(T, 1, ?DIR_DEF),
    Path.
    
file_find (Dir, Suf) ->
    filelib:wildcard(Dir ++ "*" ++ Suf).
    
parse_doc (FileList) ->
    parse_file(FileList, #doc{}).
    
parse_doc_complete (Doc) when is_record(Doc, doc) ->
    Doc #doc{
        pro_list = lists:reverse(Doc #doc.pro_list)
    }.
    
parse_file ([], Doc) ->
    Doc;
parse_file ([File | Rest], Doc) ->
    parse_file(Rest, 
        Doc #doc{pro_list = [parse_file(File) | Doc #doc.pro_list]}
    ).
    
parse_file (File) ->
    init(),
    {ok, [Data]} = file:consult(File),
    Pro = parse_protocol(Data, #protocol{file = filename:basename(File)}),
    parse_file_complete(Pro).
    
parse_file_complete (Pro) when is_record(Pro, protocol) ->
    Pro #protocol{
        enum_list = lists:reverse(Pro #protocol.enum_list),
        list_list = lists:reverse(Pro #protocol.list_list),
        class_list = lists:reverse(Pro #protocol.class_list),
        action_list = lists:reverse(Pro #protocol.action_list)
    }.
    
parse_protocol (Data, Pro) when is_map(Data) ->
    #{name := Name, id := Id, action := Action, class := Class} = Data,
    Pro1 = Pro #protocol{name = ?A2L(Name), id = Id},
    Pro2 = parse_class(Class, parse_action(Action, Pro1)),
    Pro2 #protocol{enum_list = ?GDF(enum_list), list_list = ?GDF(list_list)}.
    
parse_action ([], Pro) ->
    Pro;
parse_action ([Data | Rest], Pro) ->
    #{name := Name, id := Id, in := In, out := Out} = Data,
        
    Act = #action{
        name = ?A2L(Name), 
        id = Id,
        field_in = parse_field_list(In),
        field_out = parse_field_list(Out)
    },
    
    parse_action(
        Rest, 
        Pro #protocol{
            action_list = [Act | Pro #protocol.action_list]
        }
    ).
    
parse_class ([], Pro) ->
    Pro;
parse_class ([Data | Rest], Pro) ->
    #{name := Name, field := Field} = Data,
    
    Class = #class{
        name = ?A2L(Name),
        field = parse_field_list(Field)
    },
    
    parse_class(
        Rest,
        Pro #protocol{
            class_list = [Class | Pro #protocol.class_list]
        }
    ).
    
parse_field_list (Data) ->
    parse_field_list_complete(parse_field_list(Data, [])).
    
parse_field_list_complete (List) when is_list(List) ->
    lists:reverse(List).
    
parse_field_list ([], FieldList) ->
    FieldList;
parse_field_list ([Field | Rest], FieldList) ->
    parse_field_list(Rest, [parse_field(Field) | FieldList]).
    
parse_field ({Name, byte}) ->
    #field{name = ?A2L(Name), type = byte};
parse_field ({Name, short}) ->
    #field{name = ?A2L(Name), type = short};
parse_field ({Name, int}) ->
    #field{name = ?A2L(Name), type = int};
parse_field ({Name, long}) ->
    #field{name = ?A2L(Name), type = long};
parse_field ({Name, string}) ->
    #field{name = ?A2L(Name), type = string};
parse_field ({Name, enum, Data}) ->
    ok = insert_enum(Data),
    #field{name = ?A2L(Name), type = enum};
parse_field ({Name, list, Data}) ->
    LName = get_list_name(Name),
    insert_list(LName, Data),
    #field{name = LName, type = list};
parse_field ({Name, class, Data}) ->
    #field{name = ?A2L(Name), type = class, class = ?A2L(Data)};
parse_field (_) ->
    exit(invalid_field).

init () ->
    ?SDF(list_counter, 0),
    ?SDF(enum_list, []),
    ?SDF(list_list, []).
    
clean () ->
    ?EDF(list_counter),
    ?EDF(enum_list),
    ?EDF(list_list).
    
insert_enum ([]) ->
    ok;
insert_enum ([Enum | Rest]) ->
    case ?GDF(enum_list) of
        [] ->
            ?SDF(enum_list, [#enum{name = ?A2L(Enum), val = 1}]);
        EL ->
            case lists:keyfind(?A2L(Enum), #enum.name, EL) of
                false ->
                    [Tl | _] = EL,

                    ?SDF(
                        enum_list, 
                        [#enum{name = ?A2L(Enum), val = Tl #enum.val + 1} | EL]
                    );
                _ ->
                    ok
            end
    end,
    
    insert_enum(Rest).
    
get_list_name (Name) when is_atom(Name) ->
    Id = ?SDF(list_counter, ?GDF(list_counter) + 1),
    ?A2L(Name) ++ "_" ++ ?I2L(Id);
get_list_name (Name) when is_list(Name) ->
    Id = ?SDF(list_counter, ?GDF(list_counter) + 1),
    Name ++ "_" ++ ?I2L(Id).
    
insert_list (Name, Data) when is_atom(Data) ->
    List = #list{name = Name, class = ?A2L(Data),
        field = [#field{name = Name, type = class, class = ?A2L(Data)}]
    },
    ?SDF(list_list, [List | ?GDF(list_list)]);
insert_list (Name, Data) when is_list(Data) ->
    List = #list{name = Name, field = parse_field_list(Data)},
    ?SDF(list_list, [List | ?GDF(list_list)]).
    
delete_dir_file (Dir, Suf) ->
    [ok = file:delete(F) || F <- file_find(Dir, Suf)].
    
field_desc (byte) ->
    {"8", "signed"};
field_desc (short) ->
    {"16", "signed"};
field_desc (int) ->
    {"32", "signed"};
field_desc (long) ->
    {"64", "signed"};
field_desc (enum) ->
    {"8", "unsigned"};
field_desc (_) ->
    nil.
    
generate_doc_file (Doc) ->
    generate_header(Doc),
    generate_out(Doc),
    generate_router(Doc).
    
generate_header (Doc) ->
    Dir = get_default_dir(header),
    ok = filelib:ensure_dir(Dir),
    delete_dir_file(Dir, ".hrl"),
    generate_header(Doc #doc.pro_list, Dir).
    
generate_header ([], _) ->
    ok;
generate_header ([Pro | Left], Dir) ->
    case Pro #protocol.enum_list of
        [] -> 
            generate_header(Left, Dir);
        _ ->
            {ok, Fd} = ?FOPEN(
                Dir ++ "api_" ++ Pro #protocol.name ++ ".hrl", 
                [write]
            ),

            write_header_file(Fd, Pro),
            ?FCLOSE(Fd),
            generate_header(Left, Dir)
    end.
    
write_header_file (Fd, Pro) ->
    [
        ?FWRITE(Fd, "-define(" ++ ?TUP(E #enum.name) ++ ", " ++ 
                ?I2L(E #enum.val) ++ ").\n")
        || E <- Pro #protocol.enum_list
    ].
    
generate_out (Doc) ->
    Dir = get_default_dir(out),
    ok = filelib:ensure_dir(Dir),
    delete_dir_file(Dir, ".erl"),
    generate_out(Doc #doc.pro_list, Dir),
    generate_class(Doc, Dir),
    generate_list(Doc, Dir).
    
generate_out ([], _) ->
    ok;
generate_out ([Pro | Left], Dir) ->
    {ok, Fd} = ?FOPEN(
        Dir ++ "api_" ++ Pro #protocol.name ++ "_out.erl",
        [write]
    ),
    
    write_out_file(Fd, Pro),
    ?FCLOSE(Fd),
    generate_out(Left, Dir).
    
write_out_file (Fd, Pro) ->
    ?FWRITE(Fd, 
        "-module(api_" ++ Pro #protocol.name ++ "_out).\n\n"
            ++ "-export([ "
    ),
    
    [
        ?FWRITE(Fd, "\n\t" ++ A #action.name ++ "/1,")
        || A <- Pro #protocol.action_list
    ],
    
    ?FWRITE(Fd, "\n]).", -1),
    
    [
        begin
            ?FWRITE(Fd, "\n\n" ++ A #action.name ++ " ({ "),
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\n}) ->\n", -1),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\tapi_base_list_out:item_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            ?FWRITE(Fd, "\t" ++ FN 
                                ++ "_Bin = api_base_class_out:type_to_bin_" 
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\t<<\n\t\t" ++ ?I2L(Pro #protocol.id) 
                ++ ":8/unsigned,\n\t\t" ++ ?I2L(A #action.id) ++ ":8/unsigned,"
            ),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- A #action.field_out
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || A <- Pro #protocol.action_list
    ].
    
generate_class (Doc, Dir) ->
    generate_class_in(Doc, Dir),
    generate_class_out(Doc, Dir).
    
generate_class_in (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_class_in.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_class_in).\n\n-compile(export_all)."),
    write_class_in(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_class_in ([], _) ->
    ok;
write_class_in ([Pro | Left], Fd) ->
    [
        begin
            PN = Pro #protocol.name,
            
            ?FWRITE(Fd, "\n\ntype_parse_" 
                ++ class_name(C #class.name, PN) ++ " (_Args0) ->"
            ),
            
            AC = write_class_field(Fd, C, PN),
            write_class_item(Fd, C),
            
            ?FWRITE(Fd, "\n\t{_" ++ format_name(C #class.name)
                ++ "Item, _Args" ++ ?I2L(AC) ++ "}."
            )
        end
        || C <- Pro #protocol.class_list
    ],
    
    write_class_in(Left, Fd).
    
write_class_field (Fd, C, PN) ->
    write_class_field(C #class.field, [], 0, Fd, PN).

write_class_field ([], [], AC, _, _) ->
    AC;
write_class_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, "_Args" ++ ?I2L(AC+1)
        ++ "/binary>> = _Args" ++ ?I2L(AC) ++ ","
    ),
    
    AC + 1;
write_class_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, 
                FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = type_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_class_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = api_base_list_in:list_parse_" 
                ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_class_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_class_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_class_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_class_item (Fd, C) ->
    ?FWRITE(Fd, "\n\t_" ++ format_name(C #class.name) ++ "Item = {"),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- C #class.field
    ],
      
    ?FWRITE(Fd, "},", -2).
    
generate_class_out (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_class_out.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_class_out).\n\n-compile(export_all)."),
    write_class_out(Doc #doc.pro_list, Fd, Doc),
    ?FCLOSE(Fd).
    
write_class_out ([], _, _) ->
    ok;
write_class_out ([Pro | Left], Fd, Doc) ->
    [
        begin
            CN = class_name(C #class.name, Pro #protocol.name),
            ?FWRITE(Fd, "\n\ntype_to_bin_" ++ CN ++ " ({ "),
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\n}) ->\n", -1),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\tapi_base_list_out:item_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            check_class(F #field.class, Pro #protocol.name, Doc),
                            
                            ?FWRITE(Fd, "\t" ++ FN ++ "_Bin = type_to_bin_"
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\t<< "),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- C #class.field
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || C <- Pro #protocol.class_list
    ],

    write_class_out(Left, Fd, Doc).
    
check_class (CN, PN, Doc) ->
    {NPN, NCN} = case re:split(CN, "\\.") of
        [N] -> 
            {PN, ?B2L(N)};
        [P, N] -> 
            {?B2L(P), ?B2L(N)};
        _ -> 
            ?EXITG("Class name error")
    end,
    
    case lists:keyfind(NPN, #protocol.name, Doc #doc.pro_list) of
        false ->
            ?EXITG("Protocol " ++ NPN ++ " not define");
        Pro ->
            case lists:keyfind(NCN, #class.name, Pro #protocol.class_list) of
                false -> 
                    ?EXITG("Class " ++ CN ++ " not define");
                _ ->
                    ok
            end
    end.
    
generate_list (Doc, Dir) ->
    generate_list_in(Doc, Dir),
    generate_list_out(Doc, Dir).
    
generate_list_in (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_list_in.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_list_in).\n\n-compile(export_all)."),
    write_list_in(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_list_in ([], _) ->
    ok;
write_list_in ([Pro | Left], Fd) ->
    [
        begin
            PN = Pro #protocol.name,
            
            ?FWRITE(Fd, "\n\nlist_parse_" ++ PN ++ "_" ++ L #list.name  
                ++ " (0, _Args, _Result) ->\n\t{_Result, _Args};\n"
                ++ "list_parse_" ++ PN ++ "_" ++ L #list.name  
                ++ " (_Count, _Args0, _Result) ->"
            ),
            
            AC = write_list_field(Fd, L, PN),
            write_list_item(Fd, L),
            
            ?FWRITE(Fd, "\n\tlist_parse_" ++ PN ++ "_" ++ L #list.name
                ++ "(_Count - 1, _Args" ++ ?I2L(AC) ++ ", [_"
                ++ format_name(L #list.name) ++ "Item | _Result])."
            )
        end
        || L <- Pro #protocol.list_list
    ],
    
    write_list_in(Left, Fd).
    
write_list_field (Fd, L, PN) ->
    write_list_field(L #list.field, [], 0, Fd, PN).

write_list_field ([], [], AC, _, _) ->
    AC;
write_list_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, "_Args" ++ ?I2L(AC+1) 
        ++ "/binary>> = _Args" ++ ?I2L(AC) ++ ","
    ),
    
    AC + 1;
write_list_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, 
                FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = api_base_class_in:type_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_list_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = list_parse_" ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_list_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_list_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_list_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_list_item (Fd, L) ->
    ?FWRITE(Fd, "\n\t_" ++ format_name(L #list.name) ++ "Item = {"),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- L #list.field
    ],
      
    ?FWRITE(Fd, "},", -2).
    
generate_list_out (Doc, Dir) ->
    {ok, Fd} = ?FOPEN(Dir ++ "api_base_list_out.erl", [write]),
    ?FWRITE(Fd, "-module(api_base_list_out).\n\n-compile(export_all)."),
    write_list_out(Doc #doc.pro_list, Fd),
    ?FCLOSE(Fd).
    
write_list_out ([], _) ->
    ok;
write_list_out ([Pro | Left], Fd) ->
    [
        begin
            case L #list.class of
                [] ->
                    ?FWRITE(Fd, "\n\nitem_to_bin_" 
                        ++ Pro #protocol.name ++ "_" ++ L #list.name ++ " ({ "
                    );
                _ ->
                    ?FWRITE(Fd, "\n\nitem_to_bin_" 
                        ++ Pro #protocol.name ++ "_" ++ L #list.name ++ " ( "
                    )
            end,
            
            [
                ?FWRITE(Fd, "\n\t" ++ format_name(F #field.name) ++ ",")
                || F <- L #list.field
            ],
            
            case L #list.class of
                [] ->
                    ?FWRITE(Fd, "\n}) ->\n", -1);
                _ ->
                    ?FWRITE(Fd, "\n) ->\n", -1)
            end,
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\tBinList_" ++ FN
                                ++ " = [\n\t\titem_to_bin_" 
                                ++ Pro #protocol.name ++ "_"
                                ++ F #field.name ++ "(" ++ FN ++ "_Item) || "
                                ++ FN ++ "_Item <- " ++ FN ++ "\n\t],\n\n\t"
                                ++ FN ++ "_Len = length(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ " = list_to_binary("
                                ++ "BinList_" ++ FN ++ "),\n\n"
                            );
                        string ->
                            ?FWRITE(Fd, "\tBin_" ++ FN
                                ++ " = list_to_binary(" ++ FN ++ "),\n\t"
                                ++ "Bin_" ++ FN ++ "_Len = size("
                                ++ "Bin_" ++ FN ++ "),\n\n"
                            );
                        class ->
                            ?FWRITE(Fd, "\t" ++ FN 
                                ++ "_Bin = api_base_class_out:type_to_bin_"
                                ++ class_name(F #field.class, Pro #protocol.name) 
                                ++ "(" ++ FN ++ "),\n\n"
                            );
                        _ ->
                            ok
                    end
                end
                || F <- L #list.field
            ],
            
            ?FWRITE(Fd, "\t<< "),
            
            [
                begin
                    FN = format_name(F #field.name),
                    
                    case F #field.type of
                        list ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        string ->
                            ?FWRITE(Fd, "\n\t\tBin_" ++ FN ++ "_Len:16/unsigned, "
                                ++ "Bin_" ++ FN ++ "/binary,"
                            );
                        class ->
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ "_Bin/binary,");
                        FT ->
                            {FLen, FDes} = field_desc(FT),
                            
                            ?FWRITE(Fd, "\n\t\t" ++ FN ++ ":"
                                ++ FLen ++ "/" ++ FDes ++ ","
                            )
                    end
                end
                || F <- L #list.field
            ],
            
            ?FWRITE(Fd, "\n\t>>.", -1)
        end
        || L <- Pro #protocol.list_list
    ],
    
    write_list_out(Left, Fd).
    
generate_router (Doc) ->
    Dir = get_default_dir(out),
    ok = filelib:ensure_dir(Dir),
    {ok, Fd} = ?FOPEN(Dir ++ "game_router.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_router).\n\n-export([route_request/2])."
        "\n\nroute_request "
        "(<<Module:8/unsigned, Action:8/unsigned, Args/binary>>, State) ->"
        "\n\t{_M, _A, NewState} = route_request(Module, Action, Args, State),"
        "\n\tNewState."
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\nroute_request (" ++ ?I2L(Pro #protocol.id) 
                ++ ", _Action, _Args0, _State) ->\n\tcase _Action of"
            ),
            
            [
                begin
                    ?FWRITE(Fd, "\n\t\t" ++ ?I2L(A #action.id) ++ " ->"),
                    
                    case length(A #action.field_in) of
                        0 ->
                            ok;
                        _ ->
                            write_action_field(Fd, A, Pro #protocol.name)
                    end,
                    
                    write_action_api(Fd, A, Pro #protocol.name),
                    
                    ?FWRITE(Fd, "\n\t\t\t{" ++ Pro #protocol.name 
                        ++ ", " ++ A #action.name ++ ", NewState};"
                    )
                end
                || A <- Pro #protocol.action_list
            ],
            
            ?FWRITE(Fd, "\n\tend;", -1)
        end
        || Pro <- Doc #doc.pro_list
    ],
    
    ?FWRITE(Fd, ".", -1),
    ?FCLOSE(Fd).
    
write_action_field (Fd, A, PN) ->
    write_action_field(A #action.field_in, [], 0, Fd, PN).

write_action_field ([], [], _, _, _) ->
    ok;
write_action_field ([], _, AC, Fd, _) ->
    ?FWRITE(Fd, ">> = _Args" ++ ?I2L(AC) ++ ",", -2);
write_action_field ([F | LF], PF, AC, Fd, PN) ->
    case PF of
        [] -> ?FWRITE(Fd, "\n\t\t\t<<");
        _ -> ok
    end,
        
    FN = format_name(F #field.name),
    
    case F #field.type of
        class ->
            ?FWRITE(Fd, FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","),
            
            ?FWRITE(Fd, "\n\t\t\t{" ++ FN ++ ", _Args" 
                ++ ?I2L(AC+1) ++ "} = api_base_class_in:type_parse_" 
                ++ class_name(F #field.class, PN) ++ "("
                ++ FN ++ "Bin),"
            ),
            
            write_action_field(LF, [], AC + 1, Fd, PN);
        list ->
            ?FWRITE(Fd, "Size_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ "Bin/binary>> = _Args" ++ ?I2L(AC) ++ ","
            ),
            
            ?FWRITE(Fd, "\n\t\t\t{" ++ FN ++ ", _Args" ++ ?I2L(AC+1)
                ++ "} = api_base_list_in:list_parse_" 
                ++ PN ++ "_" ++ F #field.name 
                ++ "(Size_" ++ FN ++ ", " ++ FN ++ "Bin, []),"
            ),
            
            write_action_field(LF, [], AC + 1, Fd, PN);
        string ->
            ?FWRITE(Fd, "Len_" ++ FN ++ ":16/unsigned, "
                ++ FN ++ ":" ++ "Len_" ++ FN ++ "/binary, "
            ),
            
            write_action_field(LF, [F | PF], AC, Fd, PN);
        FT ->
            {FLen, FDes} = field_desc(FT),
                            
            ?FWRITE(Fd, FN ++ ":" 
                ++ FLen ++ "/" ++ FDes ++ ", "
            ),
            
            write_action_field(LF, [F | PF], AC, Fd, PN)
    end.
    
write_action_api (Fd, A, PN) ->
    ?FWRITE(Fd, "\n\t\t\tNewState = api_" 
        ++ PN ++ ":" ++ A #action.name ++ "("
    ),
    
    [
        begin
            FN = format_name(F #field.name),
            
            case F #field.type of
                string ->
                    ?FWRITE(Fd, "binary_to_list(" ++ FN ++ "), ");
                _ ->
                    ?FWRITE(Fd, FN ++ ", ")
            end
        end
        || F <- A #action.field_in
    ],
      
    ?FWRITE(Fd, "_State),").
        
format_name ([]) ->
    [];
format_name (FN) ->
    [C | L] = FN,
    
    case (C >= $a andalso C =< $z) of
        true -> 
            format_name(L, [C - 32], C);
        _ -> 
            case (C >= $A andalso C =< $Z) of
                true ->
                    format_name(L, [C], C);
                _ ->
                    ?EXITG("Field name " ++ FN ++ " error")
            end
    end.
        
format_name ([], FN, _) ->
    lists:reverse(FN);
format_name ([$_ | L], FN, _) ->
    format_name(L, FN, $_);
format_name ([C | L], FN, $_) ->
    case (C >= $a andalso C =< $z) of
        true -> 
            format_name(L, [C - 32 | FN], C);
        _ ->
            format_name(L, [C | FN], C)
    end;
format_name ([C | L], FN, _) ->
    format_name(L, [C | FN], C).
    
class_name (CN, PN) ->
    case re:split(CN, "\\.") of
        [N] -> 
            PN ++ "_" ++ ?B2L(N);
        [P, N] -> 
            ?B2L(P) ++ "_" ++ ?B2L(N);
        _ -> 
            ?EXITG("Class name error")
    end.
    
init_database () ->
    mysql:start_link(db, ?DB_SRV, ?DB_USR, ?DB_PSW, "information_schema"),
    mysql:fetch(db, <<"SET NAMES 'utf8'">>),
    
    {data, R} = mysql:fetch(db, 
        <<"SELECT `TABLE_NAME` FROM `TABLES` WHERE `TABLE_SCHEMA` = '", 
            ?GAMEDB, "'">>
    ),
    
    Rows = lib_mysql:get_rows(R),
    Db = init_table_column(init_table_name(Rows)),
    save_to_file(Db, "DB.txt", false),
    Db.
    
init_table_name (Rows) ->
    init_table_name(Rows, #db{}).
    
init_table_name ([], Db) ->
    Db;
init_table_name ([R | L], Db) ->
    case lists:keyfind(<<"TABLE_NAME">>, 1, R) of
        {<<"TABLE_NAME">>, <<"db_version">>} ->
             init_table_name(L, Db);
        {<<"TABLE_NAME">>, N} ->
            init_table_name(L,
                Db #db{table_list = 
                    [#table{name = ?B2L(N)} | Db #db.table_list]}
            );
        _ ->
            init_table_name(L, Db)
    end.
    
init_table_column (Db) ->
    init_table_column(Db #db.table_list, #db{}).
      
init_table_column ([], Db) ->
    Db;
init_table_column ([T | L], Db) ->
    {data, R} = mysql:fetch(db,
        ?L2B("SELECT `COLUMN_NAME`, `COLUMN_DEFAULT`, `DATA_TYPE`, `COLUMN_KEY` ,"
            " `EXTRA`, `COLUMN_COMMENT` FROM `COLUMNS` WHERE `TABLE_SCHEMA` = '"
            ++ ?GAMEDB ++ "' AND `TABLE_NAME` = '" ++ T #table.name ++ "'")
    ),

    Rows = lib_mysql:get_rows(R),

    init_table_column(L, Db #db{
        table_list = [set_table_column_complete(
            set_table_column(Rows, #table{name = T #table.name}))
            | Db #db.table_list]
    }).
     
set_table_column ([], T) ->
    T;
set_table_column ([CL | L], T) ->
    set_table_column(L, T #table{
        column = [init_column_field(CL) | T #table.column]}
    ).
    
set_table_column_complete (T) ->
    T #table{column = lists:reverse(T #table.column)}.
    
init_column_field (CL) ->
    init_column_field(CL, #column{}).
    
init_column_field ([], C) ->
    C;
init_column_field ([{FT, FV} | L], C) ->
    case FT of
        <<"COLUMN_NAME">> ->
            init_column_field(L, C #column{name = field_val(FV)});
        <<"COLUMN_DEFAULT">> ->
            init_column_field(L, C #column{val = field_val(FV)});
        <<"DATA_TYPE">> ->
            init_column_field(L, C #column{type = field_val(FV)});
        <<"COLUMN_KEY">> ->
            init_column_field(L, C #column{key = field_val(FV)});
        <<"EXTRA">> ->
            init_column_field(L, C #column{extra = field_val(FV)});
        <<"COLUMN_COMMENT">> ->
            init_column_field(L, C #column{comment = field_val(FV)})
    end.
    
field_val (V) when is_binary(V) ->
    ?B2L(V);
field_val (V) ->
    V.
    
generate_db_file (Db) ->
    generate_db_header(Db),
    generate_db_init(Db),
    generate_db_save(Db).
    
generate_db_header (Db) ->
    Dir = get_default_dir(header),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db.hrl", [write]),
    write_db_macro(?MACRO_LIST, Fd),
    write_db_record(Db #db.table_list, Fd),
    ?FCLOSE(Fd).
    
write_db_record ([], _) ->
    ok;
write_db_record ([T | L], Fd) ->
    ?FWRITE(Fd, "-record(" ++ T #table.name ++ ", {\n\trow_key, "),
    
    [
        begin
            ?FWRITE(Fd, "\n\t" ++ C #column.name),
            
            case C #column.val of
                undefined -> 
                    ?FWRITE(Fd, " = null,");
                [] ->
                    ?FWRITE(Fd, " = \"\",");
                V ->
                    ?FWRITE(Fd, " = " ++ V ++ ",")
            end,
            
            case C #column.comment of
                [] ->
                    ok;
                Com ->?FWRITE(Fd, " %% " ++ Com)
            end
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\trow_ver = 0\n}).\n-record(pk_"
        ++ T #table.name ++ ", { "
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\t" ++ C #column.name ++ ",")
        end
        || C <- table_primary_key(T)
    ],
    
    ?FWRITE(Fd, "\n}).\n\n", -1),
    write_db_record(L, Fd).
    
write_db_macro ([], _) ->
    ok;
write_db_macro ([M | L], Fd) ->
    mysql:fetch(db, <<"USE ", ?GAMEDB>>),
    
    {data, R} = mysql:fetch(db, ?L2B("SELECT `"
        ++ M #macro.id ++ "`, `" ++ M #macro.sign ++ "`, `"
        ++ M #macro.name ++ "` FROM " ++ M #macro.table)
    ),
    
    Rows = lib_mysql:get_rows(R),
    
    [
        begin
            ?FWRITE(Fd, "-define(" ++ M #macro.prefix
                ++ ?TUP(?B2L(lib_mysql:get_field(Row, <<"sign">>)))
                ++ ", " ++ ?I2L(lib_mysql:get_field(Row, <<"id">>))
                ++ "). %% " 
                ++ ?B2L(lib_mysql:get_field(Row, <<"name">>)) ++ "\n"
            )
        end
        || Row <- Rows
    ],
    
    ?FWRITE(Fd, "\n"),
    write_db_macro(L, Fd).
    
generate_db_init (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_init.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_db_init).\n\n-export([\n\t"
        "init/0,\n\twait_for_loaded/0\n]).\n\n"
        "-include(\"gen/game_db.hrl\").\n\n"
        "init () ->\n\tregister(game_db, self()),\n\t"
        "mysql:fetch(gamedb, [<<\"SET FOREIGN_KEY_CHECKS=0;\">>]),\n\t"
        "ets:new(auto_increment, [public, set, named_table]),\n"
    ),
    
    [
        begin
            ?FWRITE(Fd, "\n\tinit(" ++ T #table.name ++ "),")
        end
        ||
        T <- Db #db.table_list
    ],
    
    ?FWRITE(Fd, "\n\n\tproc_lib:init_ack({ok, self()}),\n\tloop()."
        "\n\nloop () ->\n\treceive\n\t\t{is_loaded, Pid} -> Pid "
        "! yes, loop();\n\t\t_ -> loop()\n\tend."
        "\n\nwait_for_loaded () ->\n\tgame_db ! {is_loaded, self()},"
        "\n\treceive yes -> ok end."
    ),
    
    write_db_init(Db, Fd),
    write_db_load(Db, Fd),
    ?FCLOSE(Fd).
    
write_db_init (Db, Fd) ->
    write_table_init(Db #db.table_list, Fd).
    
write_table_init ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_table_init ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\ninit (" ++ T #table.name ++ ") ->"),
    
    case table_auto_increment(T) of
        {true, C} ->
            ?FWRITE(Fd, "\n\t{data, AutoIncResultId} = mysql:fetch("
                "gamedb, [<<\n\t\t\"SELECT IFNULL(MAX(`"
                ++ C #column.name ++ "`), 0) AS `max_id` FROM `"
                ++ T #table.name ++ "`;\"\n\t>>]),"
            ),
            
            ?FWRITE(Fd, "\n\n\t[AutoIncResult] = lib_mysql:get_rows("
                "AutoIncResultId),\n\t{<<\"max_id\">>, AutoIncStart} = "
                "lists:keyfind(<<\"max_id\">>, 1, AutoIncResult),"
                "\n\ttrue = ets:insert_new(auto_increment, {{"
                ++ T #table.name ++ ", " ++ C #column.name 
                ++ "}, AutoIncStart}),"
            );
        _ ->
            ok
    end,
    
    case table_need_load(T) of
        true ->
            case table_need_split(T) of
                {true, _} ->
                    ?FWRITE(Fd, "\n\t[ets:new(list_to_atom(\"t_" 
                        ++ T #table.name ++ "_\" ++ integer_to_list(I)), "
                        "[public, set, name_table, {keypos, 2}]) || "
                        "I <- lists:seq(0, 99)],"
                    );
                _ ->
                    ?FWRITE(Fd, "\n\tets:new(t_" ++ T #table.name
                        ++ ", [public, set, name_table, {keypos, 2}]),"
                    )
            end,
            
            ?FWRITE(Fd, "\n\tload(" ++ T #table.name ++ "),");
        _ ->
            ok
    end,
    
    ?FWRITE(Fd, "\n\tok;"),
    write_table_init(L, Fd).
    
write_db_load (Db, Fd) ->
    write_table_load(
        [T || T <- Db #db.table_list, table_need_load(T)],
        Fd
    ).
    
write_table_load ([], Fd) ->
    ?FWRITE(Fd, ".", -1);
write_table_load ([T | L], Fd) ->
    ?FWRITE(Fd, "\n\nload (" ++ T #table.name ++ ") ->"
        "\n\t{data, [NumResultId]} = mysql:fetch(game_db, [<<\""
        "SELECT COUNT(1) AS `num` FROM `" ++ T #table.name
        ++ "`\">>]),\n\t{<<\"num\">>, RecordNumber} = "
        "lists:keyfind(<<\"num\">>, 1, lib_mysql:get_rows("
        "NumResultId)),"
    ),
    
    ?FWRITE(Fd, "\n\n\tlists:foreach(fun(Page) ->\n\t\t"
        "Sql = \"SELECT * FROM `" ++ T #table.name ++ "` LIMIT \""
        " ++ integer_to_list((Page - 1) * 100000) ++ \", 100000\",\n\t\t"
        "{data, ResultId} = mysql:fetch(gamedb, [list_to_binary(Sql)]),"
        "\n\t\tRows = lib_mysql:get_rows(ResultId),\n\n\t\t"
    ),
    
    ?FWRITE(Fd, "lists:foreach(\n\t\t\tfun(Row) ->"),
    
    [
        begin
            ?FWRITE(Fd, "\n\t\t\t\t{<<\"" ++ C #column.name ++ "\">>, "
                ++ format_name(C #column.name) ++ "} = lists:keyfind(<<\""
                ++ C #column.name ++ "\">>, 1, Row),"
            )
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\n\t\t\t\tRecord = #" ++ T #table.name ++ "{"
        ++ "\n\t\t\t\t\trow_key = {"
    ),
    
    [
        begin
            case C #column.type of
                "varchar" ->
                    ?FWRITE(Fd, "lib_mysql:mysql_binary_to_list("
                        ++ format_name(C #column.name) ++ "), "
                    );
                _ ->
                    ?FWRITE(Fd, format_name(C #column.name) ++ ", ")
            end
        end
        || C <- table_primary_key(T)
    ],
    
    ?FWRITE(Fd, "},", -2),
    
    [
        begin
            ?FWRITE(Fd, "\n\t\t\t\t\t" ++ C #column.name ++ " = "),

            case C #column.type of
                "varchar" ->
                    ?FWRITE(Fd, "lib_mysql:mysql_binary_to_list("
                        ++ format_name(C #column.name) ++ "),"
                    );
                _ ->
                    ?FWRITE(Fd, format_name(C #column.name) ++ ",")
            end
        end
        || C <- T #table.column
    ],
    
    ?FWRITE(Fd, "\n\t\t\t\t},", -1),
    
    case table_need_split(T) of
        {true, SC} ->
            ?FWRITE(Fd, "\n\n\t\t\t\tTabId = integer_to_list(("
                "Record #" ++ T #table.name ++ "."
                ++ SC #column.name ++ ") rem 100),\n\t\t\t\t"
                "EtsTab = list_to_atom(\"t_" ++ T #table.name
                ++ "_\" ++ TabId),\n\t\t\t\t"
                "ets:insert(EtsTab, Record)"
            );
        _ ->
            ?FWRITE(Fd, "\n\n\t\t\t\tets:insert(t_" 
                ++ T #table.name ++ ", Record)"
            )
    end,
    
    ?FWRITE(Fd, "\n\t\t\tend,\n\t\t\tRows\n\t\t) end,\n\t\t"
        "lists:seq(1, lib_misc:ceil(RecordNumber / 100000))\n\t);"
    ),
    
    write_table_load(L, Fd).
    
generate_db_save (Db) ->
    Dir = get_default_dir(out),
    {ok, Fd} = ?FOPEN(Dir ++ "game_db_save.erl", [write]),
    
    ?FWRITE(Fd, "-module(game_db_save).\n\n-export([run/6]).\n\n"
        "-include(\"gen/game_db.hrl\").\n\n"
        "run (Host, Port, User, Password, Database, OnlyPlayerTable) ->\n"
        "\t{ok, Pid} = mysql_conn:start(Host, Port, User, Password, Database, "
        "fun(_, M, A) -> io:format(M, A) end),\n\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET NAMES utf8 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET SQL_MODE=''*/;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS,"
        "UNIQUE_CHECKS=0 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40014 SET @OLD_FOREIGN_KEY_CHECKS="
        "@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40101 SET @OLD_SQL_MODE=@@"
        "SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;\\n\">>], self()),\n"
        "\tmysql_conn:fetch(Pid, [<<\"/*!40111 SET @OLD_SQL_NOTES=@@"
        "SQL_NOTES, SQL_NOTES=0 */;\\n\\n\">>], self()),\n\n"
    ),
    
    write_save_dump(Db #db.table_list, Fd),
    ?FWRITE(Fd, "\n\tmysql_conn:stop(Pid),\n\tok."),
    write_table_dump(Db #db.table_list, Fd),
    
    ?FWRITE(Fd, "\n\nlst_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nlst_to_bin (List) ->\n\tList2 = escape_str(List, []),"
        "\n\tBin = list_to_binary(List2),"
        "\n\t<<\"'\", Bin/binary, \"'\">>."
        "\n\nint_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nint_to_bin (Value) ->\n\tlist_to_binary(integer_to_list(Value))."
        "\n\nrel_to_bin (null) ->\n\t<<\"NULL\">>;"
        "\nrel_to_bin (Value) when is_integer(Value) ->"
        "\n\tlist_to_binary(integer_to_list(Value));"
        "\nrel_to_bin (Value) ->\n\tlist_to_binary(float_to_list(Value))."
        "\n\nescape_str ([], Result) ->\n\tlists:reverse(Result);"
        "\nescape_str ([$' | String], Result) ->"
        "\n\tescape_str(String, [$' | [$\\\\ | Result]]);"
        "\nescape_str ([$\" | String], Result) ->"
        "\n\tescape_str(String, [$\" | [$\\\\ | Result]]);"
        "\nescape_str ([$\\\\ | String], Result) ->"
        "\n\tescape_str(String, [$\\ | [$\\\\ | Result]]);"
        "\nescape_str ([$\\n | String], Result) ->"
        "\n\tescape_str(String, [$n | [$\\\\ | Result]]);"
        "\nescape_str ([$\\r | String], Result) ->"
        "\n\tescape_str(String, [$r | [$\\\\ | Result]]);"
        "\nescape_str ([Char | String], Result) ->"
        "\n\tescape_str(String, [Char | Result])."
    ),
    
    ?FCLOSE(Fd).
    
write_save_dump ([], _) ->
    ok;
write_save_dump ([T | L], Fd) ->
    case re:run(T #table.name, "^player_") of
        {match, _} ->
            ?FWRITE(Fd, "\tdump_" ++ T #table.name ++ "(Pid),\n");
        _ ->
            ?FWRITE(Fd, "\tif OnlyPlayerTable -> ok; true -> dump_"
                ++ T #table.name ++ "(Pid) end,\n"
            )
    end,
    
    write_save_dump(L, Fd).
    
write_table_dump ([], _) ->
    ok;
write_table_dump ([T | L], Fd) ->
    TableName = T #table.name,
    
    ?FWRITE(Fd, "\n\ndump_" ++ T #table.name ++ " (Pid) ->" 
        "\n\tmysql_conn:fetch(Pid, [<<\"DELETE FROM `" ++ 
        TableName ++ "`;\\n\\n\">>], self()),"
    ),
 
    case table_need_split(T) of
        {true, _} ->
            ?FWRITE(Fd, "\n\tSize = lists:foldl(fun(I, S)-> S + "
                "ets:info(list_to_atom(\"t_" ++ TableName ++ "_\""
                " ++ integer_to_list(I)), size) end, 0, lists:seq(0, 99)),"
            ),
            
            ?FWRITE(Fd, "\n\n\tlists:foldl(fun(I, {S1, N1, L1}) ->"
                "\n\t\tets:foldl(fun(Record, {S, N, L}) ->"
            ),
            
            lists:foreach(
                fun(C) ->
                    OCN = C #column.name,
                    CN = format_name(OCN),
                    TS = get_trans_by_column(C),
                    
                    ?FWRITE(Fd, "\n\t\t\t_" ++ CN ++ " = " ++ TS ++
                        "(Record #" ++ TableName ++ "." ++ OCN ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    CN = format_name(C #column.name),
                    ?FWRITE(Fd, "\n\t\t\t\t_" ++ CN ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\t\tLast/binary\n\t\t\t>> | L],"
                "\n\n\t\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\t\tmysql_conn:fetch(Pid, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    OCN = C #column.name,
                    ?FWRITE(Fd, "\n\t\t\t\t\t\"`" ++ OCN ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t\t{S + 1, 0, []};\n\t\t\ttrue ->"
                "\n\t\t\t\t{S + 1, N + 1, L2}\n\t\t\tend"
                "\n\t\tend, {S1, N1, L1}, list_to_atom(\"t_" ++ 
                TableName ++ "\" ++ integer_to_list(I)))"
                "\n\tend, {0, 0, []}, lists:seq(0, 99)),\n\tok.", -3
            );
        _ ->
            ?FWRITE(Fd, "\n\tSize = ets:info(t_" ++ T #table.name ++ 
                ", size),"
            ),
            
            ?FWRITE(Fd, "\n\n\tets:foldl(fun(Record, {S, N, L}) ->"),
            
            lists:foreach(
                fun(C) ->
                    OCN = C #column.name,
                    CN = format_name(OCN),
                    TS = get_trans_by_column(C),
                    
                    ?FWRITE(Fd, "\n\t\t_" ++ CN ++ " = " ++ TS ++
                        "(Record #" ++ TableName ++ "." ++ OCN ++ "),"
                    )
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\n\t\tLast = if N == 100 orelse S + 1 == Size ->"
                " <<\");\\n\\n\">>; true -> <<\"),\\n\">> end,"
                "\n\n\t\tL2 = [<<\"(\","
            ),
            
            lists:foreach(
                fun(C) ->
                    CN = format_name(C #column.name),
                    ?FWRITE(Fd, "\n\t\t\t_" ++ CN ++ "/binary, \",\",")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\n\t\t\tLast/binary\n\t\t>> | L],"
                "\n\n\t\tif N == 100 orelse S + 1 == Size ->"
                "\n\t\t\tmysql_conn:fetch(Pid, [<<\"INSERT IGNORE INTO `" ++
                TableName ++ "` (\"", -4
            ),
            
            lists:foreach(
                fun(C) ->
                    OCN = C #column.name,
                    ?FWRITE(Fd, "\n\t\t\t\t\"`" ++ OCN ++ "`, \"")
                end,
                T #table.column
            ),
            
            ?FWRITE(Fd, "\"\n\t\t\t\t\") VALUES \">> | lists:reverse(L2)], "
                "self()),\n\n\t\t\t{S + 1, 0, []};\n\t\ttrue ->"
                "\n\t\t\t{S + 1, N + 1, L2}\n\t\tend"
                "\n\tend, {0, 0, []}, t_" ++ TableName ++
                "),\n\tok.", -3
            )
    end,
    
    write_table_dump(L, Fd).
    
get_trans_by_column (C) ->
    case C #column.type of
        "int" -> "int_to_bin";
        "bigint" -> "int_to_bin";
        "tinyint" -> "int_to_bin";
        "mediumint" -> "int_to_bin";
        "float" -> "rel_to_bin";
        "varchar" -> "lst_to_bin";
        "text" -> "lst_to_bin";
        "char" -> "lst_to_bin"
    end.
    
table_auto_increment (T) ->
    case lists:keyfind("auto_increment", #column.extra, T #table.column) of
        C when is_record(C, column) ->
            {true, C};
        _ ->
            false
    end.
    
table_need_split (T) ->
    case re:run(T #table.name, "^player_") of
        {match, _} ->
            case lists:keyfind("player_id", #column.name, T #table.column) of
                C when is_record(C, column) ->
                    {true, C};
                _ ->
                    false
            end;
        _ ->
            false
    end.
    
table_need_load (T) ->
    case re:run(T #table.name, "_log$") of
        {match, _} ->
            false;
        _ ->
            true
    end.
    
table_primary_key (T) ->
    table_primary_key(T #table.column, []).
    
table_primary_key ([], KL) ->
    lists:reverse(KL);
table_primary_key ([C | L], KL) ->
    case C #column.key of
        "PRI" ->
            table_primary_key(L, [C | KL]);
        _ ->
            table_primary_key(L, KL)
    end.