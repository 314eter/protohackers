-module(voracious_code_storage_server).

-behaviour(gen_server).

-export([start_link/0, get/1, get/2, put/2, list/1]).
-export([init/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Name) ->
    gen_server:call(?MODULE, {get, Name}).

get(Name, Rev) ->
    gen_server:call(?MODULE, {get, Name, Rev}).

put(Name, Data) ->
    gen_server:call(?MODULE, {put, Name, Data}).

list(Name) ->
    gen_server:call(?MODULE, {list, Name}).

init([]) ->
    {ok, gb_trees:empty()}.

handle_call({get, Name}, _, Tree) ->
    {reply, get_file(Name, current, Tree), Tree};
handle_call({get, Name, Rev}, _, Tree) ->
    {reply, get_file(Name, Rev, Tree), Tree};
handle_call({put, Name, Data}, _, Tree) ->
    {Rev, NewTree} = put_file(Name, Data, Tree),
    {reply, Rev, NewTree};
handle_call({list, Name}, _, Tree) ->
    Root = <<Name/binary, $/>>,
    {reply, list_dir(Root, <<>>, gb_trees:iterator_from(Root, Tree)), Tree}.

handle_cast(_, Tree) ->
    {noreply, Tree}.

get_file(Name, RevBin, Tree) ->
    case gb_trees:lookup(Name, Tree) of
        {value, Versions} ->
            case get_revision(RevBin, array:size(Versions)) of
                no_such_revision -> {error, <<"no such revision">>};
                Rev -> {ok, array:get(Rev - 1, Versions)}
            end;
        none ->
            {error, <<"no such file">>}
    end.

get_revision(current, Current) ->
    Current;
get_revision(Bin, Current) when is_binary(Bin) ->
    case re:run(Bin, <<"^r?([1-9][0-9]*)$">>, [{capture, all_but_first, binary}]) of
        {match, [R]} -> get_revision(binary_to_integer(R), Current);
        nomatch -> no_such_revision
    end;
get_revision(Rev, Current) when Rev =< Current ->
    Rev;
get_revision(_, _) ->
    no_such_revision.

put_file(Name, Data, Tree) ->
    case gb_trees:lookup(Name, Tree) of
        {value, Versions} ->
            Current = array:size(Versions),
            case array:get(Current - 1, Versions) of
                Data ->
                    {Current, Tree};
                _ ->
                    NewVersions = array:set(Current, Data, Versions),
                    {Current + 1, gb_trees:enter(Name, NewVersions, Tree)}
            end;
        none ->
            {1, gb_trees:insert(Name, array:from_list([Data]), Tree)}
    end.

list_dir(Root, SubDir, Iter) ->
    RootLength = byte_size(Root),
    SubDirLength = byte_size(SubDir),
    case gb_trees:next(Iter) of
        {<<Root:RootLength/binary, SubDir:SubDirLength/binary, "/", _>>, _, NextIter} ->
            list_dir(Root, SubDir, NextIter);
        {<<Root:RootLength/binary, Name/binary>>, Versions, NextIter} ->
            case binary:split(Name, <<"/">>) of
                [File] ->
                    [{file, File, array:size(Versions)} | list_dir(Root, File, NextIter)];
                [Dir, _] ->
                    [{dir, Dir} | list_dir(Root, Dir, NextIter)]
            end;
        _ ->
            []
    end.
