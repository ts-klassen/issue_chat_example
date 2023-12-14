-module(issue_chat_example).

-export([
        on_issue_open_webhook_worker/0
      , on_issue_open_webhook/0
      , on_comment_create_webhook_worker/0
      , run_worker/0
    ]).

on_issue_open_webhook_worker() ->
    {ok, spawn_link(fun on_issue_open_webhook_loop/0)}.

on_issue_open_webhook_loop() ->
    on_issue_open_webhook(),
    on_issue_open_webhook_loop().

on_issue_open_webhook() ->
    Issue0 = ghwhk_issues:new(44333065, <<"ts-klassen">>, <<"supdls">>),
    Issue1 = ghwhk_issues:await(opened, Issue0),
    {value, IssueUserLogin} = ghwhk_issues:login(Issue1),
    case IssueUserLogin of
        <<"ts-klassen">> ->
            jobpq:queue_unassigned(?MODULE, {issue_open, Issue1}, 0);
        _ ->
            do_nothing
    end,
    ok.

on_comment_create_webhook_worker() ->
    {ok, spawn_link(fun on_comment_create_webhook_loop/0)}.

on_comment_create_webhook_loop() ->
    on_comment_create_webhook(),
    on_comment_create_webhook_loop().

on_comment_create_webhook() ->
    Comment0 = ghwhk_comments:new(44333065, <<"ts-klassen">>, <<"supdls">>),
    Comment1 = ghwhk_comments:await(created, Comment0),
    {value, CommentUserLogin} = ghwhk_comments:login(Comment1),
    case CommentUserLogin of
        <<"ts-klassen">> ->
            jobpq:queue_unassigned(?MODULE, {comment_create, Comment1}, 0);
        _ ->
            do_nothing
    end,
    ok.

on({issue_open, Issue0}) ->
    {value, Number0} = ghwhk_issues:number(Issue0),
    Number = integer_to_binary(Number0),
    {value, Title} = ghwhk_issues:title(Issue0),
    {value, Body} = ghwhk_issues:body(Issue0),
    System = <<"Github Issue Number: ", Number/binary, "\nGithub Issue Title: ", Title/binary>>,
    Chat0 = new_chat(Issue0),
    Chat1 = chat_gpte:system(System, Chat0),
    {Res, _Chat2} = chat_gpte:ask(Body, Chat1),
    Comment0 = ghwhk_comments:new(Issue0),
    Comment1 = ghwhk_comments:body(Res, Comment0),
    ghwhk_comments:create(Comment1),
    ok;
on({comment_create, Comment0}) ->
    Issue0 = ghwhk_issues:new(Comment0),
    {value, Number0} = ghwhk_issues:number(Comment0),
    Issue1 = ghwhk_issues:number(Number0, Issue0),
    Issue2 = ghwhk_issues:get(Issue1),
    [Message|Messages] = comments_to_messages(Issue2),
    case Message of
        {user, Body} ->
            Chat0 = new_chat(Issue2),
            Chat1 = chat_gpte:messages(Messages, Chat0),
            {Res, Chat2} = chat_gpte:ask(Body, Chat1),
            Comment1 = ghwhk_comments:new(Issue2),
            Comment2 = ghwhk_comments:body(Res, Comment1),
            ghwhk_comments:create(Comment2),
            ok;
        _ ->
            ok
    end.


new_chat(Issue) ->
    Chat0 = chat_gpte:new(),
    Chat1 = chat_gpte:function(gpte_functions:new(
        get_issue
      , <<"get messages from github issue.">>
      , fun(#{issue_number:=Number}, _State) ->
            Issue0 = ghwhk_issues:new(Issue),
            Issue1 = ghwhk_issues:number(Number, Issue0),
            Issue2 = ghwhk_issues:get(Issue1),
            Messages0 = comments_to_messages(Issue2),
            Messages1 = lists:map(fun({R,C})->#{
                role => R
              , content => C
            }end, Messages0),
            jsone:encode(#{
                messages => Messages1
            })
        end
      , [{
            integer
          , issue_number
          , <<"issue number">>
          , true
        }]
    ), Chat0),
    Chat2 = chat_gpte:function(gpte_functions:new(
        post_issue
      , <<"oepn a new github issue.">>
      , fun(#{title:=Title, body:=Body}, _State) ->
            Issue0 = ghwhk_issues:new(Issue),
            Issue1 = ghwhk_issues:title(Title, Issue0),
            Issue2 = ghwhk_issues:body(Body, Issue1),
            Issue3 = ghwhk_issues:create(Issue2),
            {value, Number} = ghwhk_issues:number(Issue3),
            jsone:encode(#{
                issue_number => Number
            })
        end
      , [
            {
                string
              , title
              , <<"issue title">>
              , true
            }
          , {
                string
              , body
              , <<"issue body">>
              , true
            }
        ]
    ), Chat1),
    Chat3 = chat_gpte:function(gpte_functions:new(
        get_file
      , <<"view file on repository.">>
      , fun(Args=#{file_path:=Path}, _State) ->
            Branch = case klsn_map:lookup([branch], Args) of
                {value, Value} -> Value;
                none -> <<"main">>
            end,
            Url = <<"https://raw.githubusercontent.com/ts-klassen/supdls/", Branch/binary, "/", Path/binary>>,
            Res0 = httpc:request(get, {Url, []}, [], [{body_format, binary}]),
            {ok, {{_,200,_}, _, Res1}} = Res0,
            Res1
        end
      , [
            {
                string
              , branch
              , <<"branch name. on default: `main`.">>
              , false
            }
          , {
                string
              , file_path
              , <<"file path from repository root.">>
              , true
            }
        ]
    ), Chat2),
    Chat3.

-spec comments_to_messages(
        ghwhk_issues:issue()
    ) -> chat_gpte:messages().
comments_to_messages(Issue0) ->
    {value, Number0} = ghwhk_issues:number(Issue0),
    Number = integer_to_binary(Number0),
    {value, IssueTitle} = ghwhk_issues:title(Issue0),
    {value, IssueBody} = ghwhk_issues:body(Issue0),
    SystemBody = <<"Github Issue Number: ", Number/binary, "\nGithub Issue Title: ", IssueTitle/binary>>,
    System = {system, SystemBody},
    Comment0 = ghwhk_comments:new(Issue0),
    Comments = ghwhk_comments:list(Comment0),
    Messages0 = lists:filtermap(fun(Comment)->
        Login = ghwhk_comments:login(Comment),
        {value, Body} = ghwhk_comments:body(Comment),
        case Login of
            {value, <<"ts-klassen">>} ->
                {true, {user, Body}};
            {value, <<"ttsquest-test[bot]">>} ->
                {true, {assistant, Body}};
            _ ->
                false
        end
    end, Comments),
    Messages1 = [{user, IssueBody}|Messages0],
    Messages2 = [System|Messages1],
    lists:reverse(Messages2).

run_worker() ->
    {ok, spawn_link(fun run_loop/0)}.

run() ->
    Event = jobpq:wait_for_assignment(?MODULE),
    on(Event).

run_loop() ->
    run(),
    timer:sleep(60*1000),
    run_loop().
