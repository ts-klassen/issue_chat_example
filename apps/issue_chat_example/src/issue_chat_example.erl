-module(issue_chat_example).

-export([
      , on_issue_open_webhook_loop/0
      , run_loop/0
    ]).

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

on({issue_open, Issue0}) ->
    {value, Number0} = ghwhk_issues:number(Issue0),
    Number = integer_to_binary(Number0),
    {value, Title} = ghwhk_issues:title(Issue0),
    {value, Body} = ghwhk_issues:body(Issue0),
    System = <<"Github Issue Number: ", Number/binary, "\nGithub Issue Title: ", Title/binary>>,
    Chat0 = new_chat(),
    Chat1 = chat_gpte:system(System, Chat0),
    {Res, Chat2} = chat_gpte:ask(Body, Chat1),
    Comment0 = ghwhk_comments:new(Issue0),
    Comment1 = ghwhk_comments:body(Res, Comment0),
    ghwhk_comments:create(Comment1),
    ok.


new_chat() ->
    chat_gpte:new().

run() ->
    Event = jobpq:wait_for_assignment(?MODULE),
    on(Event).

run_loop() ->
    run(),
    timer:sleep(60*1000),
    run_loop().
