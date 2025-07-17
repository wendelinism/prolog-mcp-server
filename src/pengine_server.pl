% Pengine Server for Multi-User Prolog Sessions
%
% Commands that work:
% 1. Start pengine server:
%    PORT=9998 swipl -g "start_pengine_server" -t "halt" src/pengine_server.pl
%
% 2. Test basic pengine creation (works):
%    swipl -g "use_module(library(pengines)), pengine_create([id(PengineID), sandbox(true)]), write('Created pengine: '), write(PengineID), nl, halt"
%
% 3. Create session via HTTP (works):
%    curl -X POST -H "Content-Type: application/json" -d '{"action": "create"}' http://localhost:9998/session
%
% 4. Test clause assertion (currently fails):
%    curl -X POST -H "Content-Type: application/json" -d '{"session_id": "SESSION_ID", "clause": "parent(tom, bob)."}' http://localhost:9998/assert
%
% Known issues:
% - pengine_ask() and pengine_send() fail in HTTP handler context
% - Direct pengine interaction from server context doesn't work
% - Need to use built-in pengine HTTP endpoints or find proper server API

:- use_module(library(pengines)).
:- use_module(library(http/http_server)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(uuid)).

% Enable pengines - use default application
% :- pengine_application(prolog_mcp).

% Dynamic clause storage per session
:- dynamic session_clause/2.  % session_clause(SessionID, Clause)

% Session management
:- dynamic session_pengine/2.  % session_pengine(SessionID, PengineID)

% API Endpoints
:- http_handler(root(session), handle_session, [method(post)]).
:- http_handler(root(assert), handle_assert, [method(post)]).
:- http_handler(root(retract), handle_retract, [method(post)]).
:- http_handler(root(list_clauses), handle_list_clauses, [method(get)]).
:- http_handler(root(query), handle_query, [method(get)]).
:- http_handler(root(cleanup), handle_cleanup, [method(post)]).

% Session creation/management
handle_session(Request) :-
    http_read_json_dict(Request, Dict),
    (   Dict.get(action) == "create"
    ->  create_user_session(SessionID, PengineID),
        reply_json(_{status:success, session_id:SessionID, pengine_id:PengineID})
    ;   Dict.get(action) == "destroy"
    ->  SessionID = Dict.get(session_id),
        destroy_user_session(SessionID),
        reply_json(_{status:success, message:"Session destroyed"})
    ;   reply_json(_{status:error, message:"Invalid action"})
    ).

% Create isolated pengine for user session using standard pengine HTTP API
create_user_session(SessionID, PengineID) :-
    uuid(SessionID),
    % Use standard pengine creation - this will create a pengine with proper HTTP handling
    pengine_create([
        id(PengineID),
        sandbox(true)
    ]),
    assertz(session_pengine(SessionID, PengineID)).

% Cleanup user session
destroy_user_session(SessionID) :-
    (   session_pengine(SessionID, PengineID)
    ->  pengine_destroy(PengineID),
        retractall(session_pengine(SessionID, _)),
        retractall(session_clause(SessionID, _))
    ;   true
    ).

% Get pengine ID for session
get_session_pengine(Request, SessionID, PengineID) :-
    (   http_parameters(Request, [session_id(SessionID, [string])])
    ->  true
    ;   memberchk(session_id=SessionID, Request)
    ),
    (   session_pengine(SessionID, PengineID)
    ->  true
    ;   throw(error(session_not_found, SessionID))
    ).

% Add clause to user's pengine  
handle_assert(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            SessionID = Dict.get(session_id),
            ClauseStr = Dict.get(clause),
            session_pengine(SessionID, PengineID),
            term_string(Term, ClauseStr),
            pengine_send(PengineID, ask(assertz(Term), [])),
            assertz(session_clause(SessionID, ClauseStr)),
            reply_json(_{status:success, asserted:ClauseStr})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Remove clause from user's pengine
handle_retract(Request) :-
    catch(
        (   http_read_json_dict(Request, Dict),
            SessionID = Dict.get(session_id),
            ClauseStr = Dict.get(clause),
            session_pengine(SessionID, PengineID),
            term_string(Term, ClauseStr),
            pengine_send(PengineID, ask(retractall(Term), [])),
            retractall(session_clause(SessionID, ClauseStr)),
            reply_json(_{status:success, removed:ClauseStr})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% List clauses for user's pengine
handle_list_clauses(Request) :-
    catch(
        (   get_session_pengine(Request, SessionID, _PengineID),
            findall(Clause, session_clause(SessionID, Clause), Clauses),
            reply_json(Clauses)
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Execute query in user's pengine
handle_query(Request) :-
    catch(
        (   get_session_pengine(Request, _SessionID, PengineID),
            http_parameters(Request, [q(QueryString, [string])]),
            term_string(Term, QueryString),
            pengine_send(PengineID, ask(Term, [])),
            % For now, just return success - proper event handling would be needed for full implementation
            reply_json(_{result:success, message:"Query sent"})
        ),
        Error,
        (   format(string(ErrorString), '~w', [Error]),
            reply_json(_{status:error, message:ErrorString})
        )
    ).

% Session cleanup endpoint
handle_cleanup(Request) :-
    http_read_json_dict(Request, Dict),
    SessionID = Dict.get(session_id),
    destroy_user_session(SessionID),
    reply_json(_{status:success, message:"Session cleaned up"}).

% Collect all solutions from pengine
collect_solutions(PengineID, Solutions) :-
    collect_solutions(PengineID, [], Solutions).

collect_solutions(PengineID, Acc, Solutions) :-
    pengine_event(PengineID, Event),
    (   Event = success(Solution, _More)
    ->  collect_solutions(PengineID, [Solution|Acc], Solutions)
    ;   Event = failure
    ->  reverse(Acc, Solutions)
    ;   Event = error(Error)
    ->  throw(Error)
    ;   reverse(Acc, Solutions)
    ).

% Format solutions for JSON response
format_solutions([], []).
format_solutions([Solution|Rest], [SolutionStr|FormattedRest]) :-
    format(string(SolutionStr), '~w', [Solution]),
    format_solutions(Rest, FormattedRest).

% Start pengine-enabled server
start_pengine_server :-
    (getenv('PORT', PortStr) -> atom_number(PortStr, Port)
    ; current_prolog_flag(argv, [PortArg|_]) -> atom_number(PortArg, Port)
    ; Port = 8080),
    http_server([port(Port)]),
    thread_get_message(_).

% Initialize pengine server
:- initialization(start_pengine_server).